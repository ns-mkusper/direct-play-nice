//! OCR engine selection, worker planning, model provisioning, and execution-provider setup.

use anyhow::{anyhow, bail, Context, Result};
use log::warn;
use std::collections::HashSet;
use std::ffi::CStr;
use std::path::Path;

use super::language::{detect_system_ocr_language, list_tesseract_languages, resolve_ocr_language};
use super::ocr_pipeline::{
    discover_candidates, ocr_single_stream, probe_video_dimensions, OcrStreamRequest,
};
use super::text_render::{write_ass, write_srt};
use super::{OcrEngine, OcrFormat, OcrStreamFilter, OcrSubtitleTrack, SubMode};
use crate::SubtitleFailurePolicy;

mod converters;
mod factory;
mod models;
mod providers;
mod runtime;
mod types;
mod workers;
#[cfg(test)]
pub(in crate::subtitle_ocr) use converters::rec_profile_for_language_with_test_config;
#[allow(unused_imports)]
pub(in crate::subtitle_ocr) use converters::{rec_profile_for_language, OcrRecProfile};
pub(super) use factory::*;
pub(super) use models::*;
pub(super) use providers::*;
pub(super) use runtime::*;
pub(super) use types::*;
pub(super) use workers::*;

/// OCR settings for [`convert_bitmap_subtitles`].
pub struct OcrOptions<'a> {
    pub sub_mode: SubMode,
    pub default_language: Option<&'a str>,
    pub ocr_engine: OcrEngine,
    pub ocr_format: OcrFormat,
    pub ocr_external_command: Option<&'a str>,
    /// Leaves out bitmap streams a text stream already covers or whose
    /// language is not in the OCR language list.
    pub stream_filter: &'a OcrStreamFilter,
    /// What to do when OCR of a stream fails even after the CPU retry.
    pub failure_policy: SubtitleFailurePolicy,
}

/// Converts bitmap subtitle streams into OCR subtitle tracks.
///
/// The function performs stream discovery, language resolution, engine selection,
/// optional parallelization, and final subtitle serialization (`.srt`/`.ass`).
pub fn convert_bitmap_subtitles(
    input_file: &CStr,
    work_dir: &Path,
    options: OcrOptions<'_>,
) -> Result<Vec<OcrSubtitleTrack>> {
    let OcrOptions {
        sub_mode,
        default_language,
        ocr_engine,
        ocr_format,
        ocr_external_command,
        stream_filter,
        failure_policy,
    } = options;
    if matches!(sub_mode, SubMode::Skip) {
        return Ok(Vec::new());
    }

    let candidates = discover_candidates(input_file, sub_mode, stream_filter)?;
    if candidates.is_empty() {
        return Ok(Vec::new());
    }

    if matches!(ocr_engine, OcrEngine::External) && ocr_external_command.is_none() {
        bail!("--ocr-engine=external requires --ocr-external-command");
    }

    let input_path = input_file
        .to_str()
        .map_err(|_| anyhow!("Input path must be valid UTF-8 for OCR side pass"))?
        .to_string();
    apply_ocr_cuda_visible_devices_override();
    let system_language = detect_system_ocr_language();
    let video_dimensions = probe_video_dimensions(input_file);

    let (resolved_engine, mut seed_engine) = build_ocr_engine(ocr_engine, ocr_external_command)?;
    let available_langs = if matches!(resolved_engine, OcrEngine::Tesseract) {
        list_tesseract_languages().context(
            "Failed to query Tesseract language packs. Install `tesseract-ocr` and required traineddata files.",
        )?
    } else {
        HashSet::new()
    };

    let mut tasks = Vec::with_capacity(candidates.len());
    for (order, candidate) in candidates.into_iter().enumerate() {
        let resolved_lang = resolve_ocr_language(
            candidate.language_tag.as_deref(),
            default_language,
            system_language.as_deref(),
            &available_langs,
            resolved_engine,
        );
        let subtitle_path = work_dir.join(format!(
            "stream-{}.{}",
            candidate.stream_index,
            ocr_format.extension()
        ));
        tasks.push(OcrTask {
            order,
            stream_index: candidate.stream_index,
            language: resolved_lang,
            subtitle_path,
        });
    }

    let worker_plan = plan_ocr_workers(resolved_engine, tasks.len());
    let outputs = if worker_plan.worker_count <= 1 {
        let mut outputs = Vec::with_capacity(tasks.len());
        let total_tasks = tasks.len().max(1);
        for (idx, task) in tasks.into_iter().enumerate() {
            let request = OcrStreamRequest {
                input_path: &input_path,
                stream_index: task.stream_index,
                language: &task.language,
                work_dir,
                ocr_format,
                video_dimensions,
                ocr_engine: resolved_engine,
            };
            let cues = ocr_stream_with_recovery(
                &request,
                &mut seed_engine,
                ocr_external_command,
                failure_policy,
            )?;
            if let Some(cues) = cues {
                outputs.push(OcrTaskOutput {
                    order: task.order,
                    stream_index: task.stream_index,
                    language: task.language,
                    subtitle_path: task.subtitle_path,
                    cues,
                });
            }
            log_ocr_stream_progress(idx + 1, total_tasks);
        }
        outputs
    } else {
        let total_tasks = tasks.len();
        drop(seed_engine);
        let params = OcrParallelParams {
            input_path: input_path.clone(),
            work_dir: work_dir.to_path_buf(),
            ocr_format,
            video_dimensions,
            resolved_engine,
            ocr_external_command: ocr_external_command.map(str::to_string),
            total_tasks,
            failure_policy,
        };
        run_ocr_tasks_parallel(tasks, worker_plan, params)?
    };

    finalize_ocr_outputs(outputs, ocr_format, video_dimensions)
}

impl OcrFormat {
    /// File extension used when writing OCR output for this subtitle format.
    fn extension(self) -> &'static str {
        match self {
            OcrFormat::Srt => "srt",
            OcrFormat::Ass => "ass",
        }
    }
}

/// True when an OCR error came from a GPU execution provider at run time:
/// device memory exhaustion or a CUDA/cuDNN/cuBLAS failure. Small GPUs shared
/// with video encode/decode run out of memory mid-stream.
pub(in crate::subtitle_ocr) fn is_gpu_runtime_failure(err: &anyhow::Error) -> bool {
    let text = format!("{err:#}").to_ascii_lowercase();
    [
        "failed to allocate memory",
        "out of memory",
        "bfc_arena",
        "cuda",
        "cudnn",
        "cublas",
    ]
    .iter()
    .any(|marker| text.contains(marker))
}

/// OCRs one stream. A GPU run-time failure switches this process's OCR to the
/// CPU execution provider and retries the stream once. A stream that still
/// fails is dropped (`Ok(None)`) under `SkipStream`, or fails the run under
/// `Fail`.
pub(in crate::subtitle_ocr) fn ocr_stream_with_recovery(
    request: &OcrStreamRequest<'_>,
    engine: &mut Box<dyn SubtitleConverter>,
    ocr_external_command: Option<&str>,
    failure_policy: SubtitleFailurePolicy,
) -> Result<Option<Vec<SubtitleCue>>> {
    let mut err = match ocr_single_stream(request, &mut **engine) {
        Ok(cues) => return Ok(Some(cues)),
        Err(err) => err,
    };
    if is_gpu_runtime_failure(&err) && !force_cpu_execution_providers() && !require_gpu() {
        warn!(
            "OCR of subtitle stream {} failed on the GPU; switching OCR to the CPU execution provider and retrying: {:#}",
            request.stream_index, err
        );
        FORCE_CPU_EP.store(true, std::sync::atomic::Ordering::Relaxed);
        match create_ocr_engine(request.ocr_engine, ocr_external_command) {
            Ok(cpu_engine) => {
                *engine = cpu_engine;
                match ocr_single_stream(request, &mut **engine) {
                    Ok(cues) => return Ok(Some(cues)),
                    Err(retry_err) => err = retry_err,
                }
            }
            Err(init_err) => err = init_err.context("initializing CPU OCR engine for retry"),
        }
    }
    match failure_policy {
        SubtitleFailurePolicy::SkipStream => {
            warn!(
                "Dropping subtitle stream {} after OCR failed (--subtitle-failure-policy=skip-stream): {:#}",
                request.stream_index, err
            );
            Ok(None)
        }
        SubtitleFailurePolicy::Fail => Err(err),
    }
}

#[cfg(test)]
mod gpu_failure_tests {
    use super::is_gpu_runtime_failure;
    use anyhow::anyhow;

    #[test]
    fn device_memory_exhaustion_is_a_gpu_failure() {
        let err = anyhow!(
            "Non-zero status code returned while running Conv node. Status Message: bfc_arena.cc:376 Failed to allocate memory for requested buffer of size 6927872"
        )
        .context("PP-OCRv3 failed (rec_profile=cjk, language=chi_sim)");
        assert!(is_gpu_runtime_failure(&err));
        assert!(is_gpu_runtime_failure(&anyhow!(
            "CUDNN_STATUS_EXECUTION_FAILED"
        )));
    }

    #[test]
    fn decode_and_layout_errors_are_not_gpu_failures() {
        assert!(!is_gpu_runtime_failure(&anyhow!(
            "OCR produced no cues for bitmap subtitle stream 5 (hdmv_pgs_subtitle)"
        )));
    }
}
