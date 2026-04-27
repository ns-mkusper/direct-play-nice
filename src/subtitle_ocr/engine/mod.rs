//! OCR engine selection, worker planning, model provisioning, and execution-provider setup.

use anyhow::{anyhow, bail, Context, Result};
use std::collections::HashSet;
use std::ffi::CStr;
use std::path::Path;

use super::language::{detect_system_ocr_language, list_tesseract_languages, resolve_ocr_language};
use super::ocr_pipeline::{discover_candidates, ocr_single_stream, probe_video_dimensions};
use super::text_render::{write_ass, write_srt};
use super::{OcrEngine, OcrFormat, OcrSubtitleTrack, SubMode};

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

/// Converts bitmap subtitle streams into OCR subtitle tracks.
///
/// The function performs stream discovery, language resolution, engine selection,
/// optional parallelization, and final subtitle serialization (`.srt`/`.ass`).
pub fn convert_bitmap_subtitles(
    input_file: &CStr,
    work_dir: &Path,
    sub_mode: SubMode,
    default_language: Option<&str>,
    ocr_engine: OcrEngine,
    ocr_format: OcrFormat,
    ocr_external_command: Option<&str>,
) -> Result<Vec<OcrSubtitleTrack>> {
    if matches!(sub_mode, SubMode::Skip) {
        return Ok(Vec::new());
    }

    let candidates = discover_candidates(input_file, sub_mode)?;
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
            let cues = ocr_single_stream(
                &input_path,
                task.stream_index,
                &task.language,
                work_dir,
                ocr_format,
                video_dimensions,
                resolved_engine,
                &mut *seed_engine,
            )?;
            outputs.push(OcrTaskOutput {
                order: task.order,
                stream_index: task.stream_index,
                language: task.language,
                subtitle_path: task.subtitle_path,
                cues,
            });
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
        };
        run_ocr_tasks_parallel(tasks, worker_plan, params)?
    };

    finalize_ocr_outputs(outputs, ocr_format, video_dimensions)
}

/// Implements behavior for `OcrFormat`.
impl OcrFormat {
    /// File extension used when writing OCR output for this subtitle format.
    fn extension(self) -> &'static str {
        match self {
            OcrFormat::Srt => "srt",
            OcrFormat::Ass => "ass",
        }
    }
}
