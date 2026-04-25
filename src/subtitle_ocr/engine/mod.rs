//! OCR engine selection, worker planning, model provisioning, and execution-provider setup.

use anyhow::{anyhow, bail, Context, Result};
use log::{debug, info, warn};
use ort::session::builder::SessionBuilder;
use paddle_ocr_rs::ocr_lite::OcrLite;
use sha2::{Digest, Sha256};
use std::collections::HashSet;
use std::env;
use std::ffi::CStr;
use std::fs;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::sync::{
    atomic::{AtomicBool, AtomicUsize, Ordering},
    Arc, OnceLock,
};
use std::thread;

use super::language::{
    detect_system_ocr_language, list_tesseract_languages, map_language_tag_to_tesseract,
    resolve_ocr_language,
};
use super::ocr_pipeline::{discover_candidates, ocr_single_stream, probe_video_dimensions};
use super::text_render::{
    bounding_box_from_points, load_image, merge_ocr_lines_with_spacing, normalize_utf8_text,
    run_external_ocr_command, run_tesseract, write_ass, write_srt,
};
use super::{OcrEngine, OcrFormat, OcrSubtitleTrack, SubMode};

mod providers;
pub(super) use providers::*;

pub(super) struct SubtitleCandidate {
    pub(super) stream_index: i32,
    pub(super) language_tag: Option<String>,
}

#[derive(Debug)]
pub(super) struct OcrTask {
    pub(super) order: usize,
    pub(super) stream_index: i32,
    pub(super) language: String,
    pub(super) subtitle_path: PathBuf,
}

#[derive(Debug)]
pub(super) struct OcrTaskOutput {
    pub(super) order: usize,
    pub(super) stream_index: i32,
    pub(super) language: String,
    pub(super) subtitle_path: PathBuf,
    pub(super) cues: Vec<SubtitleCue>,
}

#[derive(Debug, Clone)]
pub(super) struct SubtitleCue {
    pub(super) start_ms: i64,
    pub(super) end_ms: i64,
    pub(super) text: String,
}

#[derive(Debug, Clone)]
pub(super) struct OcrBoundingBox {
    pub(super) left: i32,
    pub(super) top: i32,
    pub(super) right: i32,
    pub(super) bottom: i32,
}

#[derive(Debug, Clone)]
pub(super) struct OcrLine {
    pub(super) text: String,
    pub(super) bbox: Option<OcrBoundingBox>,
    pub(super) score: Option<f32>,
    pub(super) color: Option<(u8, u8, u8)>,
    pub(super) italic: bool,
}

#[derive(Debug, Default)]
pub(super) struct OcrOutput {
    pub(super) lines: Vec<OcrLine>,
}

pub(super) trait SubtitleConverter {
    /// Extracts OCR text lines from a subtitle image for the requested language.
    ///
    /// Implementations should return an empty `OcrOutput` when extraction succeeds but no text is found.
    fn extract_lines(&mut self, image_path: &Path, language: &str) -> Result<OcrOutput>;
}

pub(super) struct TesseractEngine;

pub(super) struct ExternalEngine {
    command: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum PpOcrVariant {
    V3,
    V4,
}

impl PpOcrVariant {
    /// Returns a human-readable label used in logs and diagnostics.
    pub(super) fn label(self) -> &'static str {
        match self {
            PpOcrVariant::V3 => "PP-OCRv3",
            PpOcrVariant::V4 => "PP-OCRv4",
        }
    }

    /// Returns the detector/classifier/recognizer model specs for this PP-OCR variant.
    pub(super) fn model_specs(
        self,
    ) -> (&'static ModelSpec, &'static ModelSpec, &'static ModelSpec) {
        match self {
            PpOcrVariant::V3 => (
                &PPOCR_V3_DET_MODEL,
                &PPOCR_V3_CLS_MODEL,
                &PPOCR_V3_REC_MODEL,
            ),
            PpOcrVariant::V4 => (
                &PPOCR_V4_DET_MODEL,
                &PPOCR_V4_CLS_MODEL,
                &PPOCR_V4_REC_MODEL,
            ),
        }
    }

    /// Returns the default latin recognizer model for the current PP-OCR variant.
    pub(super) fn default_latin_rec_spec(self) -> &'static ModelSpec {
        match self {
            PpOcrVariant::V3 => &PPOCR_V3_LATIN_REC_MODEL,
            PpOcrVariant::V4 => &PPOCR_V4_LATIN_REC_MODEL,
        }
    }

    /// Returns the default Japanese recognizer model for the current PP-OCR variant.
    pub(super) fn default_japanese_rec_spec(self) -> &'static ModelSpec {
        match self {
            PpOcrVariant::V3 => &PPOCR_V3_JAPANESE_REC_MODEL,
            PpOcrVariant::V4 => &PPOCR_V4_JAPANESE_REC_MODEL,
        }
    }

    /// Returns the default Korean recognizer model for the current PP-OCR variant.
    pub(super) fn default_korean_rec_spec(self) -> &'static ModelSpec {
        match self {
            PpOcrVariant::V3 => &PPOCR_V3_KOREAN_REC_MODEL,
            PpOcrVariant::V4 => &PPOCR_V4_KOREAN_REC_MODEL,
        }
    }

    /// Returns the default CJK recognizer model for the current PP-OCR variant.
    pub(super) fn default_cjk_rec_spec(self) -> &'static ModelSpec {
        match self {
            PpOcrVariant::V3 => &PPOCR_V3_CJK_REC_MODEL,
            PpOcrVariant::V4 => &PPOCR_V4_CJK_REC_MODEL,
        }
    }
}

pub(super) struct PpOcrEngine {
    english_ocr: OcrLite,
    latin_ocr: Option<OcrLite>,
    japanese_ocr: Option<OcrLite>,
    korean_ocr: Option<OcrLite>,
    cjk_ocr: Option<OcrLite>,
    variant: PpOcrVariant,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum OcrRecProfile {
    English,
    Latin,
    Japanese,
    Korean,
    Cjk,
}

impl SubtitleConverter for TesseractEngine {
    fn extract_lines(&mut self, image_path: &Path, language: &str) -> Result<OcrOutput> {
        let text = run_tesseract(image_path, language)?;
        if text.is_empty() {
            return Ok(OcrOutput::default());
        }
        Ok(OcrOutput {
            lines: vec![OcrLine {
                text,
                bbox: None,
                score: None,
                color: None,
                italic: false,
            }],
        })
    }
}

impl SubtitleConverter for ExternalEngine {
    fn extract_lines(&mut self, image_path: &Path, language: &str) -> Result<OcrOutput> {
        let text = run_external_ocr_command(image_path, language, &self.command)?;
        if text.is_empty() {
            return Ok(OcrOutput::default());
        }
        Ok(OcrOutput {
            lines: vec![OcrLine {
                text,
                bbox: None,
                score: None,
                color: None,
                italic: false,
            }],
        })
    }
}

impl SubtitleConverter for PpOcrEngine {
    fn extract_lines(&mut self, image_path: &Path, language: &str) -> Result<OcrOutput> {
        let rec_profile = rec_profile_for_language(language);
        let (ocr, rec_label) = match rec_profile {
            OcrRecProfile::Japanese => {
                if let Some(japanese_ocr) = self.japanese_ocr.as_mut() {
                    (japanese_ocr, "japanese")
                } else if let Some(cjk_ocr) = self.cjk_ocr.as_mut() {
                    (cjk_ocr, "cjk")
                } else {
                    (&mut self.english_ocr, "english")
                }
            }
            OcrRecProfile::Korean => {
                if let Some(korean_ocr) = self.korean_ocr.as_mut() {
                    (korean_ocr, "korean")
                } else if let Some(cjk_ocr) = self.cjk_ocr.as_mut() {
                    (cjk_ocr, "cjk")
                } else {
                    (&mut self.english_ocr, "english")
                }
            }
            OcrRecProfile::Cjk => {
                if let Some(cjk_ocr) = self.cjk_ocr.as_mut() {
                    (cjk_ocr, "cjk")
                } else {
                    (&mut self.english_ocr, "english")
                }
            }
            OcrRecProfile::Latin => {
                if let Some(latin_ocr) = self.latin_ocr.as_mut() {
                    (latin_ocr, "latin")
                } else {
                    (&mut self.english_ocr, "english")
                }
            }
            OcrRecProfile::English => (&mut self.english_ocr, "english"),
        };

        let img = load_image(image_path)?;
        let result = ocr
            .detect(&img, 50, 1024, 0.5, 0.3, 1.6, false, false)
            .map_err(|err| {
                anyhow!(
                    "{} failed (rec_profile={}, language={}): {} (debug: {:?})",
                    self.variant.label(),
                    rec_label,
                    language,
                    err,
                    err
                )
            })?;

        let mut lines = Vec::with_capacity(result.text_blocks.len());
        for block in result.text_blocks {
            let text = normalize_utf8_text(&block.text);
            if text.is_empty() {
                continue;
            }
            let bbox = bounding_box_from_points(&block.box_points);
            lines.push(OcrLine {
                text,
                bbox,
                score: Some(block.text_score),
                color: None,
                italic: false,
            });
        }

        let lines = merge_ocr_lines_with_spacing(lines);

        Ok(OcrOutput { lines })
    }
}

/// Maps a language tag to the PP-OCR recognition profile used for model selection.
///
/// Unknown tags intentionally fall back to `English` so OCR can still proceed.
pub(super) fn rec_profile_for_language(language: &str) -> OcrRecProfile {
    let normalized =
        map_language_tag_to_tesseract(language).unwrap_or_else(|| language.to_ascii_lowercase());
    match normalized.as_str() {
        "eng" => OcrRecProfile::English,
        "jpn" | "ja" => OcrRecProfile::Japanese,
        "kor" | "ko" => OcrRecProfile::Korean,
        "chi_sim" | "chi_tra" | "chi" | "zho" | "zh" => OcrRecProfile::Cjk,
        "fra" | "fre" | "spa" | "deu" | "ger" | "ita" | "por" | "nld" | "swe" | "dan" | "nor"
        | "fin" | "ron" | "pol" | "ces" | "slk" | "hun" | "tur" | "cat" | "glg" | "ind" | "vie" => {
            OcrRecProfile::Latin
        }
        _ => OcrRecProfile::English,
    }
}

pub(super) static TESSERACT_LANG_CACHE: OnceLock<Result<HashSet<String>>> = OnceLock::new();
pub(super) static DISABLE_TESS_FALLBACK_LOGGED: AtomicBool = AtomicBool::new(false);
pub(super) static FORCE_TESS_NON_ENGLISH_LOGGED: AtomicBool = AtomicBool::new(false);

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

/// Applies `DPN_OCR_CUDA_DEVICES` onto `CUDA_VISIBLE_DEVICES` before OCR initialization.
///
/// This ensures provider initialization and worker planning observe the same visible GPU set.
pub(super) fn apply_ocr_cuda_visible_devices_override() {
    let Ok(raw) = env::var("DPN_OCR_CUDA_DEVICES") else {
        return;
    };
    let parsed = parse_cuda_device_list(&raw);
    if parsed.is_empty() {
        return;
    }
    let normalized = parsed
        .iter()
        .map(i32::to_string)
        .collect::<Vec<_>>()
        .join(",");
    let current = env::var("CUDA_VISIBLE_DEVICES").ok();
    if current.as_deref() == Some(normalized.as_str()) {
        return;
    }
    if let Some(existing) = current {
        warn!(
            "Overriding CUDA_VISIBLE_DEVICES='{}' with DPN_OCR_CUDA_DEVICES='{}' for OCR initialization.",
            existing, normalized
        );
    } else {
        info!(
            "Setting CUDA_VISIBLE_DEVICES='{}' from DPN_OCR_CUDA_DEVICES for OCR initialization.",
            normalized
        );
    }
    env::set_var("CUDA_VISIBLE_DEVICES", normalized);
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

#[derive(Debug, Clone)]
pub(super) struct OcrWorkerPlan {
    pub(super) worker_count: usize,
    pub(super) device_ids: Vec<i32>,
}

/// Builds an OCR worker plan from runtime capabilities and optional env overrides.
pub(super) fn plan_ocr_workers(resolved_engine: OcrEngine, task_count: usize) -> OcrWorkerPlan {
    let available_parallelism = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);
    let max_jobs_override = ocr_max_jobs_env();
    let jobs_per_gpu = ocr_jobs_per_gpu_env().unwrap_or(1);
    let gpu_available =
        *ORT_ENV_GPU_AVAILABLE.get().unwrap_or(&false) && !force_cpu_execution_providers();
    let device_ids =
        if matches!(resolved_engine, OcrEngine::PpOcrV3 | OcrEngine::PpOcrV4) && gpu_available {
            detect_ocr_cuda_devices()
        } else {
            Vec::new()
        };

    let plan = plan_ocr_workers_with_inputs(
        resolved_engine,
        task_count,
        available_parallelism,
        max_jobs_override,
        jobs_per_gpu,
        gpu_available,
        device_ids,
    );
    if matches!(resolved_engine, OcrEngine::PpOcrV3 | OcrEngine::PpOcrV4) {
        info!(
            "OCR worker plan: engine={:?}, tasks={}, workers={}, gpu_available={}, devices={:?}, jobs_per_gpu={}, max_jobs={:?}",
            resolved_engine,
            task_count,
            plan.worker_count,
            gpu_available,
            plan.device_ids,
            jobs_per_gpu,
            max_jobs_override
        );
    }
    plan
}

/// Pure planning helper used by tests and production code.
///
/// Applies CPU caps, GPU capacity (`devices * jobs_per_gpu`), and fallback rules.
pub(super) fn plan_ocr_workers_with_inputs(
    resolved_engine: OcrEngine,
    task_count: usize,
    available_parallelism: usize,
    max_jobs_override: Option<usize>,
    jobs_per_gpu: usize,
    gpu_available: bool,
    mut device_ids: Vec<i32>,
) -> OcrWorkerPlan {
    if task_count <= 1 {
        return OcrWorkerPlan {
            worker_count: task_count,
            device_ids: Vec::new(),
        };
    }

    let mut workers = max_jobs_override.unwrap_or(available_parallelism);
    workers = workers.max(1).min(task_count);

    if !matches!(resolved_engine, OcrEngine::PpOcrV3 | OcrEngine::PpOcrV4) {
        return OcrWorkerPlan {
            worker_count: workers,
            device_ids: Vec::new(),
        };
    }

    if !gpu_available {
        return OcrWorkerPlan {
            worker_count: workers,
            device_ids: Vec::new(),
        };
    }

    device_ids.sort_unstable();
    device_ids.dedup();
    if device_ids.is_empty() {
        warn!(
            "GPU OCR is available but no CUDA device indexes were resolved; falling back to a single OCR worker. \
             Set DPN_OCR_CUDA_DEVICES=0,1,... explicitly if needed."
        );
        return OcrWorkerPlan {
            worker_count: 1,
            device_ids,
        };
    }

    let jobs_per_gpu = jobs_per_gpu.max(1);
    let capacity = device_ids.len().saturating_mul(jobs_per_gpu).max(1);
    workers = workers.min(capacity);
    OcrWorkerPlan {
        worker_count: workers.max(1),
        device_ids,
    }
}

/// Reads `DPN_OCR_JOBS_PER_GPU` as a positive integer.
pub(super) fn ocr_jobs_per_gpu_env() -> Option<usize> {
    parse_positive_usize_env("DPN_OCR_JOBS_PER_GPU")
}

/// Reads `DPN_OCR_MAX_JOBS` as a positive integer.
pub(super) fn ocr_max_jobs_env() -> Option<usize> {
    parse_positive_usize_env("DPN_OCR_MAX_JOBS")
}

/// Parses a required positive integer env value, warning on invalid input.
pub(super) fn parse_positive_usize_env(key: &str) -> Option<usize> {
    let raw = env::var(key).ok()?;
    match raw.trim().parse::<usize>() {
        Ok(value) if value >= 1 => Some(value),
        _ => {
            warn!("Ignoring invalid {}='{}'; expected integer >= 1.", key, raw);
            None
        }
    }
}

/// Parses a comma-separated CUDA device list, removing duplicates and invalid entries.
pub(super) fn parse_cuda_device_list(value: &str) -> Vec<i32> {
    let mut out = Vec::new();
    for token in value.split(',').map(str::trim).filter(|s| !s.is_empty()) {
        if let Ok(id) = token.parse::<i32>() {
            out.push(id);
        }
    }
    out.sort_unstable();
    out.dedup();
    out
}

/// Resolves CUDA device indexes for OCR from explicit env vars or `nvidia-smi`.
pub(super) fn detect_ocr_cuda_devices() -> Vec<i32> {
    if let Ok(raw) = env::var("DPN_OCR_CUDA_DEVICES") {
        let parsed = parse_cuda_device_list(&raw);
        if parsed.is_empty() {
            warn!(
                "Ignoring DPN_OCR_CUDA_DEVICES='{}'; expected comma-separated GPU indexes.",
                raw
            );
        } else {
            info!("OCR CUDA devices set by DPN_OCR_CUDA_DEVICES={:?}", parsed);
            return parsed;
        }
    }

    if let Ok(raw) = env::var("CUDA_VISIBLE_DEVICES") {
        let parsed = parse_cuda_device_list(&raw);
        if !parsed.is_empty() {
            info!(
                "OCR CUDA devices inferred from CUDA_VISIBLE_DEVICES={:?}",
                parsed
            );
            return parsed;
        }
    }

    if env::var("DPN_OCR_DISABLE_NVIDIA_SMI").ok().as_deref() == Some("1") {
        info!("DPN_OCR_DISABLE_NVIDIA_SMI=1; skipping nvidia-smi CUDA device probe.");
        return Vec::new();
    }

    // Best-effort fallback for dynamic multi-GPU assignment when no env override is present.
    let detected = detect_nvidia_gpu_indexes();
    if detected.is_empty() {
        warn!("nvidia-smi probe did not return any CUDA device indexes for OCR.");
    } else {
        info!("OCR CUDA devices detected via nvidia-smi: {:?}", detected);
    }
    detected
}

pub(super) struct OcrParallelParams {
    input_path: String,
    work_dir: PathBuf,
    ocr_format: OcrFormat,
    video_dimensions: Option<(u32, u32)>,
    resolved_engine: OcrEngine,
    ocr_external_command: Option<String>,
    total_tasks: usize,
}

/// Executes OCR tasks in parallel worker threads and returns results in original stream order.
pub(super) fn run_ocr_tasks_parallel(
    tasks: Vec<OcrTask>,
    worker_plan: OcrWorkerPlan,
    params: OcrParallelParams,
) -> Result<Vec<OcrTaskOutput>> {
    let completed = Arc::new(AtomicUsize::new(0));
    let worker_count = worker_plan.worker_count.max(1);
    align_cuda_visible_devices_with_worker_plan(&worker_plan.device_ids);
    let worker_batches = build_ocr_worker_batches(tasks, worker_count, &worker_plan.device_ids);
    if !worker_plan.device_ids.is_empty() {
        info!(
            "Running OCR with {} workers across CUDA devices {:?}",
            worker_count, worker_plan.device_ids
        );
    } else {
        info!(
            "Running OCR with {} workers (no explicit CUDA device assignment)",
            worker_count
        );
    }

    let mut handles = Vec::with_capacity(worker_count);
    for (worker_idx, batch) in worker_batches.into_iter().enumerate() {
        let input_path = params.input_path.clone();
        let work_dir = params.work_dir.clone();
        let command = params.ocr_external_command.clone();
        let completed = Arc::clone(&completed);
        let assigned_device = batch.assigned_device;
        let worker_tasks = batch.tasks;
        let ocr_format = params.ocr_format;
        let video_dimensions = params.video_dimensions;
        let resolved_engine = params.resolved_engine;
        let total_tasks = params.total_tasks;

        handles.push(thread::spawn(move || -> Result<Vec<OcrTaskOutput>> {
            let _device_guard = set_thread_ocr_cuda_device(assigned_device);
            let mut engine =
                create_ocr_engine(resolved_engine, command.as_deref()).with_context(|| {
                    if let Some(device_id) = assigned_device {
                        format!(
                            "failed to initialize OCR worker {} on CUDA device {}",
                            worker_idx, device_id
                        )
                    } else {
                        format!(
                            "failed to initialize OCR worker {} (no explicit CUDA device)",
                            worker_idx
                        )
                    }
                })?;
            let mut local_outputs = Vec::with_capacity(worker_tasks.len());
            if let Some(device_id) = assigned_device {
                info!(
                    "OCR worker {} processing {} subtitle stream task(s) on CUDA device {}",
                    worker_idx,
                    worker_tasks.len(),
                    device_id
                );
            }
            for task in worker_tasks {
                let cues = ocr_single_stream(
                    &input_path,
                    task.stream_index,
                    &task.language,
                    &work_dir,
                    ocr_format,
                    video_dimensions,
                    resolved_engine,
                    &mut *engine,
                )?;

                local_outputs.push(OcrTaskOutput {
                    order: task.order,
                    stream_index: task.stream_index,
                    language: task.language,
                    subtitle_path: task.subtitle_path,
                    cues,
                });
                let done = completed.fetch_add(1, Ordering::Relaxed) + 1;
                log_ocr_stream_progress(done, total_tasks.max(1));
            }
            Ok(local_outputs)
        }));
    }

    let mut outputs = Vec::new();
    for handle in handles {
        let mut worker_outputs = handle
            .join()
            .map_err(|_| anyhow!("OCR worker thread panicked"))??;
        outputs.append(&mut worker_outputs);
    }
    outputs.sort_by_key(|output| output.order);
    Ok(outputs)
}

/// Ensures `CUDA_VISIBLE_DEVICES` includes all GPUs required by the worker plan.
pub(super) fn align_cuda_visible_devices_with_worker_plan(device_ids: &[i32]) {
    if device_ids.len() <= 1 {
        return;
    }

    let planned = device_ids
        .iter()
        .map(i32::to_string)
        .collect::<Vec<_>>()
        .join(",");
    let current = env::var("CUDA_VISIBLE_DEVICES").ok();
    let current_parsed = current
        .as_deref()
        .map(parse_cuda_device_list)
        .unwrap_or_default();

    let should_override = current_parsed.len() < device_ids.len()
        || !device_ids.iter().all(|id| current_parsed.contains(id));
    if !should_override {
        return;
    }

    if let Some(raw) = current {
        warn!(
            "CUDA_VISIBLE_DEVICES='{}' does not expose all planned OCR devices {:?}; overriding to '{}'.",
            raw, device_ids, planned
        );
    } else {
        info!(
            "CUDA_VISIBLE_DEVICES was unset; exposing planned OCR devices {:?} via CUDA_VISIBLE_DEVICES='{}'.",
            device_ids, planned
        );
    }
    env::set_var("CUDA_VISIBLE_DEVICES", planned);
}

#[derive(Debug)]
pub(super) struct OcrWorkerBatch {
    pub(super) assigned_device: Option<i32>,
    pub(super) tasks: Vec<OcrTask>,
}

/// Shards OCR tasks across workers in round-robin order, assigning optional GPU ids per worker.
pub(super) fn build_ocr_worker_batches(
    tasks: Vec<OcrTask>,
    worker_count: usize,
    device_ids: &[i32],
) -> Vec<OcrWorkerBatch> {
    let worker_count = worker_count.max(1);
    let mut batches = (0..worker_count)
        .map(|idx| OcrWorkerBatch {
            assigned_device: if device_ids.is_empty() {
                None
            } else {
                Some(device_ids[idx % device_ids.len()])
            },
            tasks: Vec::new(),
        })
        .collect::<Vec<_>>();

    for (task_idx, task) in tasks.into_iter().enumerate() {
        let worker_idx = task_idx % worker_count;
        batches[worker_idx].tasks.push(task);
    }

    batches
}

/// Emits per-stream OCR progress as a coarse percentage.
pub(super) fn log_ocr_stream_progress(completed: usize, total: usize) {
    let pct = ((completed as f32 / total as f32) * 100.0)
        .round()
        .clamp(0.0, 100.0) as u32;
    info!(
        "OCR progress: {}/{} subtitle streams complete ({}%)",
        completed, total, pct
    );
}

/// Writes OCR outputs to disk and converts task outputs into mux-ready subtitle tracks.
pub(super) fn finalize_ocr_outputs(
    outputs: Vec<OcrTaskOutput>,
    ocr_format: OcrFormat,
    video_dimensions: Option<(u32, u32)>,
) -> Result<Vec<OcrSubtitleTrack>> {
    let mut tracks = Vec::with_capacity(outputs.len());
    for output in outputs {
        match ocr_format {
            OcrFormat::Srt => write_srt(&output.subtitle_path, &output.cues)?,
            OcrFormat::Ass => write_ass(&output.subtitle_path, &output.cues, video_dimensions)?,
        }

        info!(
            "OCR subtitle stream {} -> '{}' ({} cues, format={:?})",
            output.stream_index,
            output.subtitle_path.display(),
            output.cues.len(),
            ocr_format
        );

        tracks.push(OcrSubtitleTrack {
            language: output.language,
            subtitle_path: output.subtitle_path,
            format: ocr_format,
        });
    }
    Ok(tracks)
}

/// Selects the effective OCR engine and initializes the first converter instance.
pub(super) fn build_ocr_engine(
    ocr_engine: OcrEngine,
    ocr_external_command: Option<&str>,
) -> Result<(OcrEngine, Box<dyn SubtitleConverter>)> {
    match ocr_engine {
        OcrEngine::Tesseract => Ok((
            OcrEngine::Tesseract,
            create_ocr_engine(OcrEngine::Tesseract, ocr_external_command)?,
        )),
        OcrEngine::External => Ok((
            OcrEngine::External,
            create_ocr_engine(OcrEngine::External, ocr_external_command)?,
        )),
        OcrEngine::PpOcrV3 => {
            let variant = PpOcrVariant::V3;
            let gpu_available = init_ort_environment()?;
            if !gpu_available {
                warn!(
                    "{} running without GPU acceleration. \
                     Install the CUDA/DirectML/CoreML runtime or set DPN_OCR_REQUIRE_GPU=1 to fail if GPU is required.",
                    variant.label(),
                );
            }
            Ok((
                OcrEngine::PpOcrV3,
                create_ocr_engine(OcrEngine::PpOcrV3, ocr_external_command)?,
            ))
        }
        OcrEngine::PpOcrV4 => {
            let variant = PpOcrVariant::V4;
            let gpu_available = init_ort_environment()?;
            if !gpu_available {
                warn!(
                    "{} running without GPU acceleration. \
                     Install the CUDA/DirectML/CoreML runtime or set DPN_OCR_REQUIRE_GPU=1 to fail if GPU is required.",
                    variant.label(),
                );
            }
            Ok((
                OcrEngine::PpOcrV4,
                create_ocr_engine(OcrEngine::PpOcrV4, ocr_external_command)?,
            ))
        }
        OcrEngine::Auto => {
            let require_gpu = require_gpu();
            let gpu_available = match init_ort_environment() {
                Ok(available) => available,
                Err(err) => {
                    if require_gpu {
                        return Err(err);
                    }
                    warn!("PP-OCR unavailable; falling back to Tesseract: {}", err);
                    return Ok((OcrEngine::Tesseract, Box::new(TesseractEngine)));
                }
            };

            let selected_engine = auto_engine_preference(gpu_available);
            if matches!(selected_engine, OcrEngine::Tesseract) {
                info!("Auto-selected Tesseract (no GPU execution provider available).");
                return Ok((
                    OcrEngine::Tesseract,
                    create_ocr_engine(OcrEngine::Tesseract, ocr_external_command)?,
                ));
            }
            let variant = match selected_engine {
                OcrEngine::PpOcrV3 => PpOcrVariant::V3,
                OcrEngine::PpOcrV4 => PpOcrVariant::V4,
                _ => PpOcrVariant::V4,
            };

            match create_ocr_engine(selected_engine, ocr_external_command) {
                Ok(engine) => {
                    info!(
                        "Auto-selected {} engine with GPU acceleration",
                        variant.label()
                    );
                    Ok((selected_engine, engine))
                }
                Err(err) => {
                    if require_gpu {
                        return Err(err);
                    }
                    warn!(
                        "{} unavailable; falling back to Tesseract: {}",
                        variant.label(),
                        err
                    );
                    Ok((
                        OcrEngine::Tesseract,
                        create_ocr_engine(OcrEngine::Tesseract, ocr_external_command)?,
                    ))
                }
            }
        }
    }
}

/// Creates a concrete OCR converter for the selected engine.
pub(super) fn create_ocr_engine(
    resolved_engine: OcrEngine,
    ocr_external_command: Option<&str>,
) -> Result<Box<dyn SubtitleConverter>> {
    match resolved_engine {
        OcrEngine::Tesseract => Ok(Box::new(TesseractEngine)),
        OcrEngine::External => {
            let command = ocr_external_command
                .ok_or_else(|| anyhow!("missing OCR external command"))?
                .to_string();
            Ok(Box::new(ExternalEngine { command }))
        }
        OcrEngine::PpOcrV3 => {
            let model_dir = resolve_model_dir()?;
            let engine = init_ppocr_engine(&model_dir, require_gpu(), PpOcrVariant::V3)?;
            Ok(Box::new(engine))
        }
        OcrEngine::PpOcrV4 => {
            let model_dir = resolve_model_dir()?;
            let engine = init_ppocr_engine(&model_dir, require_gpu(), PpOcrVariant::V4)?;
            Ok(Box::new(engine))
        }
        OcrEngine::Auto => unreachable!("Auto engine must be resolved before creation"),
    }
}

pub(super) fn auto_engine_preference(gpu_available: bool) -> OcrEngine {
    auto_engine_preference_with_capability(gpu_available, prefer_ppocr_v3_for_legacy_nvidia())
}

pub(super) fn auto_engine_preference_with_capability(
    gpu_available: bool,
    prefer_v3_on_gpu: bool,
) -> OcrEngine {
    if !gpu_available {
        return OcrEngine::Tesseract;
    }
    if prefer_v3_on_gpu {
        OcrEngine::PpOcrV3
    } else {
        OcrEngine::PpOcrV4
    }
}

pub(super) fn disable_tesseract_quality_fallback() -> bool {
    let disabled = env::var("DPN_OCR_DISABLE_TESS_FALLBACK")
        .ok()
        .map(|v| {
            let x = v.trim().to_ascii_lowercase();
            matches!(x.as_str(), "1" | "true" | "yes" | "on")
        })
        .unwrap_or(false);
    if disabled && !DISABLE_TESS_FALLBACK_LOGGED.swap(true, Ordering::Relaxed) {
        warn!(
            "DPN_OCR_DISABLE_TESS_FALLBACK=1 set; skipping Tesseract quality fallback and using pure PP-OCR output."
        );
    }
    disabled
}

pub(super) fn ppocr_require_gpu_error(variant: PpOcrVariant, err: &anyhow::Error) -> anyhow::Error {
    anyhow!(
        "{} failed to initialize with DPN_OCR_REQUIRE_GPU=1. \
         Verify CUDA/ONNX Runtime GPU libraries are installed. Underlying error: {:#} (debug: {:?})",
        variant.label(),
        err,
        err
    )
}

pub(super) fn init_ppocr_engine(
    model_dir: &Path,
    require_gpu: bool,
    variant: PpOcrVariant,
) -> Result<PpOcrEngine> {
    let skip_cls = skip_ppocr_cls(variant, require_gpu);
    if skip_cls {
        info!(
            "{} classifier model is disabled (DPN_OCR_SKIP_CLS or Maxwell GPU mode).",
            variant.label()
        );
    }
    match PpOcrEngine::new(model_dir, variant, skip_cls) {
        Ok(engine) => Ok(engine),
        Err(err) => {
            if require_gpu {
                return Err(ppocr_require_gpu_error(variant, &err));
            }
            if force_cpu_execution_providers() {
                return Err(err);
            }
            warn!(
                "{} failed to initialize with GPU providers; retrying with CPU-only providers: {:#} (debug: {:?})",
                variant.label(),
                err,
                err
            );
            FORCE_CPU_EP.store(true, Ordering::Relaxed);
            match PpOcrEngine::new(model_dir, variant, skip_cls) {
                Ok(engine) => {
                    info!(
                        "{} initialized with CPU-only execution provider",
                        variant.label()
                    );
                    Ok(engine)
                }
                Err(retry_err) => {
                    warn!(
                        "{} CPU-only initialization failed; falling back: {:#} (debug: {:?})",
                        variant.label(),
                        retry_err,
                        retry_err
                    );
                    Err(err)
                }
            }
        }
    }
}

pub(super) fn init_ort_environment() -> Result<bool> {
    if ORT_ENV_INIT.get().is_some() {
        return Ok(*ORT_ENV_GPU_AVAILABLE.get().unwrap_or(&false));
    }
    let selection = build_execution_providers()?;
    match ort::init().commit() {
        Ok(true) => info!("Initialized ONNX Runtime environment for OCR execution providers"),
        Ok(false) => debug!("ONNX Runtime environment already initialized; skipping reconfigure"),
        Err(err) => {
            warn!("Failed to initialize ONNX Runtime environment: {}", err);
            return Err(anyhow!(
                "Failed to initialize ONNX Runtime environment: {err}"
            ));
        }
    }
    let _ = ORT_ENV_INIT.set(());
    let _ = ORT_ENV_GPU_AVAILABLE.set(selection.gpu_available);
    Ok(selection.gpu_available)
}

thread_local! {
    static OCR_CUDA_DEVICE_ID: std::cell::Cell<Option<i32>> = const { std::cell::Cell::new(None) };
}

pub(super) struct OcrCudaDeviceGuard {
    previous: Option<i32>,
}

impl Drop for OcrCudaDeviceGuard {
    fn drop(&mut self) {
        OCR_CUDA_DEVICE_ID.with(|slot| slot.set(self.previous));
    }
}

pub(super) fn set_thread_ocr_cuda_device(device_id: Option<i32>) -> OcrCudaDeviceGuard {
    let previous = OCR_CUDA_DEVICE_ID.with(|slot| {
        let prev = slot.get();
        slot.set(device_id);
        prev
    });
    OcrCudaDeviceGuard { previous }
}

pub(super) fn thread_ocr_cuda_device() -> Option<i32> {
    OCR_CUDA_DEVICE_ID.with(|slot| slot.get())
}

pub(super) struct ModelSpec {
    pub(super) filename: &'static str,
    pub(super) url: &'static str,
    pub(super) sha256: &'static str,
}

pub(super) const PPOCR_V4_DET_MODEL: ModelSpec = ModelSpec {
    filename: "ch_PP-OCRv4_det_infer.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/v3.7.0/onnx/PP-OCRv4/det/ch_PP-OCRv4_det_infer.onnx",
    sha256: "D2A7720D45A54257208B1E13E36A8479894CB74155A5EFE29462512D42F49DA9",
};
pub(super) const PPOCR_V4_CLS_MODEL: ModelSpec = ModelSpec {
    filename: "ch_ppocr_mobile_v2.0_cls_infer.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/v3.7.0/onnx/PP-OCRv4/cls/ch_ppocr_mobile_v2.0_cls_infer.onnx",
    sha256: "E47ACEDF663230F8863FF1AB0E64DD2D82B838FCEB5957146DAB185A89D6215C",
};
pub(super) const PPOCR_V4_REC_MODEL: ModelSpec = ModelSpec {
    filename: "en_PP-OCRv4_rec_infer.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/v3.7.0/onnx/PP-OCRv4/rec/en_PP-OCRv4_rec_infer.onnx",
    sha256: "E8770C967605983D1570CDF5352041DFB68FA0C21664F49F47B155ABD3E0E318",
};

pub(super) const PPOCR_V3_DET_MODEL: ModelSpec = ModelSpec {
    filename: "ch_PP-OCRv3_det_infer.onnx",
    url: "https://huggingface.co/SWHL/RapidOCR/resolve/main/PP-OCRv3/ch_PP-OCRv3_det_infer.onnx?download=true",
    sha256: "3439588C030FAEA393A54515F51E983D8E155B19A2E8ABA7891934C1CF0DE526",
};
pub(super) const PPOCR_V3_CLS_MODEL: ModelSpec = ModelSpec {
    filename: "ch_ppocr_mobile_v2.0_cls_train.onnx",
    url: "https://huggingface.co/SWHL/RapidOCR/resolve/main/PP-OCRv3/ch_ppocr_mobile_v2.0_cls_train.onnx?download=true",
    sha256: "70581B300B83BABD9E0DD1D7D74C5B006869E8796DA277A70C2E405BF9D77C82",
};
pub(super) const PPOCR_V3_REC_MODEL: ModelSpec = ModelSpec {
    filename: "en_PP-OCRv3_rec_infer.onnx",
    url: "https://huggingface.co/SWHL/RapidOCR/resolve/main/PP-OCRv3/en_PP-OCRv3_rec_infer.onnx?download=true",
    sha256: "EF7ABD8BD3629AE57EA2C28B425C1BD258A871B93FD2FE7C433946ADE9B5D9EA",
};

pub(super) const PPOCR_V4_LATIN_REC_MODEL: ModelSpec = ModelSpec {
    filename: "latin_PP-OCRv3_rec_mobile.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/master/onnx/PP-OCRv4/rec/latin_PP-OCRv3_rec_mobile.onnx",
    sha256: "E9D7A33667E8AAA702862975186ADF2012E3F390CC0F9422865957125F8071CF",
};
pub(super) const PPOCR_V4_JAPANESE_REC_MODEL: ModelSpec = ModelSpec {
    filename: "japan_PP-OCRv4_rec_mobile.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/master/onnx/PP-OCRv4/rec/japan_PP-OCRv4_rec_mobile.onnx",
    sha256: "E1075A67DBA758ECFC7EBC78A10AE61C95AC8FB66A9C86FAB5541E33F085CB7A",
};
pub(super) const PPOCR_V4_KOREAN_REC_MODEL: ModelSpec = ModelSpec {
    filename: "korean_PP-OCRv4_rec_mobile.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/master/onnx/PP-OCRv4/rec/korean_PP-OCRv4_rec_mobile.onnx",
    sha256: "AB151BA9065ECCD98F884CF4D927DB091BE86137276392072EDD4F9D43AD7426",
};
pub(super) const PPOCR_V4_CJK_REC_MODEL: ModelSpec = ModelSpec {
    filename: "chinese_cht_PP-OCRv3_rec_mobile.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/master/onnx/PP-OCRv4/rec/chinese_cht_PP-OCRv3_rec_mobile.onnx",
    sha256: "779656D044CE388045E02EA9244724616194E63928606436CDFC6DC3C9528CC6",
};

pub(super) const PPOCR_V3_LATIN_REC_MODEL: ModelSpec = PPOCR_V4_LATIN_REC_MODEL;
pub(super) const PPOCR_V3_JAPANESE_REC_MODEL: ModelSpec = PPOCR_V4_JAPANESE_REC_MODEL;
pub(super) const PPOCR_V3_KOREAN_REC_MODEL: ModelSpec = PPOCR_V4_KOREAN_REC_MODEL;
pub(super) const PPOCR_V3_CJK_REC_MODEL: ModelSpec = PPOCR_V4_CJK_REC_MODEL;

pub(super) struct PpOcrModels {
    det: PathBuf,
    cls: PathBuf,
    rec: PathBuf,
}

impl PpOcrEngine {
    pub(super) fn new(model_dir: &Path, variant: PpOcrVariant, skip_cls: bool) -> Result<Self> {
        let models = ensure_ppocr_models(model_dir, variant, skip_cls)?;
        let latin_rec = resolve_optional_latin_rec_model(model_dir, variant)?;
        let japanese_rec = resolve_optional_japanese_rec_model(model_dir, variant)?;
        let korean_rec = resolve_optional_korean_rec_model(model_dir, variant)?;
        let cjk_rec = resolve_optional_cjk_rec_model(model_dir, variant)?;
        info!(
            "Initializing {} models (det='{}', cls='{}', rec='{}')",
            variant.label(),
            models.det.display(),
            models.cls.display(),
            models.rec.display()
        );
        let english_ocr = init_ocr_lite(variant, "english", &models.det, &models.cls, &models.rec)?;

        let latin_ocr = if let Some(latin_rec_path) = latin_rec {
            info!(
                "Initializing {} latin rec model at '{}'",
                variant.label(),
                latin_rec_path.display()
            );
            match init_ocr_lite(variant, "latin", &models.det, &models.cls, &latin_rec_path) {
                Ok(ocr) => Some(ocr),
                Err(err) => {
                    warn!(
                        "{} latin rec model failed to initialize; falling back to english rec model only: {:#} (debug: {:?})",
                        variant.label(),
                        err,
                        err
                    );
                    None
                }
            }
        } else {
            info!(
                "{} latin rec model not configured/found; using english rec model for all languages.",
                variant.label()
            );
            None
        };

        let japanese_ocr =
            init_optional_rec_profile(variant, "japanese", &models.det, &models.cls, japanese_rec);
        let korean_ocr =
            init_optional_rec_profile(variant, "korean", &models.det, &models.cls, korean_rec);
        let cjk_ocr = init_optional_rec_profile(variant, "cjk", &models.det, &models.cls, cjk_rec);

        Ok(Self {
            english_ocr,
            latin_ocr,
            japanese_ocr,
            korean_ocr,
            cjk_ocr,
            variant,
        })
    }
}

pub(super) fn init_optional_rec_profile(
    variant: PpOcrVariant,
    profile_label: &'static str,
    det: &Path,
    cls: &Path,
    rec_path: Option<PathBuf>,
) -> Option<OcrLite> {
    let Some(rec_path) = rec_path else {
        info!(
            "{} {} rec model not configured/found; fallback routing will use other rec profiles.",
            variant.label(),
            profile_label
        );
        return None;
    };

    info!(
        "Initializing {} {} rec model at '{}'",
        variant.label(),
        profile_label,
        rec_path.display()
    );
    match init_ocr_lite(variant, profile_label, det, cls, &rec_path) {
        Ok(ocr) => Some(ocr),
        Err(err) => {
            warn!(
                "{} {} rec model failed to initialize; routing will fall back to other rec profiles: {:#} (debug: {:?})",
                variant.label(),
                profile_label,
                err,
                err
            );
            None
        }
    }
}

pub(super) fn init_ocr_lite(
    variant: PpOcrVariant,
    profile_label: &str,
    det: &Path,
    cls: &Path,
    rec: &Path,
) -> Result<OcrLite> {
    let mut ocr = OcrLite::new();
    ocr.init_models_custom(
        det.to_string_lossy().as_ref(),
        cls.to_string_lossy().as_ref(),
        rec.to_string_lossy().as_ref(),
        configure_ort_builder,
    )
    .map_err(|err| {
        anyhow!(
            "failed to initialize {} {} models: {} (debug: {:?})",
            variant.label(),
            profile_label,
            err,
            err
        )
    })?;
    Ok(ocr)
}

pub(super) fn configure_ort_builder(builder: SessionBuilder) -> Result<SessionBuilder, ort::Error> {
    let selection = build_execution_providers().map_err(|err| ort::Error::new(err.to_string()))?;
    let mut builder = builder.with_execution_providers(selection.providers)?;
    builder = builder.with_intra_threads(
        std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(2),
    )?;
    builder.with_inter_threads(
        std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(2),
    )
}

pub(super) fn resolve_model_dir() -> Result<PathBuf> {
    if let Some(dir) = env::var_os("DPN_OCR_MODEL_DIR") {
        let path = PathBuf::from(dir);
        fs::create_dir_all(&path)
            .with_context(|| format!("creating OCR model directory '{}'", path.display()))?;
        return Ok(path);
    }

    if let Ok(exe) = env::current_exe() {
        if let Some(parent) = exe.parent() {
            let candidate = parent.join("models");
            if candidate.is_dir() {
                return Ok(candidate);
            }
        }
    }

    let fallback = if let Some(xdg) = env::var_os("XDG_CONFIG_HOME") {
        PathBuf::from(xdg).join("direct-play-nice").join("models")
    } else if let Some(home) = env::var_os("HOME") {
        PathBuf::from(home)
            .join(".config")
            .join("direct-play-nice")
            .join("models")
    } else {
        env::temp_dir().join("direct-play-nice-models")
    };

    fs::create_dir_all(&fallback)
        .with_context(|| format!("creating OCR model directory '{}'", fallback.display()))?;
    Ok(fallback)
}

pub(super) fn ensure_ppocr_models(
    model_dir: &Path,
    variant: PpOcrVariant,
    skip_cls: bool,
) -> Result<PpOcrModels> {
    let (det_spec, cls_spec, rec_spec) = variant.model_specs();
    if skip_cls {
        debug!(
            "Skipping classifier is requested, but this build uses mandatory classifier initialization; loading cls model."
        );
    }
    let det = ensure_model_file(model_dir, det_spec)?;
    let cls = ensure_model_file(model_dir, cls_spec)?;
    let rec = ensure_model_file(model_dir, rec_spec)?;
    Ok(PpOcrModels { det, cls, rec })
}

pub(super) fn resolve_optional_latin_rec_model(
    model_dir: &Path,
    variant: PpOcrVariant,
) -> Result<Option<PathBuf>> {
    resolve_optional_rec_model_with_candidates(
        "DPN_OCR_REC_LATIN_MODEL",
        model_dir,
        match variant {
            PpOcrVariant::V3 => &[
                "latin_PP-OCRv3_rec_mobile.onnx",
                "latin_PP-OCRv3_rec_infer.onnx",
                "latin_ppocr_mobile_v2.0_rec_infer.onnx",
                "multilingual_PP-OCRv3_rec_infer.onnx",
            ],
            PpOcrVariant::V4 => &[
                "latin_PP-OCRv3_rec_mobile.onnx",
                "latin_PP-OCRv4_rec_infer.onnx",
                "latin_ppocr_mobile_v2.0_rec_infer.onnx",
                "multilingual_PP-OCRv4_rec_infer.onnx",
            ],
        },
        variant.default_latin_rec_spec(),
        "latin",
        variant,
    )
}

pub(super) fn resolve_optional_japanese_rec_model(
    model_dir: &Path,
    variant: PpOcrVariant,
) -> Result<Option<PathBuf>> {
    resolve_optional_rec_model_with_candidates(
        "DPN_OCR_REC_JAPANESE_MODEL",
        model_dir,
        match variant {
            PpOcrVariant::V3 => &[
                "japan_PP-OCRv4_rec_mobile.onnx",
                "japan_PP-OCRv3_rec_infer.onnx",
                "japanese_PP-OCRv3_rec_infer.onnx",
                "ja_PP-OCRv3_rec_infer.onnx",
            ],
            PpOcrVariant::V4 => &[
                "japan_PP-OCRv4_rec_mobile.onnx",
                "japan_PP-OCRv4_rec_infer.onnx",
                "japanese_PP-OCRv4_rec_infer.onnx",
                "ja_PP-OCRv4_rec_infer.onnx",
            ],
        },
        variant.default_japanese_rec_spec(),
        "japanese",
        variant,
    )
}

pub(super) fn resolve_optional_korean_rec_model(
    model_dir: &Path,
    variant: PpOcrVariant,
) -> Result<Option<PathBuf>> {
    resolve_optional_rec_model_with_candidates(
        "DPN_OCR_REC_KOREAN_MODEL",
        model_dir,
        match variant {
            PpOcrVariant::V3 => &[
                "korean_PP-OCRv4_rec_mobile.onnx",
                "korean_PP-OCRv3_rec_infer.onnx",
                "ko_PP-OCRv3_rec_infer.onnx",
            ],
            PpOcrVariant::V4 => &[
                "korean_PP-OCRv4_rec_mobile.onnx",
                "korean_PP-OCRv4_rec_infer.onnx",
                "ko_PP-OCRv4_rec_infer.onnx",
            ],
        },
        variant.default_korean_rec_spec(),
        "korean",
        variant,
    )
}

pub(super) fn resolve_optional_cjk_rec_model(
    model_dir: &Path,
    variant: PpOcrVariant,
) -> Result<Option<PathBuf>> {
    resolve_optional_rec_model_with_candidates(
        "DPN_OCR_REC_CJK_MODEL",
        model_dir,
        match variant {
            PpOcrVariant::V3 => &[
                "chinese_cht_PP-OCRv3_rec_mobile.onnx",
                "cjk_PP-OCRv3_rec_infer.onnx",
                "chinese_PP-OCRv3_rec_infer.onnx",
                "zh_PP-OCRv3_rec_infer.onnx",
            ],
            PpOcrVariant::V4 => &[
                "chinese_cht_PP-OCRv3_rec_mobile.onnx",
                "cjk_PP-OCRv4_rec_infer.onnx",
                "chinese_PP-OCRv4_rec_infer.onnx",
                "zh_PP-OCRv4_rec_infer.onnx",
            ],
        },
        variant.default_cjk_rec_spec(),
        "cjk",
        variant,
    )
}

pub(super) fn resolve_optional_rec_model_with_candidates(
    env_key: &str,
    model_dir: &Path,
    candidates: &[&str],
    default_spec: &ModelSpec,
    profile_label: &str,
    variant: PpOcrVariant,
) -> Result<Option<PathBuf>> {
    if let Some(path) = env::var_os(env_key) {
        let rec_path = PathBuf::from(path);
        if !rec_path.is_file() {
            bail!(
                "{} is set but file does not exist: '{}'",
                env_key,
                rec_path.display()
            );
        }
        return Ok(Some(rec_path));
    }

    for candidate in candidates {
        let path = model_dir.join(candidate);
        if path.is_file() {
            return Ok(Some(path));
        }
    }

    match ensure_model_file(model_dir, default_spec) {
        Ok(path) => {
            info!(
                "{} {} rec model auto-provisioned at '{}'",
                variant.label(),
                profile_label,
                path.display()
            );
            Ok(Some(path))
        }
        Err(err) => {
            warn!(
                "{} {} rec model auto-provisioning failed; continuing with fallback routing: {:#}",
                variant.label(),
                profile_label,
                err
            );
            Ok(None)
        }
    }
}

pub(super) fn ensure_model_file(model_dir: &Path, spec: &ModelSpec) -> Result<PathBuf> {
    let path = model_dir.join(spec.filename);
    if path.exists() {
        if let Ok(hash) = sha256_file(&path) {
            if hash != spec.sha256.to_ascii_lowercase() {
                warn!(
                    "OCR model '{}' hash mismatch (got {}, expected {}). Using existing file anyway.",
                    path.display(),
                    hash,
                    spec.sha256
                );
            }
        }
        return Ok(path);
    }

    info!(
        "Downloading OCR model '{}' from {}",
        spec.filename, spec.url
    );
    download_model_with_values(&path, spec.url, spec.sha256, spec.filename)?;
    Ok(path)
}

#[cfg(test)]
pub(super) fn ensure_model_file_with_values(
    model_dir: &Path,
    filename: &str,
    url: &str,
    sha256: &str,
) -> Result<PathBuf> {
    let path = model_dir.join(filename);
    if path.exists() {
        return Ok(path);
    }
    info!("Downloading OCR model '{}' from {}", filename, url);
    download_model_with_values(&path, url, sha256, filename)?;
    Ok(path)
}

pub(super) fn download_model_with_values(
    path: &Path,
    url: &str,
    sha256: &str,
    filename: &str,
) -> Result<()> {
    let parent = path
        .parent()
        .ok_or_else(|| anyhow!("invalid OCR model path '{}'", path.display()))?;
    fs::create_dir_all(parent)
        .with_context(|| format!("creating OCR model directory '{}'", parent.display()))?;

    let tmp_path = path.with_extension("download");
    let response = ureq::get(url)
        .call()
        .with_context(|| format!("downloading OCR model from {}", url))?;
    let mut reader = response.into_reader();
    let mut file = fs::File::create(&tmp_path)
        .with_context(|| format!("creating '{}'", tmp_path.display()))?;
    let mut hasher = Sha256::new();
    let mut buf = [0u8; 16 * 1024];
    let mut download_result = Ok(());
    loop {
        let read = match reader.read(&mut buf) {
            Ok(read) => read,
            Err(err) => {
                download_result = Err(anyhow!(err).context("reading OCR model download"));
                break;
            }
        };
        if read == 0 {
            break;
        }
        hasher.update(&buf[..read]);
        if let Err(err) = file.write_all(&buf[..read]) {
            download_result = Err(anyhow!(err).context("writing OCR model download"));
            break;
        }
    }
    let _ = file.flush();

    if let Err(err) = download_result {
        let _ = fs::remove_file(&tmp_path);
        return Err(err);
    }

    let actual_hash = to_hex_lower(&hasher.finalize());
    let expected = sha256.to_ascii_lowercase();
    if actual_hash != expected {
        let _ = fs::remove_file(&tmp_path);
        bail!(
            "downloaded OCR model '{}' hash mismatch (got {}, expected {})",
            filename,
            actual_hash,
            expected
        );
    }

    fs::rename(&tmp_path, path).with_context(|| {
        format!(
            "renaming OCR model download '{}' -> '{}'",
            tmp_path.display(),
            path.display()
        )
    })?;
    Ok(())
}

pub(super) fn sha256_file(path: &Path) -> Result<String> {
    let mut file = fs::File::open(path)?;
    let mut hasher = Sha256::new();
    let mut buf = [0u8; 16 * 1024];
    loop {
        let read = file.read(&mut buf)?;
        if read == 0 {
            break;
        }
        hasher.update(&buf[..read]);
    }
    Ok(to_hex_lower(&hasher.finalize()))
}

pub(super) fn to_hex_lower(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len() * 2);
    for byte in bytes {
        use std::fmt::Write as _;
        let _ = write!(&mut out, "{:02x}", byte);
    }
    out
}
