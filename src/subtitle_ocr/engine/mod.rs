//! OCR engine selection, worker planning, model provisioning, and execution-provider setup.

use anyhow::{anyhow, bail, Context, Result};
use paddle_ocr_rs::ocr_lite::OcrLite;
use std::collections::HashSet;
use std::ffi::CStr;
use std::path::{Path, PathBuf};
use std::sync::{
    atomic::AtomicBool,
    OnceLock,
};

use super::language::{
    detect_system_ocr_language, list_tesseract_languages, resolve_ocr_language,
};
use super::ocr_pipeline::{discover_candidates, ocr_single_stream, probe_video_dimensions};
use super::text_render::{write_ass, write_srt};
use super::{OcrEngine, OcrFormat, OcrSubtitleTrack, SubMode};

mod converters;
mod models;
mod providers;
mod factory;
mod runtime;
mod workers;
#[allow(unused_imports)]
pub(in crate::subtitle_ocr) use converters::{rec_profile_for_language, OcrRecProfile};
pub(super) use factory::*;
pub(super) use models::*;
pub(super) use providers::*;
pub(super) use runtime::*;
pub(super) use workers::*;

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

impl OcrFormat {
    /// File extension used when writing OCR output for this subtitle format.
    fn extension(self) -> &'static str {
        match self {
            OcrFormat::Srt => "srt",
            OcrFormat::Ass => "ass",
        }
    }
}
