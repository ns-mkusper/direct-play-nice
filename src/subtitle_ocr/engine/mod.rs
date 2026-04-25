//! OCR engine selection, worker planning, model provisioning, and execution-provider setup.

use anyhow::{anyhow, bail, Context, Result};
use log::{debug, info, warn};
use ort::session::builder::SessionBuilder;
use paddle_ocr_rs::ocr_lite::OcrLite;
use std::collections::HashSet;
use std::env;
use std::ffi::CStr;
use std::path::{Path, PathBuf};
use std::sync::{
    atomic::{AtomicBool, Ordering},
    OnceLock,
};

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

mod models;
mod providers;
mod workers;
pub(super) use models::*;
pub(super) use providers::*;
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

impl OcrFormat {
    /// File extension used when writing OCR output for this subtitle format.
    fn extension(self) -> &'static str {
        match self {
            OcrFormat::Srt => "srt",
            OcrFormat::Ass => "ass",
        }
    }
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
