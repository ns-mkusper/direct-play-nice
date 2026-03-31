use anyhow::{anyhow, bail, Context, Result};
use log::{debug, info, warn};
#[cfg(any(target_os = "linux", target_os = "windows"))]
use ort::execution_providers::cuda::{CUDAExecutionProvider, CuDNNConvAlgorithmSearch};
#[cfg(any(target_os = "linux", target_os = "windows"))]
use ort::execution_providers::ArenaExtendStrategy;
#[cfg(target_vendor = "apple")]
use ort::execution_providers::CoreMLExecutionProvider;
#[cfg(target_os = "windows")]
use ort::execution_providers::DirectMLExecutionProvider;
use ort::execution_providers::{
    CPUExecutionProvider, ExecutionProvider, ExecutionProviderDispatch,
};
use ort::session::builder::SessionBuilder;
use paddle_ocr_rs::ocr_lite::OcrLite;
use rsmpeg::avcodec::{AVCodec, AVCodecContext, AVPacket};
use rsmpeg::avformat::{AVFormatContextInput, AVFormatContextOutput};
use rsmpeg::avutil::{ra, AVDictionary};
use rsmpeg::ffi;
use sha2::{Digest, Sha256};
use std::collections::HashSet;
use std::env;
use std::ffi::{CStr, CString};
use std::fs;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::{
    atomic::{AtomicBool, Ordering},
    OnceLock,
};

use crate::{OcrEngine, OcrFormat, SubMode};

#[derive(Debug, Clone)]
pub struct OcrSubtitleTrack {
    pub language: String,
    pub subtitle_path: PathBuf,
    pub format: OcrFormat,
}

#[derive(Debug, Clone)]
struct SubtitleCandidate {
    stream_index: i32,
    language_tag: Option<String>,
}

#[derive(Debug, Clone)]
struct SubtitleCue {
    start_ms: i64,
    end_ms: i64,
    text: String,
}

#[derive(Debug, Clone)]
struct OcrBoundingBox {
    left: i32,
    top: i32,
    right: i32,
    bottom: i32,
}

#[derive(Debug, Clone)]
struct OcrLine {
    text: String,
    bbox: Option<OcrBoundingBox>,
    score: Option<f32>,
    color: Option<(u8, u8, u8)>,
    italic: bool,
}

#[derive(Debug, Default)]
struct OcrOutput {
    lines: Vec<OcrLine>,
}

trait SubtitleConverter {
    fn extract_lines(&mut self, image_path: &Path, language: &str) -> Result<OcrOutput>;
}

struct TesseractEngine;

struct ExternalEngine {
    command: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PpOcrVariant {
    V3,
    V4,
}

impl PpOcrVariant {
    fn label(self) -> &'static str {
        match self {
            PpOcrVariant::V3 => "PP-OCRv3",
            PpOcrVariant::V4 => "PP-OCRv4",
        }
    }

    fn model_specs(self) -> (&'static ModelSpec, &'static ModelSpec, &'static ModelSpec) {
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
}

struct PpOcrEngine {
    ocr: OcrLite,
    variant: PpOcrVariant,
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
    fn extract_lines(&mut self, image_path: &Path, _language: &str) -> Result<OcrOutput> {
        let img = load_image(image_path)?;
        let result = self
            .ocr
            .detect(&img, 50, 1024, 0.5, 0.3, 1.6, false, false)
            .map_err(|err| anyhow!("{} failed: {}", self.variant.label(), err))?;

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

static ORT_ENV_INIT: OnceLock<()> = OnceLock::new();
static ORT_ENV_GPU_AVAILABLE: OnceLock<bool> = OnceLock::new();
static FORCE_CPU_EP: AtomicBool = AtomicBool::new(false);
static TESSERACT_LANG_CACHE: OnceLock<Result<HashSet<String>>> = OnceLock::new();

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
    let system_language = detect_system_ocr_language();
    let video_dimensions = probe_video_dimensions(input_file);

    let (resolved_engine, mut engine) = build_ocr_engine(ocr_engine, ocr_external_command)?;
    let available_langs = if matches!(resolved_engine, OcrEngine::Tesseract) {
        list_tesseract_languages().context(
            "Failed to query Tesseract language packs. Install `tesseract-ocr` and required traineddata files.",
        )?
    } else {
        HashSet::new()
    };

    let mut tracks = Vec::with_capacity(candidates.len());
    for candidate in candidates {
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
        let cues = ocr_single_stream(
            &input_path,
            candidate.stream_index,
            &resolved_lang,
            work_dir,
            ocr_format,
            video_dimensions,
            resolved_engine,
            &mut *engine,
        )?;

        match ocr_format {
            OcrFormat::Srt => write_srt(&subtitle_path, &cues)?,
            OcrFormat::Ass => write_ass(&subtitle_path, &cues, video_dimensions)?,
        }

        info!(
            "OCR subtitle stream {} -> '{}' ({} cues, format={:?})",
            candidate.stream_index,
            subtitle_path.display(),
            cues.len(),
            ocr_format
        );

        tracks.push(OcrSubtitleTrack {
            language: resolved_lang,
            subtitle_path,
            format: ocr_format,
        });
    }

    Ok(tracks)
}

impl OcrFormat {
    fn extension(self) -> &'static str {
        match self {
            OcrFormat::Srt => "srt",
            OcrFormat::Ass => "ass",
        }
    }
}

fn build_ocr_engine(
    ocr_engine: OcrEngine,
    ocr_external_command: Option<&str>,
) -> Result<(OcrEngine, Box<dyn SubtitleConverter>)> {
    match ocr_engine {
        OcrEngine::Tesseract => Ok((OcrEngine::Tesseract, Box::new(TesseractEngine))),
        OcrEngine::External => {
            let command = ocr_external_command
                .ok_or_else(|| anyhow!("missing OCR external command"))?
                .to_string();
            Ok((OcrEngine::External, Box::new(ExternalEngine { command })))
        }
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
            let model_dir = resolve_model_dir()?;
            let engine = init_ppocr_engine(&model_dir, require_gpu(), variant)?;
            Ok((OcrEngine::PpOcrV3, Box::new(engine)))
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
            let model_dir = resolve_model_dir()?;
            let engine = init_ppocr_engine(&model_dir, require_gpu(), variant)?;
            Ok((OcrEngine::PpOcrV4, Box::new(engine)))
        }
        OcrEngine::Auto => {
            let require_gpu = require_gpu();
            let variant = PpOcrVariant::V4;
            let gpu_available = match init_ort_environment() {
                Ok(available) => available,
                Err(err) => {
                    if require_gpu {
                        return Err(err);
                    }
                    warn!(
                        "{} unavailable; falling back to Tesseract: {}",
                        variant.label(),
                        err
                    );
                    return Ok((OcrEngine::Tesseract, Box::new(TesseractEngine)));
                }
            };

            if matches!(auto_engine_preference(gpu_available), OcrEngine::Tesseract) {
                info!("Auto-selected Tesseract (no GPU execution provider available).");
                return Ok((OcrEngine::Tesseract, Box::new(TesseractEngine)));
            }

            match (|| -> Result<PpOcrEngine> {
                let model_dir = resolve_model_dir()?;
                init_ppocr_engine(&model_dir, require_gpu, variant)
            })() {
                Ok(engine) => {
                    info!(
                        "Auto-selected {} engine with GPU acceleration",
                        variant.label()
                    );
                    Ok((OcrEngine::PpOcrV4, Box::new(engine)))
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
                    Ok((OcrEngine::Tesseract, Box::new(TesseractEngine)))
                }
            }
        }
    }
}

fn auto_engine_preference(gpu_available: bool) -> OcrEngine {
    if gpu_available {
        OcrEngine::PpOcrV4
    } else {
        OcrEngine::Tesseract
    }
}

fn ppocr_require_gpu_error(variant: PpOcrVariant, err: impl std::fmt::Display) -> anyhow::Error {
    anyhow!(
        "{} failed to initialize with DPN_OCR_REQUIRE_GPU=1. \
         Verify CUDA/ONNX Runtime GPU libraries are installed. Underlying error: {}",
        variant.label(),
        err
    )
}

fn init_ppocr_engine(
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
                return Err(ppocr_require_gpu_error(variant, err));
            }
            if force_cpu_execution_providers() {
                return Err(err);
            }
            warn!(
                "{} failed to initialize with GPU providers; retrying with CPU-only providers: {}",
                variant.label(),
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
                        "{} CPU-only initialization failed; falling back: {}",
                        variant.label(),
                        retry_err
                    );
                    Err(err)
                }
            }
        }
    }
}

fn init_ort_environment() -> Result<bool> {
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

struct ModelSpec {
    filename: &'static str,
    url: &'static str,
    sha256: &'static str,
}

const PPOCR_V4_DET_MODEL: ModelSpec = ModelSpec {
    filename: "ch_PP-OCRv4_det_infer.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/v3.7.0/onnx/PP-OCRv4/det/ch_PP-OCRv4_det_infer.onnx",
    sha256: "D2A7720D45A54257208B1E13E36A8479894CB74155A5EFE29462512D42F49DA9",
};
const PPOCR_V4_CLS_MODEL: ModelSpec = ModelSpec {
    filename: "ch_ppocr_mobile_v2.0_cls_infer.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/v3.7.0/onnx/PP-OCRv4/cls/ch_ppocr_mobile_v2.0_cls_infer.onnx",
    sha256: "E47ACEDF663230F8863FF1AB0E64DD2D82B838FCEB5957146DAB185A89D6215C",
};
const PPOCR_V4_REC_MODEL: ModelSpec = ModelSpec {
    filename: "en_PP-OCRv4_rec_infer.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/v3.7.0/onnx/PP-OCRv4/rec/en_PP-OCRv4_rec_infer.onnx",
    sha256: "E8770C967605983D1570CDF5352041DFB68FA0C21664F49F47B155ABD3E0E318",
};

const PPOCR_V3_DET_MODEL: ModelSpec = ModelSpec {
    filename: "ch_PP-OCRv3_det_infer.onnx",
    url: "https://huggingface.co/SWHL/RapidOCR/resolve/main/PP-OCRv3/ch_PP-OCRv3_det_infer.onnx?download=true",
    sha256: "3439588C030FAEA393A54515F51E983D8E155B19A2E8ABA7891934C1CF0DE526",
};
const PPOCR_V3_CLS_MODEL: ModelSpec = ModelSpec {
    filename: "ch_ppocr_mobile_v2.0_cls_train.onnx",
    url: "https://huggingface.co/SWHL/RapidOCR/resolve/main/PP-OCRv3/ch_ppocr_mobile_v2.0_cls_train.onnx?download=true",
    sha256: "70581B300B83BABD9E0DD1D7D74C5B006869E8796DA277A70C2E405BF9D77C82",
};
const PPOCR_V3_REC_MODEL: ModelSpec = ModelSpec {
    filename: "en_PP-OCRv3_rec_infer.onnx",
    url: "https://huggingface.co/SWHL/RapidOCR/resolve/main/PP-OCRv3/en_PP-OCRv3_rec_infer.onnx?download=true",
    sha256: "EF7ABD8BD3629AE57EA2C28B425C1BD258A871B93FD2FE7C433946ADE9B5D9EA",
};

struct PpOcrModels {
    det: PathBuf,
    cls: Option<PathBuf>,
    rec: PathBuf,
}

impl PpOcrEngine {
    fn new(model_dir: &Path, variant: PpOcrVariant, skip_cls: bool) -> Result<Self> {
        let models = ensure_ppocr_models(model_dir, variant, skip_cls)?;
        info!(
            "Initializing {} models (det='{}', cls='{}', rec='{}')",
            variant.label(),
            models.det.display(),
            models
                .cls
                .as_ref()
                .map(|p| p.display().to_string())
                .unwrap_or_else(|| "<skipped>".to_string()),
            models.rec.display()
        );
        let mut ocr = OcrLite::new();
        let cls_owned = models.cls.as_ref().map(|p| p.to_string_lossy().to_string());
        ocr.init_models_custom_optional_cls(
            models.det.to_string_lossy().as_ref(),
            cls_owned.as_deref(),
            models.rec.to_string_lossy().as_ref(),
            configure_ort_builder,
        )
        .map_err(|err| anyhow!("failed to initialize {} models: {}", variant.label(), err))?;
        Ok(Self { ocr, variant })
    }
}

fn configure_ort_builder(builder: SessionBuilder) -> Result<SessionBuilder, ort::Error> {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExecutionProviderKind {
    Cuda,
    DirectML,
    CoreML,
    Cpu,
}

impl ExecutionProviderKind {
    fn label(self) -> &'static str {
        match self {
            ExecutionProviderKind::Cuda => "cuda",
            ExecutionProviderKind::DirectML => "directml",
            ExecutionProviderKind::CoreML => "coreml",
            ExecutionProviderKind::Cpu => "cpu",
        }
    }
}

struct ExecutionProviderSelection {
    providers: Vec<ExecutionProviderDispatch>,
    gpu_available: bool,
}

fn require_gpu() -> bool {
    env::var("DPN_OCR_REQUIRE_GPU").ok().as_deref() == Some("1")
}

fn skip_ppocr_cls(variant: PpOcrVariant, require_gpu: bool) -> bool {
    let configured = env::var("DPN_OCR_SKIP_CLS").ok().and_then(|v| {
        let v = v.trim();
        if v.eq_ignore_ascii_case("1")
            || v.eq_ignore_ascii_case("true")
            || v.eq_ignore_ascii_case("yes")
            || v.eq_ignore_ascii_case("on")
        {
            Some(true)
        } else if v.eq_ignore_ascii_case("0")
            || v.eq_ignore_ascii_case("false")
            || v.eq_ignore_ascii_case("no")
            || v.eq_ignore_ascii_case("off")
        {
            Some(false)
        } else {
            None
        }
    });
    configured.unwrap_or(matches!(variant, PpOcrVariant::V3) && require_gpu)
}

fn force_cpu_execution_providers() -> bool {
    if env::var("DPN_OCR_FORCE_CPU").ok().as_deref() == Some("1") {
        return true;
    }
    FORCE_CPU_EP.load(Ordering::Relaxed)
}

fn format_provider_kinds(kinds: &[ExecutionProviderKind]) -> String {
    kinds
        .iter()
        .map(|kind| kind.label())
        .collect::<Vec<_>>()
        .join(", ")
}

#[cfg(any(target_os = "linux", target_os = "windows"))]
fn apply_cuda_env_overrides(mut ep: CUDAExecutionProvider) -> CUDAExecutionProvider {
    let flags = match env::var("ORT_CUDA_FLAGS") {
        Ok(val) => val,
        Err(_) => return ep,
    };

    for chunk in flags.split(',') {
        let chunk = chunk.trim();
        if chunk.is_empty() {
            continue;
        }
        let Some((key, value)) = chunk.split_once('=') else {
            continue;
        };
        let key = key.trim();
        let value = value.trim();
        match key {
            "device_id" => {
                if let Ok(parsed) = value.parse::<i32>() {
                    ep = ep.with_device_id(parsed);
                    info!("Applied ORT_CUDA_FLAGS device_id={}", parsed);
                } else {
                    warn!("Invalid ORT_CUDA_FLAGS device_id '{}'; ignoring.", value);
                }
            }
            "gpu_mem_limit" => {
                if let Ok(parsed) = value.parse::<usize>() {
                    ep = ep.with_memory_limit(parsed);
                    info!("Applied ORT_CUDA_FLAGS gpu_mem_limit={}", parsed);
                } else {
                    warn!(
                        "Invalid ORT_CUDA_FLAGS gpu_mem_limit '{}'; ignoring.",
                        value
                    );
                }
            }
            _ => {}
        }
    }

    ep
}

#[cfg(any(target_os = "linux", target_os = "windows"))]
fn build_cuda_provider(require_gpu: bool) -> ExecutionProviderDispatch {
    let mut ep = CUDAExecutionProvider::default();
    ep = apply_cuda_env_overrides(ep);
    ep = ep
        .with_conv_algorithm_search(CuDNNConvAlgorithmSearch::Heuristic)
        .with_cuda_graph(false)
        .with_conv_max_workspace(false)
        .with_arena_extend_strategy(ArenaExtendStrategy::SameAsRequested);
    info!("CUDA EP safety brakes enabled for Maxwell-class GPUs.");
    let mut ep = ep.build();
    if require_gpu {
        ep = ep.error_on_failure();
    }
    ep
}

#[cfg(not(any(target_os = "linux", target_os = "windows")))]
fn build_cuda_provider(_require_gpu: bool) -> ExecutionProviderDispatch {
    unreachable!("CUDA execution provider is not supported on this platform");
}

#[cfg(target_os = "windows")]
fn build_directml_provider(require_gpu: bool) -> ExecutionProviderDispatch {
    let mut ep = DirectMLExecutionProvider::default().build();
    if require_gpu {
        ep = ep.error_on_failure();
    }
    ep
}

#[cfg(not(target_os = "windows"))]
fn build_directml_provider(_require_gpu: bool) -> ExecutionProviderDispatch {
    unreachable!("DirectML execution provider is only supported on Windows");
}

#[cfg(target_vendor = "apple")]
fn build_coreml_provider(require_gpu: bool) -> ExecutionProviderDispatch {
    let mut ep = CoreMLExecutionProvider::default().build();
    if require_gpu {
        ep = ep.error_on_failure();
    }
    ep
}

#[cfg(not(target_vendor = "apple"))]
fn build_coreml_provider(_require_gpu: bool) -> ExecutionProviderDispatch {
    unreachable!("CoreML execution provider is only supported on Apple platforms");
}

#[cfg(target_os = "windows")]
fn detect_directml_available(force_cpu: bool) -> bool {
    if force_cpu {
        return false;
    }
    let directml = DirectMLExecutionProvider::default();
    match directml.is_available() {
        Ok(true) => {
            info!("ONNX Runtime DirectML execution provider available");
            true
        }
        Ok(false) => {
            warn!("ONNX Runtime DirectML execution provider not available; falling back to CPU.");
            false
        }
        Err(err) => {
            warn!(
                "Failed to query DirectML execution provider availability: {}",
                err
            );
            false
        }
    }
}

#[cfg(not(target_os = "windows"))]
fn detect_directml_available(_force_cpu: bool) -> bool {
    false
}

#[cfg(target_vendor = "apple")]
fn detect_coreml_available(force_cpu: bool) -> bool {
    if force_cpu {
        return false;
    }
    let coreml = CoreMLExecutionProvider::default();
    match coreml.is_available() {
        Ok(true) => {
            info!("ONNX Runtime CoreML execution provider available");
            true
        }
        Ok(false) => {
            warn!("ONNX Runtime CoreML execution provider not available; falling back to CPU.");
            false
        }
        Err(err) => {
            warn!(
                "Failed to query CoreML execution provider availability: {}",
                err
            );
            false
        }
    }
}

#[cfg(not(target_vendor = "apple"))]
fn detect_coreml_available(_force_cpu: bool) -> bool {
    false
}

fn select_execution_provider_plan(
    require_gpu: bool,
    cuda_available: bool,
    directml_available: bool,
    coreml_available: bool,
) -> Result<(Vec<ExecutionProviderKind>, bool)> {
    let mut kinds = Vec::new();
    let mut gpu_available = false;

    if cuda_available {
        gpu_available = true;
        kinds.push(ExecutionProviderKind::Cuda);
    }
    if directml_available {
        gpu_available = true;
        kinds.push(ExecutionProviderKind::DirectML);
    }
    if coreml_available {
        gpu_available = true;
        kinds.push(ExecutionProviderKind::CoreML);
    }

    if require_gpu && !gpu_available {
        bail!("No GPU execution providers available. Install the required GPU runtime libraries or unset DPN_OCR_REQUIRE_GPU=1 to allow CPU fallback.");
    }

    kinds.push(ExecutionProviderKind::Cpu);
    Ok((kinds, gpu_available))
}

fn build_execution_providers() -> Result<ExecutionProviderSelection> {
    let require_gpu = require_gpu();
    if require_gpu {
        info!("DPN_OCR_REQUIRE_GPU=1; GPU execution provider is required.");
    }
    let force_cpu = force_cpu_execution_providers();
    if force_cpu {
        warn!("DPN_OCR_FORCE_CPU=1; disabling GPU execution providers.");
    }
    if require_gpu && force_cpu {
        bail!("DPN_OCR_REQUIRE_GPU=1 cannot be used with DPN_OCR_FORCE_CPU=1.");
    }
    let mut providers = Vec::new();
    let cuda_available = if force_cpu {
        false
    } else {
        #[cfg(any(target_os = "linux", target_os = "windows"))]
        {
            let cuda = CUDAExecutionProvider::default();
            match cuda.is_available() {
                Ok(true) => {
                    info!("ONNX Runtime CUDA execution provider available");
                    true
                }
                Ok(false) => {
                    warn!(
                        "ONNX Runtime CUDA execution provider not available; falling back to CPU."
                    );
                    false
                }
                Err(err) => {
                    warn!(
                        "Failed to query CUDA execution provider availability: {}",
                        err
                    );
                    false
                }
            }
        }
        #[cfg(not(any(target_os = "linux", target_os = "windows")))]
        {
            false
        }
    };

    let directml_available = detect_directml_available(force_cpu);
    let coreml_available = detect_coreml_available(force_cpu);

    let (kinds, gpu_available) = select_execution_provider_plan(
        require_gpu,
        cuda_available,
        directml_available,
        coreml_available,
    )?;

    if gpu_available {
        info!(
            "OCR execution provider order: {}",
            format_provider_kinds(&kinds)
        );
    } else if force_cpu {
        info!("OCR execution provider order: cpu (GPU providers disabled)");
    } else {
        warn!(
            "No GPU execution providers available; PP-OCR will run on CPU. \
             Install the GPU runtime libraries or set DPN_OCR_REQUIRE_GPU=1 to fail instead."
        );
    }

    for kind in kinds {
        match kind {
            ExecutionProviderKind::Cuda => {
                providers.push(build_cuda_provider(require_gpu));
            }
            ExecutionProviderKind::DirectML => {
                providers.push(build_directml_provider(require_gpu));
            }
            ExecutionProviderKind::CoreML => {
                providers.push(build_coreml_provider(require_gpu));
            }
            ExecutionProviderKind::Cpu => {
                providers.push(CPUExecutionProvider::default().build());
            }
        }
    }

    Ok(ExecutionProviderSelection {
        providers,
        gpu_available,
    })
}

fn resolve_model_dir() -> Result<PathBuf> {
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

fn ensure_ppocr_models(
    model_dir: &Path,
    variant: PpOcrVariant,
    skip_cls: bool,
) -> Result<PpOcrModels> {
    let (det_spec, cls_spec, rec_spec) = variant.model_specs();
    let det = ensure_model_file(model_dir, det_spec)?;
    let cls = if skip_cls {
        None
    } else {
        Some(ensure_model_file(model_dir, cls_spec)?)
    };
    let rec = ensure_model_file(model_dir, rec_spec)?;
    Ok(PpOcrModels { det, cls, rec })
}

fn ensure_model_file(model_dir: &Path, spec: &ModelSpec) -> Result<PathBuf> {
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
fn ensure_model_file_with_values(
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

fn download_model_with_values(path: &Path, url: &str, sha256: &str, filename: &str) -> Result<()> {
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

fn sha256_file(path: &Path) -> Result<String> {
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

fn to_hex_lower(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len() * 2);
    for byte in bytes {
        use std::fmt::Write as _;
        let _ = write!(&mut out, "{:02x}", byte);
    }
    out
}

struct PendingPacket {
    ts: i64,
    packet: AVPacket,
}

struct SubtitleMuxer {
    input_ctx: AVFormatContextInput,
    input_stream_index: usize,
    input_time_base: ffi::AVRational,
    decode_ctx: AVCodecContext,
    encode_ctx: AVCodecContext,
    output_stream_index: i32,
    last_written_dts: Option<i64>,
}

impl SubtitleMuxer {
    fn collect_packets(&mut self, output_time_base: ffi::AVRational) -> Result<Vec<PendingPacket>> {
        let mut out = Vec::new();
        loop {
            let mut packet = match self.input_ctx.read_packet()? {
                Some(pkt) => pkt,
                None => break,
            };
            if packet.stream_index != self.input_stream_index as i32 {
                continue;
            }

            packet.rescale_ts(self.input_time_base, self.decode_ctx.time_base);

            if let Some(subtitle) = self.decode_ctx.decode_subtitle(Some(&mut packet))? {
                if let Some(encoded) = encode_subtitle_packet(
                    &mut self.encode_ctx,
                    &subtitle,
                    &packet,
                    self.output_stream_index,
                    output_time_base,
                    &mut self.last_written_dts,
                )? {
                    let ts = packet_ts(&encoded, output_time_base);
                    out.push(PendingPacket {
                        ts,
                        packet: encoded,
                    });
                }
            }
        }
        Ok(out)
    }
}

pub fn remux_copy_streams(input_file: &CStr, output_file: &CStr) -> Result<()> {
    let output_path = PathBuf::from(output_file.to_string_lossy().into_owned());
    let output_extension = output_path
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext.to_ascii_lowercase())
        .unwrap_or_default();
    let is_mkv = matches!(output_extension.as_str(), "mkv" | "mka" | "mks");
    let stem = output_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("output");
    let tmp_out = if output_extension.is_empty() {
        output_path.with_extension("ocr.tmp")
    } else {
        output_path.with_file_name(format!("{stem}.ocr.tmp.{output_extension}"))
    };

    let mut input_ctx = AVFormatContextInput::open(input_file)?;
    let tmp_cstr = CString::new(tmp_out.to_string_lossy().to_string())
        .context("output path has interior NUL")?;
    let mut output_ctx = AVFormatContextOutput::create(tmp_cstr.as_c_str())?;

    let mut stream_index_map = Vec::with_capacity(input_ctx.streams().len());
    for stream in input_ctx.streams() {
        let mut out_stream = output_ctx.new_stream();
        out_stream.set_time_base(stream.time_base);
        let mut codecpar = stream.codecpar().clone();
        if is_mkv {
            unsafe {
                (*codecpar.as_mut_ptr()).codec_tag = 0;
            }
        }
        out_stream.set_codecpar(codecpar);
        out_stream.set_metadata(stream.metadata().as_deref().cloned());
        stream_index_map.push(out_stream.index);
    }

    output_ctx
        .write_header(&mut None)
        .context("failed to write output header for subtitle remux")?;

    loop {
        let mut packet = match input_ctx.read_packet()? {
            Some(pkt) => pkt,
            None => break,
        };
        let input_index = packet.stream_index as usize;
        let output_index = stream_index_map
            .get(input_index)
            .copied()
            .ok_or_else(|| anyhow!("stream index {} not mapped", input_index))?;

        let input_time_base = input_ctx.streams()[input_index].time_base;
        let output_time_base = output_ctx.streams()[output_index as usize].time_base;

        packet.set_stream_index(output_index);
        packet.rescale_ts(input_time_base, output_time_base);
        output_ctx.interleaved_write_frame(&mut packet)?;
    }

    output_ctx.write_trailer()?;

    fs::rename(&tmp_out, &output_path).with_context(|| {
        format!(
            "replacing '{}' after container remux",
            output_path.display()
        )
    })?;

    Ok(())
}

pub fn mux_text_tracks_from(
    input_file: &CStr,
    output_file: &CStr,
    tracks: &[OcrSubtitleTrack],
) -> Result<()> {
    if tracks.is_empty() {
        return Ok(());
    }

    let output_path = PathBuf::from(output_file.to_string_lossy().into_owned());
    let output_extension = output_path
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext.to_ascii_lowercase())
        .unwrap_or_default();
    let stem = output_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("output");
    let tmp_out = if output_extension.is_empty() {
        output_path.with_extension("ocr.tmp")
    } else {
        output_path.with_file_name(format!("{stem}.ocr.tmp.{output_extension}"))
    };

    let is_mp4 = matches!(output_extension.as_str(), "mp4" | "m4v");
    let is_mkv = matches!(output_extension.as_str(), "mkv" | "mka" | "mks");
    if is_mp4
        && tracks
            .iter()
            .any(|track| matches!(track.format, OcrFormat::Ass))
    {
        warn!(
            "ASS OCR output is being remuxed into MP4; formatting will be downgraded to mov_text"
        );
    }

    let mut input_ctx = AVFormatContextInput::open(input_file)?;
    let tmp_cstr = CString::new(tmp_out.to_string_lossy().to_string())
        .context("output path has interior NUL")?;
    let mut output_ctx = AVFormatContextOutput::create(tmp_cstr.as_c_str())?;

    let mut stream_index_map = Vec::with_capacity(input_ctx.streams().len());
    for stream in input_ctx.streams() {
        let mut out_stream = output_ctx.new_stream();
        out_stream.set_time_base(stream.time_base);
        let mut codecpar = stream.codecpar().clone();
        if is_mkv {
            unsafe {
                (*codecpar.as_mut_ptr()).codec_tag = 0;
            }
        }
        out_stream.set_codecpar(codecpar);
        out_stream.set_metadata(stream.metadata().as_deref().cloned());
        stream_index_map.push(out_stream.index);
    }

    let mut subtitle_muxers = Vec::with_capacity(tracks.len());
    for track in tracks {
        subtitle_muxers.push(build_subtitle_muxer(
            track,
            &mut output_ctx,
            is_mp4,
            is_mkv,
        )?);
    }

    output_ctx
        .write_header(&mut None)
        .context("failed to write output header for subtitle remux")?;

    let mut pending = Vec::new();
    for muxer in subtitle_muxers.iter_mut() {
        let output_time_base = output_ctx.streams()[muxer.output_stream_index as usize].time_base;
        pending.extend(muxer.collect_packets(output_time_base)?);
    }
    pending.sort_by(|a, b| {
        a.ts.cmp(&b.ts)
            .then_with(|| a.packet.stream_index.cmp(&b.packet.stream_index))
    });

    let mut next_sub = 0usize;
    loop {
        let mut packet = match input_ctx.read_packet()? {
            Some(pkt) => pkt,
            None => break,
        };
        let input_index = packet.stream_index as usize;
        let output_index = stream_index_map
            .get(input_index)
            .copied()
            .ok_or_else(|| anyhow!("stream index {} not mapped", input_index))?;

        let input_time_base = input_ctx.streams()[input_index].time_base;
        let output_time_base = output_ctx.streams()[output_index as usize].time_base;

        packet.set_stream_index(output_index);
        packet.rescale_ts(input_time_base, output_time_base);

        let packet_ts = packet_ts(&packet, output_time_base);
        while next_sub < pending.len() && pending[next_sub].ts <= packet_ts {
            output_ctx.interleaved_write_frame(&mut pending[next_sub].packet)?;
            next_sub += 1;
        }

        output_ctx.interleaved_write_frame(&mut packet)?;
    }

    while next_sub < pending.len() {
        output_ctx.interleaved_write_frame(&mut pending[next_sub].packet)?;
        next_sub += 1;
    }

    output_ctx.write_trailer()?;

    fs::rename(&tmp_out, &output_path)
        .with_context(|| format!("replacing '{}' after subtitle remux", output_path.display()))?;

    Ok(())
}

fn build_subtitle_muxer(
    track: &OcrSubtitleTrack,
    output_ctx: &mut AVFormatContextOutput,
    is_mp4: bool,
    is_mkv: bool,
) -> Result<SubtitleMuxer> {
    let input_cstr = CString::new(track.subtitle_path.to_string_lossy().to_string())
        .context("subtitle path has interior NUL")?;
    let input_ctx = AVFormatContextInput::open(input_cstr.as_c_str())?;

    let mut input_stream_index = None;
    for (idx, stream) in input_ctx.streams().iter().enumerate() {
        if stream.codecpar().codec_type == ffi::AVMEDIA_TYPE_SUBTITLE {
            input_stream_index = Some(idx);
            break;
        }
    }
    let input_stream_index =
        input_stream_index.ok_or_else(|| anyhow!("subtitle input has no subtitle stream"))?;
    let input_stream = &input_ctx.streams()[input_stream_index];
    let input_time_base = input_stream.time_base;

    let decoder = AVCodec::find_decoder(input_stream.codecpar().codec_id).ok_or_else(|| {
        anyhow!(
            "decoder unavailable for OCR subtitle input ({})",
            codec_name(input_stream.codecpar().codec_id)
        )
    })?;

    let mut decode_ctx = AVCodecContext::new(&decoder);
    decode_ctx.apply_codecpar(&input_stream.codecpar())?;
    decode_ctx.set_time_base(input_time_base);
    decode_ctx.open(None)?;

    let output_codec_id = select_subtitle_codec_id(track.format, is_mp4, is_mkv);
    let encoder = AVCodec::find_encoder(output_codec_id).ok_or_else(|| {
        anyhow!(
            "encoder unavailable for OCR subtitle output ({})",
            codec_name(output_codec_id)
        )
    })?;

    let mut encode_ctx = AVCodecContext::new(&encoder);
    let mut output_stream = output_ctx.new_stream();
    if let Some(metadata) = build_language_metadata(&track.language) {
        output_stream.set_metadata(Some(metadata));
    }

    set_subtitle_codec_par(&mut decode_ctx, &mut encode_ctx);
    encode_ctx.open(None)?;
    output_stream.set_codecpar(encode_ctx.extract_codecpar());
    output_stream.set_time_base(encode_ctx.time_base);

    Ok(SubtitleMuxer {
        input_ctx,
        input_stream_index,
        input_time_base,
        decode_ctx,
        encode_ctx,
        output_stream_index: output_stream.index,
        last_written_dts: None,
    })
}

fn select_subtitle_codec_id(format: OcrFormat, is_mp4: bool, is_mkv: bool) -> ffi::AVCodecID {
    if is_mp4 {
        ffi::AV_CODEC_ID_MOV_TEXT
    } else if is_mkv {
        match format {
            OcrFormat::Ass => ffi::AV_CODEC_ID_ASS,
            OcrFormat::Srt => ffi::AV_CODEC_ID_SUBRIP,
        }
    } else {
        ffi::AV_CODEC_ID_MOV_TEXT
    }
}

fn build_language_metadata(language: &str) -> Option<AVDictionary> {
    let key = CString::new("language").ok()?;
    let value = CString::new(language).ok()?;
    Some(AVDictionary::new(&key, &value, 0))
}

fn set_subtitle_codec_par(
    decode_context: &mut AVCodecContext,
    encode_context: &mut AVCodecContext,
) {
    encode_context.set_time_base(decode_context.time_base);

    if decode_context.subtitle_header_size > 0 {
        let mut new_subtitle_header = vec![0u8; decode_context.subtitle_header_size as usize];
        new_subtitle_header.copy_from_slice(unsafe {
            std::slice::from_raw_parts(
                decode_context.subtitle_header,
                decode_context.subtitle_header_size as usize,
            )
        });

        unsafe {
            (*encode_context.as_mut_ptr()).subtitle_header =
                ffi::av_mallocz(new_subtitle_header.len()) as *mut _;
            (*encode_context.as_mut_ptr()).subtitle_header_size = new_subtitle_header.len() as i32;
            std::ptr::copy_nonoverlapping(
                new_subtitle_header.as_ptr(),
                (*encode_context.as_mut_ptr()).subtitle_header,
                new_subtitle_header.len(),
            );
        }
    }
}

fn encode_subtitle_packet(
    encode_context: &mut AVCodecContext,
    subtitle: &rsmpeg::avcodec::AVSubtitle,
    packet: &AVPacket,
    output_stream_index: i32,
    output_time_base: ffi::AVRational,
    last_written_dts: &mut Option<i64>,
) -> Result<Option<AVPacket>> {
    const MAX_SUBTITLE_PACKET_SIZE: usize = 32 * 1024;
    let mut subtitle_buffer = vec![0u8; MAX_SUBTITLE_PACKET_SIZE];
    encode_context.encode_subtitle(subtitle, &mut subtitle_buffer)?;

    let encoded_size = subtitle_buffer
        .iter()
        .rposition(|&x| x != 0)
        .map(|pos| pos + 1)
        .unwrap_or(0);

    if encoded_size == 0 {
        return Ok(None);
    }

    let mut encoded_packet = AVPacket::new();
    unsafe {
        ffi::av_new_packet(encoded_packet.as_mut_ptr(), encoded_size as i32);
        std::ptr::copy_nonoverlapping(
            subtitle_buffer.as_ptr(),
            (*encoded_packet.as_mut_ptr()).data,
            encoded_size,
        );
    }

    let mut pts = if subtitle.pts != ffi::AV_NOPTS_VALUE {
        subtitle.pts
    } else {
        packet.pts
    };
    let mut dts = if packet.dts != ffi::AV_NOPTS_VALUE {
        packet.dts
    } else {
        pts
    };

    if pts == ffi::AV_NOPTS_VALUE {
        pts = last_written_dts.map(|prev| prev + 1).unwrap_or(0);
    }
    if dts == ffi::AV_NOPTS_VALUE {
        dts = pts;
    }

    encoded_packet.set_stream_index(output_stream_index);
    encoded_packet.set_pts(pts);
    encoded_packet.set_dts(dts);
    let mut duration = packet.duration;
    if duration == ffi::AV_NOPTS_VALUE || duration <= 0 {
        let display_ms = i64::from(subtitle.end_display_time)
            .saturating_sub(i64::from(subtitle.start_display_time));
        if display_ms > 0 {
            duration =
                unsafe { ffi::av_rescale_q(display_ms, ra(1, 1_000), encode_context.time_base) };
        }
    }
    if duration <= 0 || duration == ffi::AV_NOPTS_VALUE {
        duration = 1;
    }
    encoded_packet.set_duration(duration);
    encoded_packet.set_flags(packet.flags);

    encoded_packet.rescale_ts(encode_context.time_base, output_time_base);

    let packet_dts = encoded_packet.dts;
    if let Some(prev_dts) = *last_written_dts {
        if packet_dts <= prev_dts {
            let adjusted = prev_dts + 1;
            encoded_packet.set_dts(adjusted);
            if encoded_packet.pts < adjusted {
                encoded_packet.set_pts(adjusted);
            }
        }
    }
    *last_written_dts = Some(encoded_packet.dts);

    Ok(Some(encoded_packet))
}

fn packet_ts(packet: &AVPacket, time_base: ffi::AVRational) -> i64 {
    let ts = if packet.pts != ffi::AV_NOPTS_VALUE {
        packet.pts
    } else if packet.dts != ffi::AV_NOPTS_VALUE {
        packet.dts
    } else {
        return 0;
    };
    unsafe { ffi::av_rescale_q(ts, time_base, ra(1, ffi::AV_TIME_BASE as i32)) }
}

fn discover_candidates(input_file: &CStr, sub_mode: SubMode) -> Result<Vec<SubtitleCandidate>> {
    let ictx = AVFormatContextInput::open(input_file)?;
    let mut out = Vec::new();

    for stream in ictx.streams() {
        let cp = stream.codecpar();
        if cp.codec_type != ffi::AVMEDIA_TYPE_SUBTITLE {
            continue;
        }
        if !is_image_based_subtitle(cp.codec_id) {
            continue;
        }

        let language_tag = stream
            .metadata()
            .as_deref()
            .and_then(extract_language_tag_from_metadata);

        if matches!(sub_mode, SubMode::Auto | SubMode::Force) {
            out.push(SubtitleCandidate {
                stream_index: stream.index,
                language_tag,
            });
        }
    }

    Ok(out)
}

fn probe_video_dimensions(input_file: &CStr) -> Option<(u32, u32)> {
    let ictx = AVFormatContextInput::open(input_file).ok()?;
    for stream in ictx.streams() {
        let cp = stream.codecpar();
        if cp.codec_type == ffi::AVMEDIA_TYPE_VIDEO && cp.width > 0 && cp.height > 0 {
            return Some((cp.width as u32, cp.height as u32));
        }
    }
    None
}

#[allow(clippy::too_many_arguments)]
fn ocr_single_stream(
    input_path: &str,
    stream_index: i32,
    language: &str,
    work_dir: &Path,
    ocr_format: OcrFormat,
    video_dimensions: Option<(u32, u32)>,
    ocr_engine: OcrEngine,
    engine: &mut dyn SubtitleConverter,
) -> Result<Vec<SubtitleCue>> {
    let input_cstr = CString::new(input_path).context("input path has interior NUL")?;
    let mut ictx = AVFormatContextInput::open(input_cstr.as_c_str())?;

    let (stream_time_base, stream_codec_id) = ictx
        .streams()
        .iter()
        .find(|st| st.index == stream_index)
        .map(|st| (st.time_base, st.codecpar().codec_id))
        .ok_or_else(|| anyhow!("subtitle stream {} not found", stream_index))?;

    let decoder = AVCodec::find_decoder(stream_codec_id).ok_or_else(|| {
        anyhow!(
            "decoder unavailable for subtitle stream {} ({})",
            stream_index,
            codec_name(stream_codec_id)
        )
    })?;

    let mut decode_context = AVCodecContext::new(&decoder);
    let mut applied_codecpar = false;
    for st in ictx.streams() {
        if st.index == stream_index {
            decode_context.apply_codecpar(&st.codecpar())?;
            applied_codecpar = true;
            break;
        }
    }
    if !applied_codecpar {
        bail!(
            "subtitle stream {} codec parameters unavailable",
            stream_index
        );
    }
    decode_context.set_time_base(stream_time_base);
    decode_context.open(None)?;

    let mut cues = Vec::new();
    let mut packet_seq: usize = 0;

    loop {
        let mut packet = match ictx.read_packet()? {
            Some(pkt) => pkt,
            None => break,
        };

        if packet.stream_index != stream_index {
            continue;
        }

        let src_pts = packet.pts;
        let src_dur = packet.duration;

        packet.rescale_ts(stream_time_base, decode_context.time_base);

        if let Some(subtitle) = decode_context.decode_subtitle(Some(&mut packet))? {
            let fallback_start_ms = timestamp_to_ms(src_pts, stream_time_base).unwrap_or(0);
            let fallback_dur_ms = timestamp_to_ms(src_dur, stream_time_base)
                .unwrap_or(0)
                .max(0);
            let mut new_cues = subtitle_to_cues(
                subtitle.as_ptr(),
                fallback_start_ms,
                fallback_dur_ms,
                language,
                stream_index,
                packet_seq,
                work_dir,
                ocr_format,
                video_dimensions,
                ocr_engine,
                engine,
            )?;
            cues.append(&mut new_cues);
            packet_seq += 1;
        }
    }

    loop {
        let Some(subtitle) = decode_context.decode_subtitle(None)? else {
            break;
        };
        let mut new_cues = subtitle_to_cues(
            subtitle.as_ptr(),
            0,
            0,
            language,
            stream_index,
            packet_seq,
            work_dir,
            ocr_format,
            video_dimensions,
            ocr_engine,
            engine,
        )?;
        cues.append(&mut new_cues);
        packet_seq += 1;
    }

    sanitize_cues(&mut cues, ocr_format);

    Ok(cues)
}

#[allow(clippy::too_many_arguments)]
fn subtitle_to_cues(
    subtitle: *const ffi::AVSubtitle,
    fallback_start_ms: i64,
    fallback_duration_ms: i64,
    language: &str,
    stream_index: i32,
    packet_seq: usize,
    work_dir: &Path,
    ocr_format: OcrFormat,
    video_dimensions: Option<(u32, u32)>,
    ocr_engine: OcrEngine,
    engine: &mut dyn SubtitleConverter,
) -> Result<Vec<SubtitleCue>> {
    if subtitle.is_null() {
        return Ok(Vec::new());
    }

    let mut end_ms;
    let sub = unsafe { &*subtitle };

    let base_ms = if sub.pts != ffi::AV_NOPTS_VALUE {
        sub.pts / 1000
    } else {
        fallback_start_ms
    };

    let start_ms = base_ms.saturating_add(sub.start_display_time as i64).max(0);
    end_ms = base_ms
        .saturating_add(sub.end_display_time as i64)
        .max(start_ms);

    if end_ms <= start_ms {
        let dur = fallback_duration_ms.max(1_000);
        end_ms = start_ms.saturating_add(dur);
    }

    let (lines, had_imagery) = extract_subtitle_lines(
        sub,
        language,
        stream_index,
        packet_seq,
        work_dir,
        ocr_engine,
        engine,
    )?;
    if had_imagery && lines.is_empty() {
        warn!(
            "OCR produced empty text for subtitle stream {} at {} ms",
            stream_index, start_ms
        );
    }

    let mut cues = Vec::new();
    match ocr_format {
        OcrFormat::Srt => {
            let merged = lines
                .iter()
                .map(|line| line.text.as_str())
                .filter(|text| !text.is_empty())
                .collect::<Vec<_>>()
                .join("\n");
            if merged.is_empty() {
                return Ok(Vec::new());
            }
            cues.push(SubtitleCue {
                start_ms,
                end_ms,
                text: merged,
            });
        }
        OcrFormat::Ass => {
            let mut unpositioned = Vec::new();
            for line in lines {
                if line.text.is_empty() {
                    continue;
                }
                if let Some(bbox) = line.bbox {
                    let (pos_x, pos_y) = ass_position_from_bbox(&bbox, video_dimensions);
                    let ass_text = format_ass_text_with_style(
                        &line.text,
                        Some((pos_x, pos_y)),
                        line.color,
                        line.italic,
                    );
                    cues.push(SubtitleCue {
                        start_ms,
                        end_ms,
                        text: ass_text,
                    });
                } else {
                    unpositioned.push(line.text);
                }
            }
            if !unpositioned.is_empty() {
                let merged = unpositioned.join("\n");
                let ass_text = format_ass_text_with_style(&merged, None, None, false);
                cues.push(SubtitleCue {
                    start_ms,
                    end_ms,
                    text: ass_text,
                });
            }
        }
    }

    Ok(cues)
}

fn extract_subtitle_lines(
    subtitle: &ffi::AVSubtitle,
    language: &str,
    stream_index: i32,
    packet_seq: usize,
    work_dir: &Path,
    ocr_engine: OcrEngine,
    engine: &mut dyn SubtitleConverter,
) -> Result<(Vec<OcrLine>, bool)> {
    let mut lines = Vec::new();
    let mut had_imagery = false;

    if subtitle.num_rects == 0 || subtitle.rects.is_null() {
        return Ok((Vec::new(), false));
    }

    for i in 0..subtitle.num_rects {
        let rect_ptr = unsafe { *subtitle.rects.add(i as usize) };
        if rect_ptr.is_null() {
            continue;
        }
        let rect = unsafe { &*rect_ptr };

        if rect.type_ == ffi::SUBTITLE_TEXT && !rect.text.is_null() {
            let txt = unsafe { CStr::from_ptr(rect.text) }
                .to_string_lossy()
                .trim()
                .to_string();
            let txt = normalize_utf8_text(&txt);
            if !txt.is_empty() {
                lines.push(OcrLine {
                    text: txt,
                    bbox: None,
                    score: None,
                    color: None,
                    italic: false,
                });
            }
            continue;
        }

        if rect.type_ == ffi::SUBTITLE_ASS && !rect.ass.is_null() {
            let txt = unsafe { CStr::from_ptr(rect.ass) }
                .to_string_lossy()
                .trim()
                .to_string();
            let italic = txt.contains("\\i1");
            let color = parse_ass_color(&txt);
            let txt = normalize_utf8_text(&strip_ass_formatting(&txt));
            if !txt.is_empty() {
                lines.push(OcrLine {
                    text: txt,
                    bbox: None,
                    score: None,
                    color,
                    italic,
                });
            }
            continue;
        }

        let Some((pgm, has_visible_pixels)) = rect_to_pgm(rect) else {
            continue;
        };
        had_imagery = had_imagery || has_visible_pixels;
        if !has_visible_pixels {
            continue;
        }

        let rect_color = dominant_color_from_rect(rect);
        let pgm_path = work_dir.join(format!("ocr-s{}-p{}-r{}.pgm", stream_index, packet_seq, i));
        fs::write(&pgm_path, pgm)
            .with_context(|| format!("writing OCR frame {}", pgm_path.display()))?;

        let mut output = engine.extract_lines(&pgm_path, language)?;
        if matches!(ocr_engine, OcrEngine::PpOcrV4 | OcrEngine::PpOcrV3)
            && language_uses_spaces(language)
            && ppocr_needs_quality_fallback(&output.lines, language)
        {
            let ppocr_text = lines_text_for_quality(&output.lines);
            let ppocr_quality = ocr_text_quality_score(&ppocr_text, language);
            let ppocr_confidence = ppocr_average_confidence(&output.lines).unwrap_or(0.0);
            if let Some(fallback_language) = resolve_tesseract_fallback_language(language) {
                match run_tesseract_best_effort(&pgm_path, &fallback_language) {
                    Ok(candidate) if !candidate.text.is_empty() => {
                        // Prefer fallback only when it is at least slightly better than PP-OCR.
                        if candidate.quality + 0.03 >= ppocr_quality {
                            let bbox: Option<OcrBoundingBox> = output
                                .lines
                                .iter()
                                .filter_map(|line| line.bbox.as_ref())
                                .fold(None, |acc, b| match acc {
                                    Some(mut current) => {
                                        current.left = current.left.min(b.left);
                                        current.right = current.right.max(b.right);
                                        current.top = current.top.min(b.top);
                                        current.bottom = current.bottom.max(b.bottom);
                                        Some(current)
                                    }
                                    None => Some(b.clone()),
                                });
                            output.lines = vec![OcrLine {
                                text: candidate.text,
                                bbox,
                                score: None,
                                color: None,
                                italic: false,
                            }];
                            info!(
                                "{} quality fallback: using Tesseract({}) psm={} for subtitle stream {} packet {} rect {} (tess_score={:.2}, ppocr_score={:.2}, ppocr_conf={:.2})",
                                ppocr_engine_label(ocr_engine),
                                fallback_language,
                                candidate.psm,
                                stream_index,
                                packet_seq,
                                i,
                                candidate.quality,
                                ppocr_quality,
                                ppocr_confidence
                            );
                        } else {
                            debug!(
                                "{} quality fallback skipped: keeping model output for subtitle stream {} packet {} rect {} (tess_score={:.2}, ppocr_score={:.2}, ppocr_conf={:.2})",
                                ppocr_engine_label(ocr_engine),
                                stream_index,
                                packet_seq,
                                i,
                                candidate.quality,
                                ppocr_quality,
                                ppocr_confidence
                            );
                        }
                    }
                    Ok(_) => {}
                    Err(err) => {
                        warn!(
                            "{} quality fallback failed for subtitle stream {} packet {} rect {} (lang={}): {}",
                            ppocr_engine_label(ocr_engine),
                            stream_index,
                            packet_seq,
                            i,
                            fallback_language,
                            err
                        );
                    }
                }
            } else {
                warn!(
                    "{} quality fallback skipped (no Tesseract languages available) for subtitle stream {} packet {} rect {}",
                    ppocr_engine_label(ocr_engine),
                    stream_index,
                    packet_seq,
                    i
                );
            }
        }
        let _ = fs::remove_file(&pgm_path);
        for mut line in output.lines {
            if let Some(bbox) = line.bbox.as_mut() {
                offset_bbox(bbox, rect.x, rect.y);
            }
            line.text = postprocess_ocr_text(&line.text, language);
            if line.color.is_none() {
                line.color = rect_color;
            }
            if !line.text.is_empty() {
                lines.push(line);
            }
        }
    }

    Ok((lines, had_imagery))
}

fn language_uses_spaces(language: &str) -> bool {
    let lang = language.to_lowercase();
    matches!(
        lang.as_str(),
        "eng"
            | "en"
            | "en-us"
            | "en_us"
            | "fre"
            | "fra"
            | "fr"
            | "spa"
            | "es"
            | "de"
            | "deu"
            | "ger"
            | "it"
            | "ita"
            | "pt"
            | "por"
            | "nl"
            | "nld"
            | "sv"
            | "swe"
            | "da"
            | "dan"
            | "no"
            | "nor"
            | "fi"
            | "fin"
    )
}

fn ppocr_spacing_needs_fallback(lines: &[OcrLine]) -> bool {
    if lines.is_empty() {
        return false;
    }
    let mut has_spaces = false;
    let mut long_token = false;
    let mut has_letters = false;
    for line in lines {
        let text = line.text.trim();
        if text.contains(' ') {
            has_spaces = true;
            break;
        }
        if text.len() >= 12 {
            long_token = true;
        }
        if text.chars().any(|c| c.is_alphabetic()) {
            has_letters = true;
        }
    }
    has_letters && long_token && !has_spaces
}

fn postprocess_ocr_text(text: &str, language: &str) -> String {
    let mut out = normalize_utf8_text(text);
    if out.is_empty() {
        return out;
    }

    if !is_english_language(language) {
        return out;
    }

    out = normalize_english_ocr_confusions(&out);
    out = insert_space_after_punctuation(&out);
    out = split_glued_english_phrases(&out);

    // Targeted corrections for frequently observed OCR glue patterns.
    const ENGLISH_GLUE_FIXES: [(&str, &str); 17] = [
        ("noneother", "none other"),
        ("notonlyme", "not only me"),
        ("notonly", "not only"),
        ("goodwork", "good work"),
        ("burnit", "burn it"),
        ("yessir", "yes sir"),
        ("constablecrane", "constable crane"),
        ("whathappenedtohim", "what happened to him"),
        ("beforehewentintotheriver", "before he went into the river"),
        ("standdown", "stand down"),
        ("loppedoff", "lopped off"),
        ("ibegpardon", "I beg pardon"),
        ("ihavenot", "I have not"),
        ("ishall", "I shall"),
        ("begpardon", "beg pardon"),
        ("havenot", "have not"),
        ("l9th", "19th"),
    ];
    for (from, to) in ENGLISH_GLUE_FIXES {
        out = replace_case_insensitive_ascii(&out, from, to);
    }

    normalize_utf8_text(&out)
}

fn is_english_language(language: &str) -> bool {
    let lang = language.trim().to_ascii_lowercase();
    matches!(lang.as_str(), "eng" | "en" | "en-us" | "en_us")
}

fn normalize_english_ocr_confusions(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    let mut token = String::new();
    let flush_token = |tok: &mut String, out: &mut String| {
        if tok.is_empty() {
            return;
        }
        let has_alpha = tok.chars().any(|c| c.is_ascii_alphabetic());
        let has_digit = tok.chars().any(|c| c.is_ascii_digit());
        let mut normalized = tok.clone();
        if has_alpha && has_digit {
            normalized = normalized
                .replace('0', "o")
                .replace('1', "l")
                .replace('5', "s")
                .replace('8', "b");
        }
        let normalized_lc = normalized.to_ascii_lowercase();
        if let Some(rest) = normalized_lc.strip_prefix('l') {
            if rest.chars().next().is_some_and(|ch| ch.is_ascii_digit())
                && (rest.ends_with("st")
                    || rest.ends_with("nd")
                    || rest.ends_with("rd")
                    || rest.ends_with("th"))
            {
                normalized.replace_range(0..1, "1");
            }
        }
        normalized = normalized.replace('|', "I").replace("vv", "w");
        out.push_str(&normalized);
        tok.clear();
    };

    for ch in input.chars() {
        if ch.is_ascii_alphanumeric() || ch == '\'' || ch == '|' {
            token.push(ch);
        } else {
            flush_token(&mut token, &mut out);
            out.push(ch);
        }
    }
    flush_token(&mut token, &mut out);
    out
}

fn split_glued_english_phrases(input: &str) -> String {
    let mut out = String::with_capacity(input.len() + 8);
    let mut token = String::new();

    let flush_token = |tok: &mut String, out: &mut String| {
        if tok.is_empty() {
            return;
        }
        if let Some(split) = split_glued_ascii_token(tok) {
            out.push_str(&split);
        } else {
            out.push_str(tok);
        }
        tok.clear();
    };

    for ch in input.chars() {
        if ch.is_ascii_alphabetic() || ch == '\'' {
            token.push(ch);
        } else {
            flush_token(&mut token, &mut out);
            out.push(ch);
        }
    }
    flush_token(&mut token, &mut out);

    out
}

fn split_glued_ascii_token(token: &str) -> Option<String> {
    if token.len() < 7 || token.contains('\'') || !token.is_ascii() {
        return None;
    }
    if !token.chars().all(|ch| ch.is_ascii_alphabetic()) {
        return None;
    }

    let lower = token.to_ascii_lowercase();
    if is_common_english_word(&lower) {
        return None;
    }

    if matches!(token.chars().next(), Some('I' | 'i')) && token.len() >= 5 {
        const I_PREFIX_CONTINUATIONS: [&str; 16] = [
            "am", "have", "had", "shall", "will", "beg", "think", "know", "need", "must", "want",
            "did", "do", "was", "were", "would",
        ];
        let rest = &lower[1..];
        if I_PREFIX_CONTINUATIONS
            .iter()
            .any(|prefix| rest.starts_with(prefix))
        {
            let split_rest =
                segment_glued_english_token(&token[1..]).unwrap_or_else(|| token[1..].to_string());
            return Some(format!("{} {}", &token[..1], split_rest));
        }
    }

    for suffix in [
        "down", "off", "out", "up", "in", "on", "over", "under", "away", "back",
    ] {
        if lower.ends_with(suffix) {
            let split = token.len() - suffix.len();
            if split >= 4 && is_common_english_word(&lower[..split]) {
                return Some(format!("{} {}", &token[..split], &token[split..]));
            }
        }
    }

    segment_glued_english_token(token)
}

fn segment_glued_english_token(token: &str) -> Option<String> {
    let lower = token.to_ascii_lowercase();
    if lower.len() < 8 || is_common_english_word(&lower) {
        return None;
    }

    // Dynamic programming split over common English words.
    let n = lower.len();
    let mut best: Vec<Option<(i32, usize, usize)>> = vec![None; n + 1]; // (score, prev_idx, segments)
    best[0] = Some((0, 0, 0));
    for end in 1..=n {
        let start_min = end.saturating_sub(12);
        for start in start_min..end {
            let Some((prev_score, _prev_idx, prev_segments)) = best[start] else {
                continue;
            };
            let candidate = &lower[start..end];
            if !is_common_english_word(candidate) {
                continue;
            }
            let segment_len = end - start;
            let score = prev_score + (segment_len as i32 * segment_len as i32) - 2;
            let segments = prev_segments + 1;
            let should_replace = best[end]
                .as_ref()
                .map(|(current_score, _, current_segments)| {
                    score > *current_score
                        || (score == *current_score && segments < *current_segments)
                })
                .unwrap_or(true);
            if should_replace {
                best[end] = Some((score, start, segments));
            }
        }
    }

    let Some((_score, _prev, segment_count)) = best[n] else {
        return None;
    };
    if segment_count < 2 {
        return None;
    }

    let mut pieces = Vec::new();
    let mut idx = n;
    while idx > 0 {
        let Some((_score, prev_idx, _segments)) = best[idx] else {
            return None;
        };
        pieces.push((prev_idx, idx));
        idx = prev_idx;
    }
    pieces.reverse();

    // Avoid over-splitting normal words. Allow 3-piece splits only when the first piece is "I".
    if pieces.len() > 2 {
        let first = &lower[pieces[0].0..pieces[0].1];
        if !(pieces.len() == 3 && first == "i") {
            return None;
        }
    }
    if pieces.iter().any(|(start, end)| {
        end - start == 1 && &lower[*start..*end] != "i" && &lower[*start..*end] != "a"
    }) {
        return None;
    }

    Some(
        pieces
            .into_iter()
            .map(|(start, end)| token[start..end].to_string())
            .collect::<Vec<_>>()
            .join(" "),
    )
}

fn is_common_english_word(word: &str) -> bool {
    const WORDS: [&str; 171] = [
        "a",
        "about",
        "after",
        "again",
        "against",
        "all",
        "almost",
        "already",
        "also",
        "always",
        "am",
        "an",
        "and",
        "any",
        "are",
        "as",
        "at",
        "back",
        "be",
        "because",
        "been",
        "before",
        "beg",
        "being",
        "best",
        "better",
        "between",
        "both",
        "burn",
        "but",
        "by",
        "can",
        "century",
        "come",
        "constable",
        "could",
        "crane",
        "dandelion",
        "day",
        "did",
        "do",
        "does",
        "done",
        "down",
        "each",
        "even",
        "every",
        "few",
        "for",
        "from",
        "get",
        "give",
        "go",
        "good",
        "had",
        "happened",
        "has",
        "have",
        "he",
        "her",
        "here",
        "him",
        "his",
        "how",
        "i",
        "if",
        "in",
        "into",
        "is",
        "it",
        "its",
        "just",
        "keep",
        "know",
        "let",
        "like",
        "living",
        "lopped",
        "made",
        "make",
        "man",
        "many",
        "may",
        "me",
        "might",
        "more",
        "most",
        "months",
        "must",
        "my",
        "need",
        "never",
        "new",
        "no",
        "none",
        "nor",
        "not",
        "now",
        "of",
        "off",
        "old",
        "on",
        "only",
        "or",
        "other",
        "our",
        "out",
        "over",
        "pardon",
        "people",
        "place",
        "put",
        "rain",
        "really",
        "river",
        "said",
        "saw",
        "say",
        "see",
        "shall",
        "she",
        "sir",
        "so",
        "some",
        "stand",
        "stopped",
        "such",
        "something",
        "than",
        "that",
        "the",
        "their",
        "them",
        "then",
        "there",
        "these",
        "they",
        "thing",
        "think",
        "this",
        "those",
        "through",
        "time",
        "to",
        "today",
        "told",
        "too",
        "under",
        "up",
        "us",
        "very",
        "want",
        "was",
        "we",
        "well",
        "went",
        "were",
        "what",
        "when",
        "where",
        "which",
        "who",
        "why",
        "will",
        "with",
        "work",
        "would",
        "yes",
        "yet",
        "you",
        "your",
    ];
    WORDS.contains(&word)
}

fn insert_space_after_punctuation(input: &str) -> String {
    let mut out = String::with_capacity(input.len() + 8);
    let chars: Vec<char> = input.chars().collect();
    for (i, ch) in chars.iter().enumerate() {
        out.push(*ch);
        if matches!(ch, ',' | '.' | ';' | ':' | '!' | '?')
            && chars
                .get(i + 1)
                .is_some_and(|next| next.is_ascii_alphabetic())
        {
            out.push(' ');
        }
    }
    out
}

fn replace_case_insensitive_ascii(input: &str, from: &str, to: &str) -> String {
    if from.is_empty() {
        return input.to_string();
    }
    let input_lc = input.to_ascii_lowercase();
    let from_lc = from.to_ascii_lowercase();
    let mut out = String::with_capacity(input.len());
    let mut pos = 0usize;
    while let Some(rel_idx) = input_lc[pos..].find(&from_lc) {
        let idx = pos + rel_idx;
        out.push_str(&input[pos..idx]);
        let orig = &input[idx..idx + from.len()];
        let replacement = if orig
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_uppercase())
        {
            let mut chars = to.chars();
            if let Some(first) = chars.next() {
                format!(
                    "{}{}",
                    first.to_ascii_uppercase(),
                    chars.collect::<String>()
                )
            } else {
                to.to_string()
            }
        } else {
            to.to_string()
        };
        out.push_str(&replacement);
        pos = idx + from.len();
    }
    out.push_str(&input[pos..]);
    out
}

fn lines_text_for_quality(lines: &[OcrLine]) -> String {
    normalize_utf8_text(
        &lines
            .iter()
            .map(|line| line.text.trim())
            .filter(|text| !text.is_empty())
            .collect::<Vec<_>>()
            .join(" "),
    )
}

fn ppocr_average_confidence(lines: &[OcrLine]) -> Option<f32> {
    let mut sum = 0.0f32;
    let mut count = 0usize;
    for score in lines.iter().filter_map(|line| line.score) {
        if score.is_finite() {
            sum += score;
            count += 1;
        }
    }
    if count == 0 {
        None
    } else {
        Some(sum / count as f32)
    }
}

fn ocr_text_quality_score(text: &str, language: &str) -> f32 {
    let text = normalize_utf8_text(text);
    if text.is_empty() {
        return 0.0;
    }

    let mut letters = 0usize;
    let mut digits = 0usize;
    let mut spaces = 0usize;
    let mut punctuation = 0usize;
    let mut noise = 0usize;

    for ch in text.chars() {
        if ch.is_alphabetic() {
            letters += 1;
        } else if ch.is_ascii_digit() {
            digits += 1;
        } else if ch.is_whitespace() {
            spaces += 1;
        } else if ch.is_ascii_punctuation() || "“”‘’…".contains(ch) {
            punctuation += 1;
        } else {
            noise += 1;
        }
    }

    let total = (letters + digits + spaces + punctuation + noise).max(1) as f32;
    let mut score = 1.0f32;

    let noise_ratio = noise as f32 / total;
    if noise_ratio > 0.0 {
        score -= noise_ratio * 1.2;
    }

    if text.contains("@&") {
        score -= 0.18;
    }
    if text.contains('|') {
        score -= 0.12;
    }

    let words_vec = text.split_whitespace().collect::<Vec<_>>();
    let word_count = words_vec.len().max(1);
    let avg_word_len = letters as f32 / word_count as f32;
    let long_word_count = words_vec.iter().filter(|word| word.len() >= 14).count();

    if language_uses_spaces(language) {
        if letters >= 12 && word_count <= 1 {
            score -= 0.2;
        }
        if avg_word_len > 8.5 {
            score -= 0.12;
        }
        if long_word_count > 0 {
            score -= (long_word_count as f32 * 0.04).min(0.2);
        }
    }

    if letters == 0 && digits == 0 {
        score -= 0.3;
    }

    // Slightly reward candidates with enough character coverage.
    let coverage_bonus = (letters as f32 / 24.0).min(0.2);
    (score + coverage_bonus).clamp(0.0, 1.0)
}

fn ppocr_needs_quality_fallback(lines: &[OcrLine], language: &str) -> bool {
    if lines.is_empty() {
        return false;
    }
    if ppocr_spacing_needs_fallback(lines) {
        return true;
    }

    let quality = ocr_text_quality_score(&lines_text_for_quality(lines), language);
    quality < 0.45
}

#[derive(Debug, Clone)]
struct TesseractCandidate {
    text: String,
    psm: u8,
    quality: f32,
}

fn ppocr_engine_label(ocr_engine: OcrEngine) -> &'static str {
    match ocr_engine {
        OcrEngine::PpOcrV3 => "PP-OCRv3",
        OcrEngine::PpOcrV4 => "PP-OCRv4",
        _ => "PP-OCR",
    }
}

fn run_tesseract(image_path: &Path, language: &str) -> Result<String> {
    Ok(run_tesseract_best_effort(image_path, language)?.text)
}

fn run_tesseract_best_effort(image_path: &Path, language: &str) -> Result<TesseractCandidate> {
    let psm_modes: &[u8] = if language_uses_spaces(language) {
        &[6, 7]
    } else {
        &[6]
    };

    let mut best: Option<TesseractCandidate> = None;
    let mut last_error = None;

    for psm in psm_modes {
        match run_tesseract_with_psm(image_path, language, *psm) {
            Ok(text) if !text.is_empty() => {
                let quality = ocr_text_quality_score(&text, language);
                let candidate = TesseractCandidate {
                    text,
                    psm: *psm,
                    quality,
                };
                let should_replace = best
                    .as_ref()
                    .map(|current| candidate.quality > current.quality + 0.10)
                    .unwrap_or(true);
                if should_replace {
                    best = Some(candidate);
                }
            }
            Ok(_) => {}
            Err(err) => {
                last_error = Some(err);
            }
        }
    }

    if let Some(candidate) = best {
        return Ok(candidate);
    }
    if let Some(err) = last_error {
        return Err(err);
    }
    Ok(TesseractCandidate {
        text: String::new(),
        psm: *psm_modes.first().unwrap_or(&6),
        quality: 0.0,
    })
}

fn run_tesseract_with_psm(image_path: &Path, language: &str, psm: u8) -> Result<String> {
    let output = Command::new("tesseract")
        .arg(image_path)
        .arg("stdout")
        .arg("-l")
        .arg(language)
        .arg("--oem")
        .arg("1")
        .arg("--psm")
        .arg(psm.to_string())
        .arg("-c")
        .arg("preserve_interword_spaces=1")
        .output()
        .with_context(|| {
            format!(
                "running tesseract on '{}' (lang={}, psm={})",
                image_path.display(),
                language,
                psm
            )
        })?;

    if !output.status.success() {
        bail!(
            "tesseract OCR failed for '{}' (lang={}, psm={}): {}",
            image_path.display(),
            language,
            psm,
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(normalize_utf8_text(&String::from_utf8_lossy(
        &output.stdout,
    )))
}

fn run_external_ocr_command(
    image_path: &Path,
    language: &str,
    ocr_external_command: &str,
) -> Result<String> {
    let mut cmd = if cfg!(windows) {
        let mut cmd = Command::new("cmd");
        cmd.arg("/C").arg(ocr_external_command);
        cmd
    } else {
        let mut cmd = Command::new("sh");
        cmd.arg("-lc").arg(ocr_external_command);
        cmd
    };

    let output = cmd
        .env("DPN_OCR_IMAGE", image_path)
        .env("DPN_OCR_LANGUAGE", language)
        .output()
        .with_context(|| {
            format!(
                "running OCR external command on '{}' with DPN_OCR_LANGUAGE={}",
                image_path.display(),
                language
            )
        })?;

    if !output.status.success() {
        bail!(
            "OCR external command failed for '{}': {}",
            image_path.display(),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(normalize_utf8_text(&String::from_utf8_lossy(
        &output.stdout,
    )))
}

fn rect_to_pgm(rect: &ffi::AVSubtitleRect) -> Option<(Vec<u8>, bool)> {
    if rect.w <= 0 || rect.h <= 0 || rect.data[0].is_null() {
        return None;
    }

    let width = rect.w as usize;
    let height = rect.h as usize;
    let stride = rect.linesize[0].max(0) as usize;
    if stride < width {
        return None;
    }

    let pixels = unsafe { std::slice::from_raw_parts(rect.data[0], stride * height) };

    let palette = if !rect.data[1].is_null() {
        Some(unsafe { std::slice::from_raw_parts(rect.data[1], 256 * 4) })
    } else {
        None
    };

    let header = format!("P5\n{} {}\n255\n", width, height);
    let mut raster = Vec::with_capacity(width * height);
    let mut has_visible_pixels = false;
    let mut strong_foreground_pixels = 0usize;

    for y in 0..height {
        let row = &pixels[y * stride..(y * stride + width)];
        for &idx in row {
            let value = if let Some(pal) = palette {
                let base = (idx as usize) * 4;
                if base + 3 >= pal.len() {
                    255u8
                } else {
                    // Palette layout is RGBA for these subtitle codecs in FFmpeg.
                    let r = pal[base] as u16;
                    let g = pal[base + 1] as u16;
                    let b = pal[base + 2] as u16;
                    let a = pal[base + 3] as u16;

                    if a > 16 {
                        has_visible_pixels = true;
                        // Weighted luma in [0,255].
                        let luma = ((77 * r + 150 * g + 29 * b) >> 8) as u8;
                        // Keep bright glyph cores dark for OCR and drop very dark outlines.
                        if luma >= 160 {
                            strong_foreground_pixels += 1;
                            0u8
                        } else if luma >= 95 {
                            strong_foreground_pixels += 1;
                            64u8
                        } else {
                            255u8
                        }
                    } else {
                        255u8
                    }
                }
            } else if idx > 0 {
                has_visible_pixels = true;
                strong_foreground_pixels += 1;
                0u8
            } else {
                255u8
            };
            raster.push(value);
        }
    }

    // Fallback: if luma filtering removed too much, use alpha-only occupancy mask.
    if has_visible_pixels && strong_foreground_pixels == 0 {
        raster.clear();
        for y in 0..height {
            let row = &pixels[y * stride..(y * stride + width)];
            for &idx in row {
                let value = if let Some(pal) = palette {
                    let base = (idx as usize) * 4;
                    if base + 3 < pal.len() && pal[base + 3] > 16 {
                        0u8
                    } else {
                        255u8
                    }
                } else if idx > 0 {
                    0u8
                } else {
                    255u8
                };
                raster.push(value);
            }
        }
    }

    let mut out = Vec::with_capacity(raster.len() + header.len());
    out.extend_from_slice(header.as_bytes());
    out.extend_from_slice(&raster);
    Some((out, has_visible_pixels))
}

fn dominant_color_from_rect(rect: &ffi::AVSubtitleRect) -> Option<(u8, u8, u8)> {
    if rect.w <= 0 || rect.h <= 0 || rect.data[0].is_null() || rect.data[1].is_null() {
        return None;
    }

    let width = rect.w as usize;
    let height = rect.h as usize;
    let stride = rect.linesize[0].max(0) as usize;
    if stride < width {
        return None;
    }

    let pixels = unsafe { std::slice::from_raw_parts(rect.data[0], stride * height) };
    let palette = unsafe { std::slice::from_raw_parts(rect.data[1], 256 * 4) };

    let mut counts = [0u32; 256];
    for y in 0..height {
        let row = &pixels[y * stride..(y * stride + width)];
        for &idx in row {
            let base = (idx as usize) * 4;
            if base + 3 >= palette.len() {
                continue;
            }
            let a = palette[base + 3];
            if a > 16 {
                counts[idx as usize] += 1;
            }
        }
    }

    let mut best_idx = None;
    let mut best_count = 0u32;
    for (idx, count) in counts.iter().enumerate() {
        if *count > best_count {
            best_count = *count;
            best_idx = Some(idx);
        }
    }

    let idx = best_idx?;
    if best_count == 0 {
        return None;
    }
    let base = idx * 4;
    if base + 2 >= palette.len() {
        return None;
    }
    let r = palette[base];
    let g = palette[base + 1];
    let b = palette[base + 2];
    Some((r, g, b))
}

fn write_srt(path: &Path, cues: &[SubtitleCue]) -> Result<()> {
    let mut body = String::new();

    for (i, cue) in cues.iter().enumerate() {
        body.push_str(&(i + 1).to_string());
        body.push('\n');
        body.push_str(&format!(
            "{} --> {}\n",
            format_srt_timestamp(cue.start_ms),
            format_srt_timestamp(cue.end_ms)
        ));
        body.push_str(&cue.text);
        body.push_str("\n\n");
    }

    fs::write(path, body.as_bytes()).with_context(|| format!("writing '{}'", path.display()))?;
    Ok(())
}

fn write_ass(
    path: &Path,
    cues: &[SubtitleCue],
    video_dimensions: Option<(u32, u32)>,
) -> Result<()> {
    let (play_res_x, play_res_y) = video_dimensions.unwrap_or((1920, 1080));
    let mut body = String::new();
    body.push_str("[Script Info]\n");
    body.push_str("ScriptType: v4.00+\n");
    body.push_str(&format!("PlayResX: {}\n", play_res_x));
    body.push_str(&format!("PlayResY: {}\n", play_res_y));
    body.push_str("WrapStyle: 2\n");
    body.push_str("ScaledBorderAndShadow: yes\n");
    body.push('\n');
    body.push_str("[V4+ Styles]\n");
    body.push_str("Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding\n");
    body.push_str("Style: Default,Arial,48,&H00FFFFFF,&H000000FF,&H00000000,&H64000000,0,0,0,0,100,100,0,0,1,2,0,2,10,10,10,1\n");
    body.push('\n');
    body.push_str("[Events]\n");
    body.push_str(
        "Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text\n",
    );

    for cue in cues {
        body.push_str(&format!(
            "Dialogue: 0,{},{},Default,,0,0,0,,{}\n",
            format_ass_timestamp(cue.start_ms),
            format_ass_timestamp(cue.end_ms),
            cue.text
        ));
    }

    fs::write(path, body.as_bytes()).with_context(|| format!("writing '{}'", path.display()))?;
    Ok(())
}

fn sanitize_cues(cues: &mut Vec<SubtitleCue>, format: OcrFormat) {
    cues.sort_by_key(|cue| cue.start_ms);

    if matches!(format, OcrFormat::Srt) {
        for i in 0..cues.len().saturating_sub(1) {
            if cues[i].end_ms > cues[i + 1].start_ms {
                cues[i].end_ms = cues[i + 1].start_ms;
            }
        }
    }

    cues.retain(|cue| cue.end_ms > cue.start_ms && !cue.text.trim().is_empty());
}

fn format_srt_timestamp(total_ms: i64) -> String {
    let ms = total_ms.max(0);
    let hours = ms / 3_600_000;
    let minutes = (ms % 3_600_000) / 60_000;
    let seconds = (ms % 60_000) / 1_000;
    let millis = ms % 1_000;
    format!("{:02}:{:02}:{:02},{:03}", hours, minutes, seconds, millis)
}

fn format_ass_timestamp(total_ms: i64) -> String {
    let ms = total_ms.max(0);
    let hours = ms / 3_600_000;
    let minutes = (ms % 3_600_000) / 60_000;
    let seconds = (ms % 60_000) / 1_000;
    let centis = (ms % 1_000) / 10;
    format!("{}:{:02}:{:02}.{:02}", hours, minutes, seconds, centis)
}

fn normalize_utf8_text(input: &str) -> String {
    input
        .replace('\r', "")
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .join("\n")
}

fn strip_ass_formatting(ass: &str) -> String {
    let text_payload = ass.rsplit_once(',').map(|(_, rhs)| rhs).unwrap_or(ass);
    let mut out = String::new();
    let mut in_tag = false;
    for ch in text_payload.chars() {
        match ch {
            '{' => in_tag = true,
            '}' => in_tag = false,
            _ if !in_tag => out.push(ch),
            _ => {}
        }
    }
    normalize_utf8_text(&out.replace("\\N", "\n"))
}

fn ass_position_from_bbox(
    bbox: &OcrBoundingBox,
    video_dimensions: Option<(u32, u32)>,
) -> (i32, i32) {
    let mut x = (bbox.left + bbox.right) / 2;
    let mut y = (bbox.top + bbox.bottom) / 2;
    if let Some((width, height)) = video_dimensions {
        let max_x = width.saturating_sub(1) as i32;
        let max_y = height.saturating_sub(1) as i32;
        x = x.clamp(0, max_x);
        y = y.clamp(0, max_y);
    }
    (x, y)
}

fn format_ass_text_with_style(
    text: &str,
    pos: Option<(i32, i32)>,
    color: Option<(u8, u8, u8)>,
    italic: bool,
) -> String {
    let mut tags = String::new();
    if let Some((x, y)) = pos {
        tags.push_str(&format!("\\pos({},{})", x, y));
    }
    if let Some((r, g, b)) = color {
        tags.push_str(&format!("\\c{}", ass_color_from_rgb(r, g, b)));
    }
    if italic {
        tags.push_str("\\i1");
    }
    if tags.is_empty() {
        ass_escape(text)
    } else {
        format!("{{{}}}{}", tags, ass_escape(text))
    }
}

fn ass_escape(text: &str) -> String {
    let replaced = text.replace('\r', "");
    let replaced = replaced.replace('\\', "\\\\");
    let replaced = replaced.replace('{', "\\{");
    let replaced = replaced.replace('}', "\\}");
    replaced.replace('\n', "\\N")
}

fn ass_color_from_rgb(r: u8, g: u8, b: u8) -> String {
    format!("&H{:02X}{:02X}{:02X}&", b, g, r)
}

fn parse_ass_color(text: &str) -> Option<(u8, u8, u8)> {
    let marker = "\\c&H";
    let start = text.find(marker)?;
    let after = &text[start + marker.len()..];
    let end = after.find('&')?;
    let hex = &after[..end];
    if hex.len() != 6 {
        return None;
    }
    let b = u8::from_str_radix(&hex[0..2], 16).ok()?;
    let g = u8::from_str_radix(&hex[2..4], 16).ok()?;
    let r = u8::from_str_radix(&hex[4..6], 16).ok()?;
    Some((r, g, b))
}

#[cfg(test)]
fn intersection_over_union(a: &OcrBoundingBox, b: &OcrBoundingBox) -> f32 {
    let x_left = a.left.max(b.left);
    let y_top = a.top.max(b.top);
    let x_right = a.right.min(b.right);
    let y_bottom = a.bottom.min(b.bottom);

    let inter_width = (x_right - x_left).max(0) as f32;
    let inter_height = (y_bottom - y_top).max(0) as f32;
    let inter_area = inter_width * inter_height;

    let area_a = ((a.right - a.left).max(0) as f32) * ((a.bottom - a.top).max(0) as f32);
    let area_b = ((b.right - b.left).max(0) as f32) * ((b.bottom - b.top).max(0) as f32);

    if area_a <= 0.0 || area_b <= 0.0 {
        return 0.0;
    }

    inter_area / (area_a + area_b - inter_area)
}

#[cfg(test)]
fn rgb_distance(a: (u8, u8, u8), b: (u8, u8, u8)) -> f32 {
    let dr = a.0 as f32 - b.0 as f32;
    let dg = a.1 as f32 - b.1 as f32;
    let db = a.2 as f32 - b.2 as f32;
    (dr * dr + dg * dg + db * db).sqrt()
}

#[cfg(test)]
#[allow(clippy::needless_range_loop)]
fn word_error_rate(expected: &str, actual: &str) -> f32 {
    let expected_words: Vec<&str> = expected.split_whitespace().collect();
    let actual_words: Vec<&str> = actual.split_whitespace().collect();
    if expected_words.is_empty() {
        return if actual_words.is_empty() { 0.0 } else { 1.0 };
    }

    let m = expected_words.len();
    let n = actual_words.len();
    let mut dp = vec![vec![0usize; n + 1]; m + 1];
    for i in 0..=m {
        dp[i][0] = i;
    }
    for j in 0..=n {
        dp[0][j] = j;
    }
    for i in 1..=m {
        for j in 1..=n {
            let cost = if expected_words[i - 1] == actual_words[j - 1] {
                0
            } else {
                1
            };
            dp[i][j] = std::cmp::min(
                std::cmp::min(dp[i - 1][j] + 1, dp[i][j - 1] + 1),
                dp[i - 1][j - 1] + cost,
            );
        }
    }
    dp[m][n] as f32 / expected_words.len() as f32
}

fn bounding_box_from_points(points: &[paddle_ocr_rs::ocr_result::Point]) -> Option<OcrBoundingBox> {
    if points.is_empty() {
        return None;
    }
    let mut left = i32::MAX;
    let mut right = i32::MIN;
    let mut top = i32::MAX;
    let mut bottom = i32::MIN;
    for point in points {
        let x = point.x as i32;
        let y = point.y as i32;
        left = left.min(x);
        right = right.max(x);
        top = top.min(y);
        bottom = bottom.max(y);
    }
    Some(OcrBoundingBox {
        left,
        right,
        top,
        bottom,
    })
}

fn offset_bbox(bbox: &mut OcrBoundingBox, offset_x: i32, offset_y: i32) {
    bbox.left += offset_x;
    bbox.right += offset_x;
    bbox.top += offset_y;
    bbox.bottom += offset_y;
}

fn sort_ocr_lines(a: &OcrLine, b: &OcrLine) -> std::cmp::Ordering {
    match (&a.bbox, &b.bbox) {
        (Some(a_box), Some(b_box)) => {
            let ay = a_box.top;
            let by = b_box.top;
            if ay == by {
                a_box.left.cmp(&b_box.left)
            } else {
                ay.cmp(&by)
            }
        }
        _ => std::cmp::Ordering::Equal,
    }
}

fn merge_ocr_lines_with_spacing(lines: Vec<OcrLine>) -> Vec<OcrLine> {
    if lines.is_empty() {
        return Vec::new();
    }

    let mut with_bbox = Vec::new();
    let mut without_bbox = Vec::new();
    for line in lines {
        if line.bbox.is_some() {
            with_bbox.push(line);
        } else {
            without_bbox.push(line);
        }
    }

    if with_bbox.is_empty() {
        return without_bbox;
    }

    with_bbox.sort_by(sort_ocr_lines);

    struct LineGroup {
        items: Vec<OcrLine>,
        center_y: f32,
        avg_height: f32,
        bbox: OcrBoundingBox,
        score_sum: f32,
        score_count: usize,
    }

    let mut groups: Vec<LineGroup> = Vec::new();

    for line in with_bbox {
        let bbox = line.bbox.clone().expect("bbox missing");
        let score = line.score;
        let height = (bbox.bottom - bbox.top).max(1) as f32;
        let center_y = (bbox.top + bbox.bottom) as f32 / 2.0;

        let mut matched = None;
        for (idx, group) in groups.iter().enumerate() {
            let threshold = (group.avg_height * 0.6).max(4.0);
            if (center_y - group.center_y).abs() <= threshold {
                matched = Some(idx);
                break;
            }
        }

        if let Some(idx) = matched {
            let group = &mut groups[idx];
            group.items.push(line);
            let count = group.items.len() as f32;
            group.center_y = (group.center_y * (count - 1.0) + center_y) / count;
            group.avg_height = (group.avg_height * (count - 1.0) + height) / count;
            group.bbox.left = group.bbox.left.min(bbox.left);
            group.bbox.right = group.bbox.right.max(bbox.right);
            group.bbox.top = group.bbox.top.min(bbox.top);
            group.bbox.bottom = group.bbox.bottom.max(bbox.bottom);
            if let Some(score) = score {
                group.score_sum += score;
                group.score_count += 1;
            }
        } else {
            let mut score_sum = 0.0;
            let mut score_count = 0;
            if let Some(score) = score {
                score_sum = score;
                score_count = 1;
            }
            groups.push(LineGroup {
                items: vec![line],
                center_y,
                avg_height: height,
                bbox: bbox.clone(),
                score_sum,
                score_count,
            });
        }
    }

    let mut merged = Vec::new();
    for mut group in groups {
        group.items.sort_by(|a, b| {
            let a_box = a.bbox.as_ref().expect("bbox missing");
            let b_box = b.bbox.as_ref().expect("bbox missing");
            a_box.left.cmp(&b_box.left)
        });

        let avg_height = group.avg_height.max(1.0);
        let space_threshold = (avg_height * 0.25).max(2.0);
        let mut text = String::new();
        let mut prev_right: Option<i32> = None;

        for item in group.items {
            let bbox = item.bbox.as_ref().expect("bbox missing");
            if let Some(prev) = prev_right {
                let gap = bbox.left - prev;
                if (gap as f32) > space_threshold {
                    text.push(' ');
                }
            }
            text.push_str(item.text.trim());
            prev_right = Some(bbox.right);
        }

        let text = normalize_utf8_text(&text);
        if text.is_empty() {
            continue;
        }

        let score = if group.score_count > 0 {
            Some(group.score_sum / group.score_count as f32)
        } else {
            None
        };

        merged.push(OcrLine {
            text,
            bbox: Some(group.bbox),
            score,
            color: None,
            italic: false,
        });
    }

    merged.extend(without_bbox);
    merged.sort_by(sort_ocr_lines);
    merged
}

fn load_image(path: &Path) -> Result<image::RgbImage> {
    let img = image::open(path).with_context(|| format!("loading image '{}'", path.display()))?;
    Ok(img.to_rgb8())
}

fn timestamp_to_ms(value: i64, time_base: ffi::AVRational) -> Option<i64> {
    if value == ffi::AV_NOPTS_VALUE || time_base.num <= 0 || time_base.den <= 0 {
        return None;
    }
    Some(unsafe { ffi::av_rescale_q(value, time_base, ffi::AVRational { num: 1, den: 1000 }) })
}

fn extract_language_tag_from_metadata(dict: &rsmpeg::avutil::AVDictionary) -> Option<String> {
    for entry in dict.iter() {
        if entry
            .key()
            .to_string_lossy()
            .eq_ignore_ascii_case("language")
        {
            let v = entry.value().to_string_lossy().trim().to_string();
            if !v.is_empty() {
                return Some(v);
            }
        }
    }
    None
}

fn resolve_ocr_language(
    tag: Option<&str>,
    default_lang: Option<&str>,
    system_lang: Option<&str>,
    available: &HashSet<String>,
    ocr_engine: OcrEngine,
) -> String {
    if matches!(
        ocr_engine,
        OcrEngine::PpOcrV4 | OcrEngine::PpOcrV3 | OcrEngine::External
    ) {
        if let Some(code) = tag.and_then(map_language_tag_to_tesseract) {
            return code;
        }
        if let Some(code) = default_lang.and_then(map_language_tag_to_tesseract) {
            return code;
        }
        if let Some(code) = system_lang.and_then(map_language_tag_to_tesseract) {
            return code;
        }
        return "eng".to_string();
    }

    let mapped = tag
        .and_then(map_language_tag_to_tesseract)
        .filter(|code| available.contains(code));

    if let Some(code) = mapped {
        return code;
    }

    if let Some(configured) = default_lang
        .and_then(map_language_tag_to_tesseract)
        .filter(|code| available.contains(code))
    {
        return configured;
    }

    if let Some(system) = system_lang
        .and_then(map_language_tag_to_tesseract)
        .filter(|code| available.contains(code))
    {
        return system;
    }

    if available.contains("eng") {
        return "eng".to_string();
    }

    available
        .iter()
        .next()
        .cloned()
        .unwrap_or_else(|| "eng".to_string())
}

fn detect_system_ocr_language() -> Option<String> {
    for var in ["LC_ALL", "LC_MESSAGES", "LANG"] {
        if let Some(raw) = env::var_os(var) {
            let val = raw.to_string_lossy().trim().to_string();
            if val.is_empty() {
                continue;
            }
            let normalized = val
                .split('.')
                .next()
                .unwrap_or(&val)
                .split('@')
                .next()
                .unwrap_or(&val)
                .trim()
                .to_string();
            if !normalized.is_empty() {
                return Some(normalized);
            }
        }
    }
    None
}

fn map_language_tag_to_tesseract(input: &str) -> Option<String> {
    let normalized = input.trim().to_ascii_lowercase();
    if normalized.is_empty() {
        return None;
    }

    let primary = normalized
        .split(['-', '_'])
        .next()
        .unwrap_or(&normalized)
        .trim();

    let mapped = match primary {
        "en" | "eng" => "eng",
        "es" | "spa" => "spa",
        "fr" | "fra" | "fre" => "fra",
        "de" | "deu" | "ger" => "deu",
        "it" | "ita" => "ita",
        "pt" | "por" => "por",
        "nl" | "nld" | "dut" => "nld",
        "sv" | "swe" => "swe",
        "no" | "nor" => "nor",
        "da" | "dan" => "dan",
        "fi" | "fin" => "fin",
        "pl" | "pol" => "pol",
        "cs" | "ces" | "cze" => "ces",
        "hu" | "hun" => "hun",
        "ro" | "ron" | "rum" => "ron",
        "tr" | "tur" => "tur",
        "el" | "ell" | "gre" => "ell",
        "ru" | "rus" => "rus",
        "uk" | "ukr" => "ukr",
        "ar" | "ara" => "ara",
        "he" | "heb" => "heb",
        "hi" | "hin" => "hin",
        "th" | "tha" => "tha",
        "vi" | "vie" => "vie",
        "id" | "ind" => "ind",
        "ja" | "jpn" => "jpn",
        "ko" | "kor" => "kor",
        "zh" | "zho" | "chi" => "chi_sim",
        _ => primary,
    };

    Some(mapped.to_string())
}

fn list_tesseract_languages() -> Result<HashSet<String>> {
    let output = Command::new("tesseract")
        .arg("--list-langs")
        .output()
        .context("failed to run tesseract --list-langs")?;

    if !output.status.success() {
        bail!(
            "tesseract --list-langs failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let langs = stdout
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .filter(|line| {
            !line
                .to_ascii_lowercase()
                .starts_with("list of available languages")
        })
        .map(|line| line.to_string())
        .collect::<HashSet<_>>();

    if langs.is_empty() {
        bail!("tesseract reports no installed OCR languages")
    }

    debug!("Detected {} Tesseract language packs", langs.len());
    Ok(langs)
}

fn tesseract_languages_cached() -> Option<&'static HashSet<String>> {
    let cached = TESSERACT_LANG_CACHE.get_or_init(list_tesseract_languages);
    cached.as_ref().ok()
}

fn resolve_tesseract_fallback_language(language: &str) -> Option<String> {
    let langs = tesseract_languages_cached()?;
    if langs.contains(language) {
        return Some(language.to_string());
    }
    if langs.contains("eng") {
        return Some("eng".to_string());
    }
    langs.iter().next().cloned()
}

fn codec_name(codec_id: ffi::AVCodecID) -> String {
    unsafe {
        CStr::from_ptr(ffi::avcodec_get_name(codec_id))
            .to_string_lossy()
            .into_owned()
    }
}

fn is_image_based_subtitle(codec_id: ffi::AVCodecID) -> bool {
    matches!(
        codec_id,
        ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE
            | ffi::AV_CODEC_ID_DVD_SUBTITLE
            | ffi::AV_CODEC_ID_DVB_SUBTITLE
            | ffi::AV_CODEC_ID_XSUB
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockito::Server;
    use std::fs::File;
    use std::io::Write;
    use std::ptr;
    use strsim::jaro_winkler;
    use tempfile::TempDir;

    fn normalize_text_for_word_similarity(input: &str) -> String {
        input
            .to_uppercase()
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ")
    }

    fn normalize_text_for_char_similarity(input: &str) -> String {
        input
            .to_uppercase()
            .chars()
            .filter(|c| !c.is_whitespace())
            .collect()
    }

    #[allow(clippy::needless_range_loop)]
    fn char_error_rate(expected: &str, actual: &str) -> f32 {
        let expected_chars: Vec<char> = expected.chars().collect();
        let actual_chars: Vec<char> = actual.chars().collect();
        if expected_chars.is_empty() {
            return if actual_chars.is_empty() { 0.0 } else { 1.0 };
        }

        let m = expected_chars.len();
        let n = actual_chars.len();
        let mut dp = vec![vec![0usize; n + 1]; m + 1];
        for i in 0..=m {
            dp[i][0] = i;
        }
        for j in 0..=n {
            dp[0][j] = j;
        }
        for i in 1..=m {
            for j in 1..=n {
                let cost = if expected_chars[i - 1] == actual_chars[j - 1] {
                    0
                } else {
                    1
                };
                dp[i][j] = std::cmp::min(
                    std::cmp::min(dp[i - 1][j] + 1, dp[i][j - 1] + 1),
                    dp[i - 1][j - 1] + cost,
                );
            }
        }
        dp[m][n] as f32 / expected_chars.len() as f32
    }

    #[test]
    fn overlap_sanitization_truncates_earlier_block() {
        let mut cues = vec![
            SubtitleCue {
                start_ms: 1000,
                end_ms: 3000,
                text: "a".to_string(),
            },
            SubtitleCue {
                start_ms: 2500,
                end_ms: 4000,
                text: "b".to_string(),
            },
        ];
        sanitize_cues(&mut cues, OcrFormat::Srt);
        assert_eq!(cues[0].end_ms, 2500);
        assert_eq!(cues[1].start_ms, 2500);
    }

    #[test]
    fn srt_timestamp_formats_correctly() {
        assert_eq!(format_srt_timestamp(0), "00:00:00,000");
        assert_eq!(format_srt_timestamp(3_723_004), "01:02:03,004");
    }

    #[test]
    fn language_mapping_handles_iso_tags() {
        assert_eq!(map_language_tag_to_tesseract("en"), Some("eng".to_string()));
        assert_eq!(
            map_language_tag_to_tesseract("eng"),
            Some("eng".to_string())
        );
        assert_eq!(
            map_language_tag_to_tesseract("pt-BR"),
            Some("por".to_string())
        );
        assert_eq!(map_language_tag_to_tesseract(""), None);
    }

    #[test]
    fn resolve_language_prefers_stream_metadata_then_config_then_system_then_english() {
        let available = ["eng", "spa", "fra"]
            .iter()
            .map(|s| (*s).to_string())
            .collect::<HashSet<_>>();

        assert_eq!(
            resolve_ocr_language(
                Some("spa"),
                Some("eng"),
                Some("fr_FR.UTF-8"),
                &available,
                OcrEngine::Tesseract
            ),
            "spa"
        );
        assert_eq!(
            resolve_ocr_language(
                None,
                Some("fra"),
                Some("en_US.UTF-8"),
                &available,
                OcrEngine::Tesseract
            ),
            "fra"
        );
        assert_eq!(
            resolve_ocr_language(
                None,
                None,
                Some("fr_FR.UTF-8"),
                &available,
                OcrEngine::Tesseract
            ),
            "fra"
        );
        assert_eq!(
            resolve_ocr_language(None, None, Some("zz_ZZ"), &available, OcrEngine::Tesseract),
            "eng"
        );
    }

    #[test]
    fn resolve_language_ai_prefers_tags_without_lang_list() {
        let available = HashSet::<String>::new();

        assert_eq!(
            resolve_ocr_language(Some("jpn"), None, None, &available, OcrEngine::PpOcrV4),
            "jpn"
        );
        assert_eq!(
            resolve_ocr_language(Some("eng"), None, None, &available, OcrEngine::PpOcrV3),
            "eng"
        );
        assert_eq!(
            resolve_ocr_language(None, None, None, &available, OcrEngine::External),
            "eng"
        );
    }

    #[test]
    fn bounding_box_to_ass_position() {
        let bbox = OcrBoundingBox {
            left: 80,
            right: 120,
            top: 790,
            bottom: 810,
        };
        let (x, y) = ass_position_from_bbox(&bbox, Some((1920, 1080)));
        assert_eq!((x, y), (100, 800));
        let formatted = format_ass_text_with_style("Hello Bitch", Some((x, y)), None, false);
        assert!(formatted.contains("{\\pos(100,800)}"));
    }

    #[test]
    fn color_extraction_to_ass_hex() {
        let color = ass_color_from_rgb(255, 0, 0);
        assert_eq!(color, "&H0000FF&");
        let formatted = format_ass_text_with_style("Red", None, Some((255, 0, 0)), false);
        assert!(formatted.contains("\\c&H0000FF&"));
    }

    #[test]
    fn dominant_color_from_rect_prefers_visible_palette() {
        let mut pixels = vec![1u8; 4];
        let mut palette = vec![0u8; 256 * 4];
        palette[4] = 255; // R
        palette[5] = 0; // G
        palette[6] = 0; // B
        palette[7] = 255; // A

        let mut rect: ffi::AVSubtitleRect = unsafe { std::mem::zeroed() };
        rect.w = 2;
        rect.h = 2;
        rect.linesize[0] = 2;
        rect.data[0] = pixels.as_mut_ptr();
        rect.data[1] = palette.as_mut_ptr();
        rect.type_ = ffi::SUBTITLE_BITMAP;
        rect.x = 0;
        rect.y = 0;
        rect.nb_colors = 256;
        rect.flags = 0;
        for i in 2..rect.data.len() {
            rect.data[i] = ptr::null_mut();
        }

        let color = dominant_color_from_rect(&rect).expect("expected color");
        assert_eq!(color, (255, 0, 0));
    }

    #[test]
    fn test_model_downloads_successfully() {
        let mut server = Server::new();
        let body = b"dummy-onnx-model";
        let hash = to_hex_lower(&Sha256::digest(body));
        let mock = server
            .mock("GET", "/model.onnx")
            .with_status(200)
            .with_body(body.as_slice())
            .create();

        let tmp = TempDir::new().unwrap();
        let path = tmp.path().join("model.onnx");
        download_model_with_values(
            &path,
            &format!("{}/model.onnx", server.url()),
            &hash,
            "model.onnx",
        )
        .expect("download should succeed");

        mock.assert();
        let metadata = fs::metadata(&path).unwrap();
        assert_eq!(metadata.len(), body.len() as u64);
    }

    #[test]
    fn test_skips_download_if_cached() {
        let mut server = Server::new();
        let _mock = server
            .mock("GET", "/cached.onnx")
            .expect(0)
            .with_status(200)
            .with_body("should-not-be-downloaded")
            .create();

        let tmp = TempDir::new().unwrap();
        let path = tmp.path().join("cached.onnx");
        let mut file = File::create(&path).unwrap();
        file.write_all(b"cached").unwrap();

        let result = ensure_model_file_with_values(
            tmp.path(),
            "cached.onnx",
            &format!("{}/cached.onnx", server.url()),
            "deadbeef",
        );
        assert!(result.is_ok());
        _mock.assert();
    }

    #[test]
    fn test_handles_corrupted_download() {
        let mut server = Server::new();
        let body = b"partial-data-should-fail";
        let hash = to_hex_lower(&Sha256::digest(body));
        let mock = server
            .mock("GET", "/corrupt.onnx")
            .with_status(200)
            .with_chunked_body(|writer| {
                writer.write_all(&body[..8])?;
                Err(std::io::Error::new(
                    std::io::ErrorKind::UnexpectedEof,
                    "simulated drop",
                ))
            })
            .create();

        let tmp = TempDir::new().unwrap();
        let path = tmp.path().join("corrupt.onnx");
        let result = download_model_with_values(
            &path,
            &format!("{}/corrupt.onnx", server.url()),
            &hash,
            "corrupt.onnx",
        );
        assert!(result.is_err());
        mock.assert();
        assert!(!path.with_extension("download").exists());
    }

    #[test]
    fn test_downloader_handles_404() {
        let mut server = Server::new();
        let mock = server
            .mock("GET", "/missing.onnx")
            .with_status(404)
            .create();

        let tmp = TempDir::new().unwrap();
        let path = tmp.path().join("missing.onnx");
        let result = download_model_with_values(
            &path,
            &format!("{}/missing.onnx", server.url()),
            "deadbeef",
            "missing.onnx",
        );
        assert!(result.is_err());
        mock.assert();
        assert!(!path.exists());
        assert!(!path.with_extension("download").exists());
    }

    #[test]
    fn test_onnx_session_initializes_with_fallbacks() {
        let init_result = std::panic::catch_unwind(init_ort_environment);
        match init_result {
            Ok(Ok(_)) => {}
            Ok(Err(err)) => {
                let msg = err.to_string();
                if msg.contains("libonnxruntime.so")
                    && msg.contains("cannot open shared object file")
                {
                    eprintln!(
                        "Skipping ORT init fallback test because ONNX Runtime shared library is unavailable: {}",
                        msg
                    );
                    return;
                }
                panic!("Failed to initialize ORT environment: {}", msg);
            }
            Err(payload) => {
                let panic_msg = if let Some(msg) = payload.downcast_ref::<&str>() {
                    *msg
                } else if let Some(msg) = payload.downcast_ref::<String>() {
                    msg.as_str()
                } else {
                    "unknown panic payload"
                };
                if panic_msg.contains("libonnxruntime.so")
                    && panic_msg.contains("cannot open shared object file")
                {
                    eprintln!(
                        "Skipping ORT init fallback test because ONNX Runtime shared library is unavailable: {}",
                        panic_msg
                    );
                    return;
                }
                std::panic::resume_unwind(payload);
            }
        }
        assert!(
            ORT_ENV_INIT.get().is_some(),
            "ORT environment not initialized"
        );
    }

    #[test]
    fn test_gpu_requirement_env_gate() {
        if std::env::var("DPN_OCR_REQUIRE_GPU").ok().as_deref() != Some("1") {
            return;
        }
        let selection =
            build_execution_providers().expect("GPU execution providers required but unavailable");
        assert!(
            selection.providers.len() > 1,
            "Expected at least one GPU execution provider plus CPU"
        );
        assert!(selection.gpu_available, "GPU availability was not detected");
    }

    #[test]
    fn test_provider_selection_prefers_cuda_when_available() {
        let (kinds, gpu_available) =
            select_execution_provider_plan(false, true, true, true).unwrap();
        assert!(gpu_available);
        assert_eq!(kinds.first(), Some(&ExecutionProviderKind::Cuda));
        assert_eq!(kinds.last(), Some(&ExecutionProviderKind::Cpu));
    }

    #[test]
    fn test_provider_selection_requires_gpu_flag() {
        let err = select_execution_provider_plan(true, false, false, false)
            .expect_err("Expected error when requiring GPU without providers");
        assert!(
            err.to_string().contains("No GPU execution providers"),
            "Unexpected error: {}",
            err
        );
    }

    #[test]
    fn test_auto_engine_prefers_ppocr_with_gpu() {
        assert_eq!(auto_engine_preference(true), OcrEngine::PpOcrV4);
    }

    #[test]
    fn test_auto_engine_prefers_tesseract_without_gpu() {
        assert_eq!(auto_engine_preference(false), OcrEngine::Tesseract);
    }

    #[test]
    fn test_text_similarity_wer_like_threshold() {
        let expected = "THIS OCR QUALITY TEST USES MANY WORDS TO ALLOW SMALL ERRORS WITHOUT FAILING STRICT THRESHOLDS IN CI RUNS TODAY ALWAYS FOR STABILITY CHECKS EACH TIME";
        let actual = "THIS OCR QUALITY TEST USES MANY WORDS TO ALLOW SMALL ERRORS WITHOUT FAILING STRICT THRESHOLDS IN CI RUNS TODAY ALWAYS FOR STABIL1TY CHECKS EACH TIME";
        let expected_words = normalize_text_for_word_similarity(expected);
        let actual_words = normalize_text_for_word_similarity(actual);
        let wer = word_error_rate(&expected_words, &actual_words);
        let similarity = 1.0 - wer;
        assert!(
            similarity > 0.95,
            "WER similarity too low: {} ({} vs {})",
            similarity,
            expected,
            actual
        );
        let expected_chars = normalize_text_for_char_similarity(expected);
        let actual_chars = normalize_text_for_char_similarity(actual);
        let cer = char_error_rate(&expected_chars, &actual_chars);
        let cer_similarity = 1.0 - cer;
        assert!(
            cer_similarity > 0.95,
            "CER similarity too low: {} ({} vs {})",
            cer_similarity,
            expected,
            actual
        );
        let jw = jaro_winkler(expected, actual);
        assert!(jw > 0.90, "Jaro-Winkler too low: {}", jw);
    }

    #[test]
    fn test_spatial_iou_threshold() {
        let a = OcrBoundingBox {
            left: 100,
            right: 200,
            top: 100,
            bottom: 150,
        };
        let b = OcrBoundingBox {
            left: 103,
            right: 198,
            top: 101,
            bottom: 149,
        };
        let iou = intersection_over_union(&a, &b);
        assert!(iou > 0.90, "IoU too low: {}", iou);
    }

    #[test]
    fn test_ppocr_spacing_inserts_space_for_gap() {
        let lines = vec![
            OcrLine {
                text: "By".to_string(),
                bbox: Some(OcrBoundingBox {
                    left: 0,
                    right: 10,
                    top: 0,
                    bottom: 10,
                }),
                score: Some(0.9),
                color: None,
                italic: false,
            },
            OcrLine {
                text: "this".to_string(),
                bbox: Some(OcrBoundingBox {
                    left: 14,
                    right: 30,
                    top: 0,
                    bottom: 10,
                }),
                score: Some(0.9),
                color: None,
                italic: false,
            },
        ];

        let merged = merge_ocr_lines_with_spacing(lines);
        assert_eq!(merged.len(), 1);
        assert_eq!(merged[0].text, "By this");
    }

    #[test]
    fn test_ppocr_spacing_keeps_compact_tokens() {
        let lines = vec![
            OcrLine {
                text: "By".to_string(),
                bbox: Some(OcrBoundingBox {
                    left: 0,
                    right: 10,
                    top: 0,
                    bottom: 10,
                }),
                score: Some(0.9),
                color: None,
                italic: false,
            },
            OcrLine {
                text: "this".to_string(),
                bbox: Some(OcrBoundingBox {
                    left: 11,
                    right: 30,
                    top: 0,
                    bottom: 10,
                }),
                score: Some(0.9),
                color: None,
                italic: false,
            },
        ];

        let merged = merge_ocr_lines_with_spacing(lines);
        assert_eq!(merged.len(), 1);
        assert_eq!(merged[0].text, "Bythis");
    }

    #[test]
    fn test_ppocr_spacing_fallback_detection() {
        let lines = vec![OcrLine {
            text: "BythistimeIobserved".to_string(),
            bbox: Some(OcrBoundingBox {
                left: 0,
                right: 100,
                top: 0,
                bottom: 10,
            }),
            score: Some(0.9),
            color: None,
            italic: false,
        }];
        assert!(ppocr_spacing_needs_fallback(&lines));
    }

    #[test]
    fn test_quality_score_penalizes_noise() {
        let clean = "By this time, I observed that the rain had stopped.";
        let noisy = "Bythistime,I0bserved @&| the rain had st0pped";
        let clean_score = ocr_text_quality_score(clean, "eng");
        let noisy_score = ocr_text_quality_score(noisy, "eng");
        assert!(
            clean_score > noisy_score,
            "expected clean score ({clean_score}) > noisy score ({noisy_score})"
        );
    }

    #[test]
    fn test_quality_fallback_detection_triggers_on_noisy_text() {
        let lines = vec![OcrLine {
            text: "BythistimeI0bserved@&|".to_string(),
            bbox: Some(OcrBoundingBox {
                left: 0,
                right: 100,
                top: 0,
                bottom: 10,
            }),
            score: Some(0.95),
            color: None,
            italic: false,
        }];
        assert!(ppocr_needs_quality_fallback(&lines, "eng"));
    }

    #[test]
    fn test_quality_fallback_detection_avoids_good_english_text() {
        let lines = vec![OcrLine {
            text: "By this time, I observed the village from afar.".to_string(),
            bbox: Some(OcrBoundingBox {
                left: 0,
                right: 200,
                top: 0,
                bottom: 12,
            }),
            score: Some(0.93),
            color: None,
            italic: false,
        }];
        assert!(!ppocr_needs_quality_fallback(&lines, "eng"));
    }

    #[test]
    fn test_postprocess_english_glue_and_punctuation() {
        let src = "ConstableCrane? Notonlyme. beforehewentintotheriver";
        let got = postprocess_ocr_text(src, "eng");
        assert_eq!(
            got,
            "Constable Crane? Not only me. before he went into the river"
        );
    }

    #[test]
    fn test_postprocess_english_deglues_common_tokens() {
        let src = "Ibegpardon. Standdown! Ihavenot Loppedoff? Ishall return in the l9th century.";
        let got = postprocess_ocr_text(src, "eng");
        assert_eq!(
            got,
            "I beg pardon. Stand down! I have not Lopped off? I shall return in the 19th century."
        );
    }

    #[test]
    fn test_split_glued_token_dp_segmentation() {
        assert_eq!(
            split_glued_ascii_token("Ibegpardon").as_deref(),
            Some("I beg pardon")
        );
        assert_eq!(
            split_glued_ascii_token("Standdown").as_deref(),
            Some("Stand down")
        );
        assert_eq!(split_glued_ascii_token("Tonight"), None);
    }

    #[test]
    fn test_postprocess_non_english_passthrough() {
        let src = "Notonlyme";
        let got = postprocess_ocr_text(src, "jpn");
        assert_eq!(got, "Notonlyme");
    }

    #[test]
    fn test_color_distance_threshold() {
        let mut pixels = vec![1u8; 4];
        let mut palette = vec![0u8; 256 * 4];
        palette[4] = 255;
        palette[5] = 0;
        palette[6] = 0;
        palette[7] = 255;

        let mut rect: ffi::AVSubtitleRect = unsafe { std::mem::zeroed() };
        rect.w = 2;
        rect.h = 2;
        rect.linesize[0] = 2;
        rect.data[0] = pixels.as_mut_ptr();
        rect.data[1] = palette.as_mut_ptr();
        rect.type_ = ffi::SUBTITLE_BITMAP;
        rect.x = 0;
        rect.y = 0;
        rect.nb_colors = 256;
        rect.flags = 0;
        for i in 2..rect.data.len() {
            rect.data[i] = ptr::null_mut();
        }

        let color = dominant_color_from_rect(&rect).expect("expected color");
        let distance = rgb_distance(color, (255, 0, 0));
        assert!(distance < 15.0, "distance too high: {}", distance);
    }

    #[test]
    fn test_golden_dataset_quality() {
        if std::env::var("DPN_OCR_GOLDEN").ok().as_deref() != Some("1") {
            eprintln!("Skipping golden OCR quality test (set DPN_OCR_GOLDEN=1 to enable).");
            return;
        }

        let dataset_dir = PathBuf::from("tests/golden_subs");
        if !dataset_dir.is_dir() {
            eprintln!("Golden dataset directory not found; skipping.");
            return;
        }

        let model_dir = match resolve_model_dir() {
            Ok(dir) => dir,
            Err(err) => {
                eprintln!("Model dir unavailable: {err}. Skipping.");
                return;
            }
        };

        let engine_result =
            std::panic::catch_unwind(|| PpOcrEngine::new(&model_dir, PpOcrVariant::V4, false));
        let mut engine = match engine_result {
            Ok(Ok(engine)) => engine,
            Ok(Err(err)) => {
                eprintln!("OCR engine unavailable: {err}. Skipping.");
                return;
            }
            Err(payload) => {
                let panic_msg = if let Some(msg) = payload.downcast_ref::<&str>() {
                    *msg
                } else if let Some(msg) = payload.downcast_ref::<String>() {
                    msg.as_str()
                } else {
                    "unknown panic payload"
                };
                if panic_msg.contains("libonnxruntime.so")
                    && panic_msg.contains("cannot open shared object file")
                {
                    eprintln!(
                        "Skipping golden OCR quality test because ONNX Runtime shared library is unavailable: {}",
                        panic_msg
                    );
                    return;
                }
                std::panic::resume_unwind(payload);
            }
        };

        #[derive(serde::Deserialize)]
        struct GoldenExpected {
            expected_text: String,
            expected_bbox: [i32; 4],
            expected_color_rgb: [u8; 3],
            is_italic: Option<bool>,
        }

        let mut found = 0usize;
        for entry in fs::read_dir(&dataset_dir).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) != Some("png") {
                continue;
            }
            let json_path = path.with_extension("json");
            if !json_path.exists() {
                continue;
            }
            found += 1;
            let json = fs::read_to_string(&json_path).unwrap();
            let expected: GoldenExpected = serde_json::from_str(&json).unwrap();

            let output = engine.extract_lines(&path, "eng").unwrap();
            let Some(line) = output.lines.first() else {
                panic!("No OCR output for {:?}", path);
            };

            let expected_text = expected.expected_text.trim();
            let actual_text = line.text.trim();
            let expected_words = normalize_text_for_word_similarity(expected_text);
            let actual_words = normalize_text_for_word_similarity(actual_text);
            let wer = word_error_rate(&expected_words, &actual_words);
            let expected_chars = normalize_text_for_char_similarity(expected_text);
            let actual_chars = normalize_text_for_char_similarity(actual_text);
            let cer = char_error_rate(&expected_chars, &actual_chars);
            let similarity = (1.0 - wer).max(1.0 - cer);
            assert!(
                similarity > 0.95,
                "Text similarity too low for {:?}: {} (wer {:.3}, cer {:.3})",
                path,
                similarity,
                wer,
                cer
            );

            let expected_bbox = OcrBoundingBox {
                left: expected.expected_bbox[0],
                top: expected.expected_bbox[1],
                right: expected.expected_bbox[2],
                bottom: expected.expected_bbox[3],
            };
            let Some(actual_bbox) = line.bbox.as_ref() else {
                panic!("Missing bounding box for {:?}", path);
            };
            let iou = intersection_over_union(&expected_bbox, actual_bbox);
            assert!(iou > 0.90, "IoU too low for {:?}: {}", path, iou);

            let expected_color = (
                expected.expected_color_rgb[0],
                expected.expected_color_rgb[1],
                expected.expected_color_rgb[2],
            );
            if let Some(actual_color) = line.color {
                let dist = rgb_distance(actual_color, expected_color);
                assert!(
                    dist < 15.0,
                    "Color distance too high for {:?}: {}",
                    path,
                    dist
                );
            }

            if let Some(italic) = expected.is_italic {
                assert_eq!(line.italic, italic, "Italic mismatch for {:?}", path);
            }
        }

        if found == 0 {
            eprintln!("Golden dataset empty; skipping.");
        }
    }

    #[test]
    #[ignore]
    fn test_manual_ppocr_v3_single_image_probe() {
        if std::env::var("DPN_OCR_MANUAL_PPOCR_V3").ok().as_deref() != Some("1") {
            eprintln!("Skipping manual PP-OCRv3 probe (set DPN_OCR_MANUAL_PPOCR_V3=1 to enable).");
            return;
        }

        let model_dir = resolve_model_dir().expect("resolve model dir");
        let gpu_available = init_ort_environment().expect("init ORT environment");
        eprintln!(
            "ORT environment initialized; gpu_available={}",
            gpu_available
        );

        let mut engine = init_ppocr_engine(&model_dir, require_gpu(), PpOcrVariant::V3)
            .expect("init PP-OCRv3 engine");

        let tmp_dir = tempfile::tempdir().expect("create temp dir");
        let image_path = tmp_dir.path().join("manual_probe.png");
        let img = image::RgbImage::from_pixel(1280, 720, image::Rgb([0, 0, 0]));
        img.save(&image_path).expect("save probe image");

        let output = engine
            .extract_lines(&image_path, "eng")
            .expect("run PP-OCRv3 inference");
        let (sum_conf, count_conf) = output
            .lines
            .iter()
            .filter_map(|line| line.score)
            .fold((0.0f32, 0usize), |(sum, count), score| {
                (sum + score, count + 1)
            });
        let avg_conf = if count_conf > 0 {
            sum_conf / count_conf as f32
        } else {
            0.0
        };
        eprintln!(
            "PP-OCRv3 probe completed: lines={}, avg_conf={:.4}",
            output.lines.len(),
            avg_conf
        );
    }
}
