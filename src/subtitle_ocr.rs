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
use text_processing::{
    is_english_language, language_uses_spaces, lines_text_for_quality, ocr_text_quality_score,
    postprocess_ocr_text, ppocr_average_confidence, ppocr_needs_quality_fallback,
};
#[cfg(test)]
use text_processing::{ppocr_spacing_needs_fallback, split_glued_ascii_token};

mod text_processing;

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
static LEGACY_NVIDIA_MAXWELL: OnceLock<bool> = OnceLock::new();
static DISABLE_TESS_FALLBACK_LOGGED: AtomicBool = AtomicBool::new(false);

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
                return Ok((OcrEngine::Tesseract, Box::new(TesseractEngine)));
            }
            let variant = match selected_engine {
                OcrEngine::PpOcrV3 => PpOcrVariant::V3,
                OcrEngine::PpOcrV4 => PpOcrVariant::V4,
                _ => PpOcrVariant::V4,
            };

            match (|| -> Result<PpOcrEngine> {
                let model_dir = resolve_model_dir()?;
                init_ppocr_engine(&model_dir, require_gpu, variant)
            })() {
                Ok(engine) => {
                    info!(
                        "Auto-selected {} engine with GPU acceleration",
                        variant.label()
                    );
                    Ok((selected_engine, Box::new(engine)))
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
    auto_engine_preference_with_capability(gpu_available, prefer_ppocr_v3_for_legacy_nvidia())
}

fn auto_engine_preference_with_capability(
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

fn prefer_ppocr_v3_for_legacy_nvidia() -> bool {
    *LEGACY_NVIDIA_MAXWELL.get_or_init(detect_legacy_nvidia_maxwell)
}

fn detect_legacy_nvidia_maxwell() -> bool {
    #[cfg(not(any(target_os = "linux", target_os = "windows")))]
    {
        return false;
    }

    #[cfg(any(target_os = "linux", target_os = "windows"))]
    {
        let output = Command::new("nvidia-smi")
            .arg("--query-gpu=compute_cap,name")
            .arg("--format=csv,noheader,nounits")
            .output();

        let Ok(output) = output else {
            return false;
        };
        if !output.status.success() {
            return false;
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        for line in stdout
            .lines()
            .map(str::trim)
            .filter(|line| !line.is_empty())
        {
            let mut parts = line.splitn(2, ',').map(str::trim);
            let cap = parts.next().unwrap_or_default();
            let name = parts.next().unwrap_or_default();
            let major = cap
                .split('.')
                .next()
                .and_then(|s| s.parse::<u32>().ok())
                .unwrap_or(999);
            if major <= 5 {
                info!(
                    "Detected legacy NVIDIA GPU '{}'(compute capability {}). Auto-selecting PP-OCRv3.",
                    name,
                    cap
                );
                return true;
            }
        }
        false
    }
}

fn disable_tesseract_quality_fallback() -> bool {
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
    cls: PathBuf,
    rec: PathBuf,
}

impl PpOcrEngine {
    fn new(model_dir: &Path, variant: PpOcrVariant, skip_cls: bool) -> Result<Self> {
        let models = ensure_ppocr_models(model_dir, variant, skip_cls)?;
        info!(
            "Initializing {} models (det='{}', cls='{}', rec='{}')",
            variant.label(),
            models.det.display(),
            models.cls.display(),
            models.rec.display()
        );
        let mut ocr = OcrLite::new();
        ocr.init_models_custom(
            models.det.to_string_lossy().as_ref(),
            models.cls.to_string_lossy().as_ref(),
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

        let Some((pgm, has_visible_pixels)) = rect_to_pgm(rect, ocr_engine) else {
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
        let force_tesseract_non_english =
            !is_english_language(language) && language_uses_spaces(language);
        if matches!(ocr_engine, OcrEngine::PpOcrV4 | OcrEngine::PpOcrV3)
            && language_uses_spaces(language)
            && !disable_tesseract_quality_fallback()
            && (force_tesseract_non_english
                || ppocr_needs_quality_fallback(&output.lines, language))
        {
            let ppocr_text = lines_text_for_quality(&output.lines);
            let ppocr_quality = ocr_text_quality_score(&ppocr_text, language);
            let ppocr_confidence = ppocr_average_confidence(&output.lines).unwrap_or(0.0);
            if let Some(fallback_language) = resolve_tesseract_fallback_language(language) {
                match run_tesseract_best_effort(&pgm_path, &fallback_language) {
                    Ok(candidate) if !candidate.text.is_empty() => {
                        // For non-English streams, prefer language-specific Tesseract
                        // because the bundled PP-OCR recognizer is English-focused.
                        if force_tesseract_non_english || candidate.quality + 0.03 >= ppocr_quality
                        {
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
                            if force_tesseract_non_english {
                                info!(
                                    "{} language fallback: using Tesseract({}) psm={} for non-English subtitle stream {} packet {} rect {} (tess_score={:.2}, ppocr_score={:.2}, ppocr_conf={:.2})",
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
                            }
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
    let argv = parse_external_ocr_argv(ocr_external_command)?;
    let mut cmd = Command::new(&argv[0]);
    cmd.args(&argv[1..]);

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

fn parse_external_ocr_argv(ocr_external_command: &str) -> Result<Vec<String>> {
    let argv = shlex::split(ocr_external_command).ok_or_else(|| {
        anyhow!("Invalid --ocr-external-command value: could not parse command/arguments safely")
    })?;
    if argv.is_empty() {
        bail!("--ocr-external-command must include a program name");
    }
    Ok(argv)
}

fn rect_to_pgm(rect: &ffi::AVSubtitleRect, ocr_engine: OcrEngine) -> Option<(Vec<u8>, bool)> {
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

    let mut raster = Vec::with_capacity(width * height);
    let mut has_visible_pixels = false;
    let mut strong_foreground_pixels = 0usize;
    let ai_mode = matches!(ocr_engine, OcrEngine::PpOcrV3 | OcrEngine::PpOcrV4);

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
                        let luma = ((77 * r + 150 * g + 29 * b) >> 8) as u8;
                        if ai_mode {
                            // Preserve grayscale detail for PP-OCR while emphasizing bright, opaque glyph cores.
                            let ink = ((luma as u16 * a + 127) / 255) as u8;
                            let value = 255u8.saturating_sub(ink);
                            if value < 220 {
                                strong_foreground_pixels += 1;
                            }
                            value
                        } else {
                            // Tesseract path: binarized foreground with mild antialias.
                            if luma >= 160 {
                                strong_foreground_pixels += 1;
                                0u8
                            } else if luma >= 95 {
                                strong_foreground_pixels += 1;
                                64u8
                            } else {
                                255u8
                            }
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

    let (final_raster, final_w, final_h) = if ai_mode {
        upscale_grayscale_nearest(&raster, width, height, 2)
    } else {
        (raster, width, height)
    };
    let header = format!("P5\n{} {}\n255\n", final_w, final_h);
    let mut out = Vec::with_capacity(final_raster.len() + header.len());
    out.extend_from_slice(header.as_bytes());
    out.extend_from_slice(&final_raster);
    Some((out, has_visible_pixels))
}

fn upscale_grayscale_nearest(
    raster: &[u8],
    width: usize,
    height: usize,
    factor: usize,
) -> (Vec<u8>, usize, usize) {
    if factor <= 1 || width == 0 || height == 0 {
        return (raster.to_vec(), width, height);
    }

    let out_w = width * factor;
    let out_h = height * factor;
    let mut out = vec![255u8; out_w * out_h];
    for y in 0..height {
        for x in 0..width {
            let value = raster[y * width + x];
            let base_y = y * factor;
            let base_x = x * factor;
            for dy in 0..factor {
                let out_row = (base_y + dy) * out_w;
                for dx in 0..factor {
                    out[out_row + base_x + dx] = value;
                }
            }
        }
    }
    (out, out_w, out_h)
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
    resolve_tesseract_fallback_language_with_available(language, langs)
}

fn resolve_tesseract_fallback_language_with_available(
    language: &str,
    langs: &HashSet<String>,
) -> Option<String> {
    let mapped = map_language_tag_to_tesseract(language).unwrap_or_else(|| language.to_string());
    if langs.contains(&mapped) {
        return Some(mapped);
    }
    // Do not silently fall back non-English streams to English OCR;
    // that degrades quality for languages like French/Spanish.
    if is_english_language(&mapped) && langs.contains("eng") {
        return Some("eng".to_string());
    }
    None
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
mod tests;
