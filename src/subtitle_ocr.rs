use anyhow::{anyhow, bail, Context, Result};
use log::{debug, info, warn};
use ort::execution_providers::{CPUExecutionProvider, ExecutionProviderDispatch};
#[cfg(any(target_os = "linux", target_os = "windows"))]
use ort::execution_providers::CUDAExecutionProvider;
#[cfg(target_vendor = "apple")]
use ort::execution_providers::CoreMLExecutionProvider;
#[cfg(target_os = "windows")]
use ort::execution_providers::DirectMLExecutionProvider;
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
use std::sync::OnceLock;

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

struct PpOcrV4Engine {
    ocr: OcrLite,
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

impl SubtitleConverter for PpOcrV4Engine {
    fn extract_lines(&mut self, image_path: &Path, _language: &str) -> Result<OcrOutput> {
        let img = load_image(image_path)?;
        let result = self
            .ocr
            .detect(&img, 50, 1024, 0.5, 0.3, 1.6, false, false)
            .map_err(|err| anyhow!("PP-OCRv4 failed: {}", err))?;

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

        lines.sort_by(|a, b| sort_ocr_lines(a, b));

        Ok(OcrOutput { lines })
    }
}

static ORT_ENV_INIT: OnceLock<()> = OnceLock::new();

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

    let available_langs = if matches!(ocr_engine, OcrEngine::Tesseract) {
        list_tesseract_languages().context(
            "Failed to query Tesseract language packs. Install `tesseract-ocr` and required traineddata files.",
        )?
    } else {
        HashSet::new()
    };

    let input_path = input_file
        .to_str()
        .map_err(|_| anyhow!("Input path must be valid UTF-8 for OCR side pass"))?
        .to_string();
    let system_language = detect_system_ocr_language();
    let video_dimensions = probe_video_dimensions(input_file);

    let mut engine = build_ocr_engine(ocr_engine, ocr_external_command)?;

    let mut tracks = Vec::with_capacity(candidates.len());
    for candidate in candidates {
        let resolved_lang = resolve_ocr_language(
            candidate.language_tag.as_deref(),
            default_language,
            system_language.as_deref(),
            &available_langs,
            ocr_engine,
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
) -> Result<Box<dyn SubtitleConverter>> {
    match ocr_engine {
        OcrEngine::Tesseract => Ok(Box::new(TesseractEngine)),
        OcrEngine::External => {
            let command = ocr_external_command
                .ok_or_else(|| anyhow!("missing OCR external command"))?
                .to_string();
            Ok(Box::new(ExternalEngine { command }))
        }
        OcrEngine::PpOcrV4 => {
            init_ort_environment();
            let model_dir = resolve_model_dir()?;
            let engine = PpOcrV4Engine::new(&model_dir)?;
            Ok(Box::new(engine))
        }
    }
}

fn init_ort_environment() {
    ORT_ENV_INIT.get_or_init(|| {
        let providers = build_execution_providers();
        match ort::init().with_execution_providers(providers).commit() {
            Ok(true) => info!("Initialized ONNX Runtime environment for OCR execution providers"),
            Ok(false) => debug!("ONNX Runtime environment already initialized; skipping reconfigure"),
            Err(err) => warn!("Failed to initialize ONNX Runtime environment: {}", err),
        }
    });
}

struct ModelSpec {
    filename: &'static str,
    url: &'static str,
    sha256: &'static str,
}

const PPOCR_DET_MODEL: ModelSpec = ModelSpec {
    filename: "ch_PP-OCRv4_det_infer.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/v3.7.0/onnx/PP-OCRv4/det/ch_PP-OCRv4_det_infer.onnx",
    sha256: "D2A7720D45A54257208B1E13E36A8479894CB74155A5EFE29462512D42F49DA9",
};
const PPOCR_CLS_MODEL: ModelSpec = ModelSpec {
    filename: "ch_ppocr_mobile_v2.0_cls_infer.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/v3.7.0/onnx/PP-OCRv4/cls/ch_ppocr_mobile_v2.0_cls_infer.onnx",
    sha256: "E47ACEDF663230F8863FF1AB0E64DD2D82B838FCEB5957146DAB185A89D6215C",
};
const PPOCR_REC_MODEL: ModelSpec = ModelSpec {
    filename: "en_PP-OCRv4_rec_infer.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/v3.7.0/onnx/PP-OCRv4/rec/en_PP-OCRv4_rec_infer.onnx",
    sha256: "E8770C967605983D1570CDF5352041DFB68FA0C21664F49F47B155ABD3E0E318",
};

struct PpOcrModels {
    det: PathBuf,
    cls: PathBuf,
    rec: PathBuf,
}

impl PpOcrV4Engine {
    fn new(model_dir: &Path) -> Result<Self> {
        let models = ensure_ppocr_models(model_dir)?;
        let mut ocr = OcrLite::new();
        ocr.init_models_custom(
            models.det.to_string_lossy().as_ref(),
            models.cls.to_string_lossy().as_ref(),
            models.rec.to_string_lossy().as_ref(),
            configure_ort_builder,
        )
        .map_err(|err| anyhow!("failed to initialize PP-OCRv4 models: {}", err))?;
        Ok(Self { ocr })
    }
}

fn configure_ort_builder(builder: SessionBuilder) -> Result<SessionBuilder, ort::Error> {
    let providers = build_execution_providers();
    let mut builder = builder.with_execution_providers(providers)?;
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

fn build_execution_providers() -> Vec<ExecutionProviderDispatch> {
    let mut providers = Vec::new();
    #[cfg(any(target_os = "linux", target_os = "windows"))]
    {
        providers.push(CUDAExecutionProvider::default().build());
    }
    #[cfg(target_os = "windows")]
    {
        providers.push(DirectMLExecutionProvider::default().build());
    }
    #[cfg(target_vendor = "apple")]
    {
        providers.push(CoreMLExecutionProvider::default().build());
    }
    providers.push(CPUExecutionProvider::default().build());
    providers
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
        PathBuf::from(xdg)
            .join("direct-play-nice")
            .join("models")
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

fn ensure_ppocr_models(model_dir: &Path) -> Result<PpOcrModels> {
    let det = ensure_model_file(model_dir, &PPOCR_DET_MODEL)?;
    let cls = ensure_model_file(model_dir, &PPOCR_CLS_MODEL)?;
    let rec = ensure_model_file(model_dir, &PPOCR_REC_MODEL)?;
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
                    out.push(PendingPacket { ts, packet: encoded });
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

    fs::rename(&tmp_out, &output_path)
        .with_context(|| format!("replacing '{}' after container remux", output_path.display()))?;

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
    if is_mp4 && tracks.iter().any(|track| matches!(track.format, OcrFormat::Ass)) {
        warn!("ASS OCR output is being remuxed into MP4; formatting will be downgraded to mov_text");
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
        let output_time_base =
            output_ctx.streams()[muxer.output_stream_index as usize].time_base;
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

fn select_subtitle_codec_id(
    format: OcrFormat,
    is_mp4: bool,
    is_mkv: bool,
) -> ffi::AVCodecID {
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

fn set_subtitle_codec_par(decode_context: &mut AVCodecContext, encode_context: &mut AVCodecContext) {
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
    encoded_packet.set_duration(packet.duration);
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

fn ocr_single_stream(
    input_path: &str,
    stream_index: i32,
    language: &str,
    work_dir: &Path,
    ocr_format: OcrFormat,
    video_dimensions: Option<(u32, u32)>,
    engine: &mut dyn SubtitleConverter,
) -> Result<Vec<SubtitleCue>> {
    let input_cstr = CString::new(input_path).context("input path has interior NUL")?;
    let mut ictx = AVFormatContextInput::open(input_cstr.as_c_str())?;

    let (stream_time_base, stream_codec_id) = ictx
        .streams()
        .into_iter()
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
            engine,
        )?;
        cues.append(&mut new_cues);
        packet_seq += 1;
    }

    sanitize_cues(&mut cues, ocr_format);

    Ok(cues)
}

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
    engine: &mut dyn SubtitleConverter,
) -> Result<Vec<SubtitleCue>> {
    if subtitle.is_null() {
        return Ok(Vec::new());
    }

    let start_ms;
    let mut end_ms;
    let sub = unsafe { &*subtitle };

    let base_ms = if sub.pts != ffi::AV_NOPTS_VALUE {
        sub.pts / 1000
    } else {
        fallback_start_ms
    };

    start_ms = base_ms.saturating_add(sub.start_display_time as i64).max(0);
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
                .into_iter()
                .map(|line| line.text)
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
                    let ass_text =
                        format_ass_text_with_style(&line.text, Some((pos_x, pos_y)), line.color, line.italic);
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

        let output = engine.extract_lines(&pgm_path, language)?;
        let _ = fs::remove_file(&pgm_path);
        for mut line in output.lines {
            if let Some(bbox) = line.bbox.as_mut() {
                offset_bbox(bbox, rect.x, rect.y);
            }
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

fn run_tesseract(image_path: &Path, language: &str) -> Result<String> {
    let output = Command::new("tesseract")
        .arg(image_path)
        .arg("stdout")
        .arg("-l")
        .arg(language)
        .arg("--psm")
        .arg("6")
        .output()
        .with_context(|| format!("running tesseract on '{}'", image_path.display()))?;

    if !output.status.success() {
        bail!(
            "tesseract OCR failed for '{}': {}",
            image_path.display(),
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

    let mut out = Vec::with_capacity(width * height + 64);
    out.extend_from_slice(format!("P5\n{} {}\n255\n", width, height).as_bytes());

    let mut has_visible_pixels = false;

    for y in 0..height {
        let row = &pixels[y * stride..(y * stride + width)];
        for &idx in row {
            let value = if let Some(pal) = palette {
                let base = (idx as usize) * 4;
                let p0 = pal[base] as i32;
                let p1 = pal[base + 1] as i32;
                let p2 = pal[base + 2] as i32;
                let p3 = pal[base + 3] as i32;

                // Treat the strongest channel as alpha-like visibility signal and produce
                // a high-contrast mask for OCR.
                let alpha = p0.max(p1).max(p2).max(p3);
                if alpha > 24 {
                    has_visible_pixels = true;
                    255u8
                } else {
                    0u8
                }
            } else if idx > 0 {
                has_visible_pixels = true;
                255u8
            } else {
                0u8
            };
            out.push(value);
        }
    }

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

fn write_ass(path: &Path, cues: &[SubtitleCue], video_dimensions: Option<(u32, u32)>) -> Result<()> {
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
    body.push_str("Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text\n");

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

fn rgb_distance(a: (u8, u8, u8), b: (u8, u8, u8)) -> f32 {
    let dr = a.0 as f32 - b.0 as f32;
    let dg = a.1 as f32 - b.1 as f32;
    let db = a.2 as f32 - b.2 as f32;
    (dr * dr + dg * dg + db * db).sqrt()
}

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
    if matches!(ocr_engine, OcrEngine::PpOcrV4 | OcrEngine::External) {
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
    use tempfile::TempDir;
    use strsim::jaro_winkler;

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
            .into_iter()
            .map(|s| s.to_string())
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
            resolve_ocr_language(
                Some("jpn"),
                None,
                None,
                &available,
                OcrEngine::PpOcrV4
            ),
            "jpn"
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
        download_model_with_values(&path, &format!("{}/model.onnx", server.url()), &hash, "model.onnx")
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
        init_ort_environment();
        assert!(ORT_ENV_INIT.get().is_some(), "ORT environment not initialized");
    }

    #[test]
    fn test_text_similarity_wer_like_threshold() {
        let expected = "THIS OCR QUALITY TEST USES MANY WORDS TO ALLOW SMALL ERRORS WITHOUT FAILING STRICT THRESHOLDS IN CI RUNS TODAY ALWAYS FOR STABILITY CHECKS EACH TIME";
        let actual = "THIS OCR QUALITY TEST USES MANY WORDS TO ALLOW SMALL ERRORS WITHOUT FAILING STRICT THRESHOLDS IN CI RUNS TODAY ALWAYS FOR STABIL1TY CHECKS EACH TIME";
        let wer = word_error_rate(expected, actual);
        let similarity = 1.0 - wer;
        assert!(
            similarity > 0.95,
            "WER similarity too low: {} ({} vs {})",
            similarity,
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

        let mut engine = match PpOcrV4Engine::new(&model_dir) {
            Ok(engine) => engine,
            Err(err) => {
                eprintln!("OCR engine unavailable: {err}. Skipping.");
                return;
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
            let wer = word_error_rate(expected_text, actual_text);
            let similarity = 1.0 - wer;
            assert!(
                similarity > 0.95,
                "Text similarity too low for {:?}: {}",
                path,
                similarity
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
}
