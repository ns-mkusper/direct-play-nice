//! Opt-in AI super-resolution for sources that sit below the active resolution cap.
//!
//! This path is separate from the deterministic libswscale/CUDA resize kernels.
//! It runs an ONNX single-image super-resolution model on every decoded frame,
//! then hands the enlarged RGB float frame to the normal software resize step,
//! which converts it to YUV420P at the exact target dimensions. The model only
//! ever enlarges; the final fit to the cap stays deterministic.

use std::env;
use std::path::PathBuf;
use std::time::Instant;

use anyhow::{anyhow, bail, Context, Result};
use clap::ValueEnum;
use log::{debug, info, warn};
use ort::execution_providers::{CPUExecutionProvider, ExecutionProviderDispatch};
use ort::session::builder::GraphOptimizationLevel;
use ort::session::Session;
use ort::value::Tensor;
use rsmpeg::avutil::AVFrame;
use rsmpeg::ffi;
use rsmpeg::swscale::SwsContext;
use serde::Deserialize;

use crate::subtitle_ocr::{ensure_model_file, resolve_model_dir, ModelSpec};

/// Built-in model choices. `Custom` loads `--ai-upscale-model-path`.
#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum, Deserialize, Default)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum AiUpscaleModel {
    /// Deterministic resize only (default).
    #[default]
    Off,
    /// Real-ESRGAN `realesr-animevideov3` (SRVGGNetCompact, 4x, BSD-3-Clause). Fastest built-in; tuned for anime and cartoons.
    #[value(name = "realesr-animevideov3", alias = "anime")]
    #[serde(rename = "realesr-animevideov3", alias = "anime")]
    RealesrAnimevideov3,
    /// Real-ESRGAN `realesr-general-x4v3` (SRVGGNetCompact, 4x, BSD-3-Clause). Twice the depth; tuned for live action and photos.
    #[value(name = "realesr-general-x4v3", alias = "general")]
    #[serde(rename = "realesr-general-x4v3", alias = "general")]
    RealesrGeneralX4v3,
    /// Any single-image ONNX model with one float NCHW RGB input in [0,1] and one output; the scale factor is probed at load time.
    Custom,
}

impl AiUpscaleModel {
    fn spec(self) -> Option<ModelSpec> {
        match self {
            AiUpscaleModel::Off | AiUpscaleModel::Custom => None,
            AiUpscaleModel::RealesrAnimevideov3 => Some(REALESR_ANIMEVIDEOV3),
            AiUpscaleModel::RealesrGeneralX4v3 => Some(REALESR_GENERAL_X4V3),
        }
    }

    pub(crate) fn label(self) -> &'static str {
        match self {
            AiUpscaleModel::Off => "off",
            AiUpscaleModel::RealesrAnimevideov3 => "realesr-animevideov3",
            AiUpscaleModel::RealesrGeneralX4v3 => "realesr-general-x4v3",
            AiUpscaleModel::Custom => "custom",
        }
    }
}

impl std::fmt::Display for AiUpscaleModel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.label())
    }
}

/// Where the model runs. There is no silent CPU fallback: `auto` fails when no
/// GPU provider is usable and tells the user how to opt into CPU.
#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum, Deserialize, Default)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum AiUpscaleDevice {
    /// Use a GPU execution provider; fail if none is available.
    #[default]
    Auto,
    /// Require the ONNX Runtime CUDA execution provider.
    Cuda,
    /// Run on the CPU. Expect roughly one frame per second at 480p.
    Cpu,
}

impl std::fmt::Display for AiUpscaleDevice {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            AiUpscaleDevice::Auto => "auto",
            AiUpscaleDevice::Cuda => "cuda",
            AiUpscaleDevice::Cpu => "cpu",
        })
    }
}

/// Everything the pipeline needs to decide whether and how to upscale.
#[derive(Clone, Debug)]
pub(crate) struct UpscaleSettings {
    pub(crate) model: AiUpscaleModel,
    pub(crate) model_path: Option<PathBuf>,
    pub(crate) device: AiUpscaleDevice,
    /// Tile edge in source pixels; `0` runs the whole frame in one pass.
    pub(crate) tile: u32,
}

impl UpscaleSettings {
    pub(crate) fn enabled(&self) -> bool {
        !matches!(self.model, AiUpscaleModel::Off)
    }

    pub(crate) fn describe(&self) -> String {
        match (&self.model, &self.model_path) {
            (AiUpscaleModel::Custom, Some(path)) => format!("custom ({})", path.display()),
            (model, _) => model.label().to_string(),
        }
    }
}

const REALESR_ANIMEVIDEOV3: ModelSpec = ModelSpec {
    filename: "realesr-animevideov3.onnx",
    url: "https://huggingface.co/skillsafe-ai/realesr-animevideov3/resolve/main/model.onnx",
    sha256: "78BAA685A1A92CAC6E14AB2AF2A8AD0EF56FA48629563B1414E3A7ACC54D86E2",
};

const REALESR_GENERAL_X4V3: ModelSpec = ModelSpec {
    filename: "realesr-general-x4v3.onnx",
    url: "https://huggingface.co/CoderViking/realesr-general-x4v3-onnx/resolve/main/realesr-general-x4v3.onnx",
    sha256: "1940A93EE08283A0A7286183186357B1688FE9FA8EDE74604B424586AADDF112",
};

/// Overlap kept around every tile so the model sees context across seams.
const TILE_PAD: u32 = 8;

/// A loaded ONNX super-resolution session plus the facts probed from it.
pub(crate) struct Upscaler {
    session: Session,
    input_name: String,
    output_name: String,
    scale: u32,
    tile: u32,
    provider: &'static str,
    frames: u64,
    inference_seconds: f64,
}

impl Upscaler {
    /// Resolves the model file, builds the session on the requested device, and
    /// runs a tiny probe to learn the scale factor and prove the provider works.
    pub(crate) fn load(settings: &UpscaleSettings) -> Result<Self> {
        let model_path = resolve_model_path(settings)?;
        let (providers, provider) = execution_providers(settings.device)?;
        match ort::init().commit() {
            Ok(_) => {}
            Err(err) => bail!("Failed to initialize ONNX Runtime for AI upscaling: {err}"),
        }
        let session = Session::builder()
            .map_err(|err| anyhow!("creating ONNX session builder: {err}"))?
            .with_execution_providers(providers)
            .map_err(|err| anyhow!("configuring {provider} execution provider: {err}"))?
            .with_optimization_level(GraphOptimizationLevel::Level3)
            .map_err(|err| anyhow!("configuring graph optimization: {err}"))?
            .with_intra_threads(
                std::thread::available_parallelism()
                    .map(|n| n.get())
                    .unwrap_or(2),
            )
            .map_err(|err| anyhow!("configuring ONNX threads: {err}"))?
            .commit_from_file(&model_path)
            .map_err(|err| {
                anyhow!(
                    "loading AI upscale model '{}' on {provider}: {err}",
                    model_path.display()
                )
            })?;
        if session.inputs.len() != 1 || session.outputs.len() != 1 {
            bail!(
                "AI upscale model '{}' must have exactly one input and one output (found {} / {})",
                model_path.display(),
                session.inputs.len(),
                session.outputs.len()
            );
        }
        let input_name = session.inputs[0].name.clone();
        let output_name = session.outputs[0].name.clone();
        let mut upscaler = Self {
            session,
            input_name,
            output_name,
            scale: 1,
            tile: settings.tile,
            provider,
            frames: 0,
            inference_seconds: 0.0,
        };
        upscaler.scale = upscaler.probe_scale()?;
        info!(
            "AI upscale: model {} ({}), scale {}x, provider {}, tile {}",
            settings.describe(),
            model_path.display(),
            upscaler.scale,
            provider,
            if settings.tile == 0 {
                "whole frame".to_string()
            } else {
                format!("{}px", settings.tile)
            }
        );
        Ok(upscaler)
    }

    /// Runs a 16x16 frame through the model and derives the integer scale.
    fn probe_scale(&mut self) -> Result<u32> {
        let (out_w, out_h, _) = self.run_rgb(&vec![0.5f32; 3 * 16 * 16], 16, 16)?;
        if out_w % 16 != 0 || out_h % 16 != 0 || out_w / 16 != out_h / 16 || out_w / 16 < 2 {
            bail!(
                "AI upscale model produced {}x{} for a 16x16 probe; expected an integer enlargement of at least 2x",
                out_w,
                out_h
            );
        }
        Ok(out_w / 16)
    }

    /// Runs one planar RGB float image through the session.
    /// Returns `(width, height, nchw_rgb)` of the model output.
    fn run_rgb(&mut self, rgb: &[f32], width: u32, height: u32) -> Result<(u32, u32, Vec<f32>)> {
        let tensor =
            Tensor::from_array(([1usize, 3, height as usize, width as usize], rgb.to_vec()))
                .map_err(|err| anyhow!("building AI upscale input tensor: {err}"))?;
        let started = Instant::now();
        let outputs = self
            .session
            .run(ort::inputs![self.input_name.as_str() => tensor])
            .map_err(|err| anyhow!("running AI upscale model on {}: {err}", self.provider))?;
        self.inference_seconds += started.elapsed().as_secs_f64();
        let value = outputs
            .get(self.output_name.as_str())
            .ok_or_else(|| anyhow!("AI upscale output '{}' missing", self.output_name))?;
        let (shape, data) = value
            .try_extract_tensor::<f32>()
            .map_err(|err| anyhow!("reading AI upscale output: {err}"))?;
        let dims: Vec<i64> = shape.iter().copied().collect();
        if dims.len() != 4 || dims[0] != 1 || dims[1] != 3 {
            bail!(
                "AI upscale output has shape {:?}; expected [1, 3, height, width]",
                dims
            );
        }
        Ok((dims[3] as u32, dims[2] as u32, data.to_vec()))
    }

    /// Enlarges a software frame by the model's scale factor. The result is a
    /// planar float GBR frame (`AV_PIX_FMT_GBRPF32LE`) the resize step can
    /// convert and fit to the target dimensions.
    pub(crate) fn upscale_frame(&mut self, frame: &AVFrame) -> Result<AVFrame> {
        let width = frame.width as u32;
        let height = frame.height as u32;
        let rgb = frame_to_rgb_planar(frame)?;
        let (out_w, out_h, out) = if self.tile > 0 && (width > self.tile || height > self.tile) {
            self.run_tiled(&rgb, width, height)?
        } else {
            let (w, h, out) = self.run_rgb(&rgb, width, height)?;
            if w != width * self.scale || h != height * self.scale {
                bail!(
                    "AI upscale model returned {}x{} for a {}x{} frame; expected {}x{}",
                    w,
                    h,
                    width,
                    height,
                    width * self.scale,
                    height * self.scale
                );
            }
            (w, h, out)
        };
        self.frames += 1;
        rgb_planar_to_frame(&out, out_w, out_h, frame)
    }

    /// Splits the frame into overlapping tiles so large sources fit GPU memory.
    fn run_tiled(&mut self, rgb: &[f32], width: u32, height: u32) -> Result<(u32, u32, Vec<f32>)> {
        let scale = self.scale;
        let out_w = width * scale;
        let out_h = height * scale;
        let mut out = vec![0f32; (3 * out_w * out_h) as usize];
        let tile = self.tile.max(32);
        let mut y0 = 0;
        while y0 < height {
            let y1 = (y0 + tile).min(height);
            let py0 = y0.saturating_sub(TILE_PAD);
            let py1 = (y1 + TILE_PAD).min(height);
            let mut x0 = 0;
            while x0 < width {
                let x1 = (x0 + tile).min(width);
                let px0 = x0.saturating_sub(TILE_PAD);
                let px1 = (x1 + TILE_PAD).min(width);
                let tw = px1 - px0;
                let th = py1 - py0;
                let mut patch = vec![0f32; (3 * tw * th) as usize];
                for c in 0..3 {
                    for y in 0..th {
                        let src = ((c * height + (py0 + y)) * width + px0) as usize;
                        let dst = ((c * th + y) * tw) as usize;
                        patch[dst..dst + tw as usize].copy_from_slice(&rgb[src..src + tw as usize]);
                    }
                }
                let (ow, oh, up) = self.run_rgb(&patch, tw, th)?;
                if ow != tw * scale || oh != th * scale {
                    bail!(
                        "AI upscale tile returned {}x{} for {}x{}; expected {}x{}",
                        ow,
                        oh,
                        tw,
                        th,
                        tw * scale,
                        th * scale
                    );
                }
                // Copy only the unpadded centre of the tile into the output.
                let inner_x = (x0 - px0) * scale;
                let inner_y = (y0 - py0) * scale;
                let inner_w = (x1 - x0) * scale;
                let inner_h = (y1 - y0) * scale;
                for c in 0..3 {
                    for y in 0..inner_h {
                        let src = ((c * oh + inner_y + y) * ow + inner_x) as usize;
                        let dst = ((c * out_h + y0 * scale + y) * out_w + x0 * scale) as usize;
                        out[dst..dst + inner_w as usize]
                            .copy_from_slice(&up[src..src + inner_w as usize]);
                    }
                }
                x0 = x1;
            }
            y0 = y1;
        }
        Ok((out_w, out_h, out))
    }

    fn log_summary(&self) {
        if self.frames == 0 {
            return;
        }
        let fps = self.frames as f64 / self.inference_seconds.max(1e-9);
        info!(
            "AI upscale: {} frames, {:.1} ms/frame model time ({:.1} fps) on {}",
            self.frames,
            1000.0 * self.inference_seconds / self.frames as f64,
            fps,
            self.provider
        );
    }
}

impl Drop for Upscaler {
    fn drop(&mut self) {
        self.log_summary();
    }
}

fn resolve_model_path(settings: &UpscaleSettings) -> Result<PathBuf> {
    match settings.model {
        AiUpscaleModel::Off => bail!("AI upscale model is off"),
        AiUpscaleModel::Custom => {
            let path = settings.model_path.clone().ok_or_else(|| {
                anyhow!("--ai-upscale-model custom requires --ai-upscale-model-path <FILE.onnx>")
            })?;
            if !path.is_file() {
                bail!("AI upscale model file '{}' does not exist", path.display());
            }
            Ok(path)
        }
        model => {
            let spec = model
                .spec()
                .ok_or_else(|| anyhow!("no download spec for {}", model))?;
            let dir = model_dir()?;
            ensure_model_file(&dir, &spec)
        }
    }
}

/// Models live beside the OCR models unless `DPN_UPSCALE_MODEL_DIR` overrides.
fn model_dir() -> Result<PathBuf> {
    if let Some(dir) = env::var_os("DPN_UPSCALE_MODEL_DIR") {
        let path = PathBuf::from(dir);
        std::fs::create_dir_all(&path)
            .with_context(|| format!("creating AI upscale model directory '{}'", path.display()))?;
        return Ok(path);
    }
    resolve_model_dir()
}

fn execution_providers(
    device: AiUpscaleDevice,
) -> Result<(Vec<ExecutionProviderDispatch>, &'static str)> {
    match device {
        AiUpscaleDevice::Cpu => Ok((vec![CPUExecutionProvider::default().build()], "cpu")),
        AiUpscaleDevice::Cuda | AiUpscaleDevice::Auto => {
            if let Some(provider) = cuda_provider()? {
                return Ok((vec![provider], "cuda"));
            }
            if let Some((provider, name)) = platform_gpu_provider() {
                if matches!(device, AiUpscaleDevice::Cuda) {
                    bail!(
                        "--ai-upscale-device cuda requested but the ONNX Runtime CUDA execution provider is not available (found {name} instead)"
                    );
                }
                return Ok((vec![provider], name));
            }
            bail!(
                "AI upscaling needs a GPU execution provider and none is available. \
                 Install the ONNX Runtime CUDA provider plus matching CUDA/cuDNN runtime libraries, \
                 or pass --ai-upscale-device cpu to accept roughly one frame per second."
            )
        }
    }
}

#[cfg(any(target_os = "linux", target_os = "windows"))]
fn cuda_provider() -> Result<Option<ExecutionProviderDispatch>> {
    use ort::execution_providers::cuda::CUDAExecutionProvider;
    use ort::execution_providers::ExecutionProvider;

    let mut ep = CUDAExecutionProvider::default();
    match ep.is_available() {
        Ok(true) => {}
        Ok(false) => {
            debug!("ONNX Runtime CUDA execution provider is not available for AI upscaling");
            return Ok(None);
        }
        Err(err) => {
            warn!("Failed to query the CUDA execution provider: {err}");
            return Ok(None);
        }
    }
    if let Some(id) = env::var("DPN_UPSCALE_CUDA_DEVICE")
        .ok()
        .and_then(|raw| raw.trim().parse::<i32>().ok())
    {
        ep = ep.with_device_id(id);
    }
    Ok(Some(ep.build().error_on_failure()))
}

#[cfg(not(any(target_os = "linux", target_os = "windows")))]
fn cuda_provider() -> Result<Option<ExecutionProviderDispatch>> {
    Ok(None)
}

#[cfg(target_os = "windows")]
fn platform_gpu_provider() -> Option<(ExecutionProviderDispatch, &'static str)> {
    use ort::execution_providers::DirectMLExecutionProvider;
    use ort::execution_providers::ExecutionProvider;
    let ep = DirectMLExecutionProvider::default();
    matches!(ep.is_available(), Ok(true)).then(|| (ep.build().error_on_failure(), "directml"))
}

#[cfg(target_vendor = "apple")]
fn platform_gpu_provider() -> Option<(ExecutionProviderDispatch, &'static str)> {
    use ort::execution_providers::CoreMLExecutionProvider;
    use ort::execution_providers::ExecutionProvider;
    let ep = CoreMLExecutionProvider::default();
    matches!(ep.is_available(), Ok(true)).then(|| (ep.build().error_on_failure(), "coreml"))
}

#[cfg(not(any(target_os = "windows", target_vendor = "apple")))]
fn platform_gpu_provider() -> Option<(ExecutionProviderDispatch, &'static str)> {
    None
}

/// Converts any software frame to planar RGB float in [0,1], NCHW order.
fn frame_to_rgb_planar(frame: &AVFrame) -> Result<Vec<f32>> {
    let width = frame.width;
    let height = frame.height;
    if frame.format == ffi::AV_PIX_FMT_GBRPF32LE {
        return Ok(read_gbrpf32_planes(frame));
    }
    let mut gbr = AVFrame::new();
    gbr.set_width(width);
    gbr.set_height(height);
    gbr.set_format(ffi::AV_PIX_FMT_GBRPF32LE);
    gbr.alloc_buffer()
        .context("allocating float RGB frame for AI upscale")?;
    let mut sws = SwsContext::get_context(
        width,
        height,
        frame.format as ffi::AVPixelFormat,
        width,
        height,
        ffi::AV_PIX_FMT_GBRPF32LE,
        ffi::SWS_POINT,
        None,
        None,
        None,
    )
    .context("creating swscale context for AI upscale input")?;
    sws.scale_frame(frame, 0, height, &mut gbr)
        .context("converting frame to float RGB for AI upscale")?;
    Ok(read_gbrpf32_planes(&gbr))
}

/// Reads a `GBRPF32LE` frame into NCHW RGB order (plane 0 = G, 1 = B, 2 = R).
fn read_gbrpf32_planes(gbr: &AVFrame) -> Vec<f32> {
    let w = gbr.width as usize;
    let h = gbr.height as usize;
    let mut rgb = vec![0f32; 3 * w * h];
    for (channel, plane) in [(0usize, 2usize), (1, 0), (2, 1)] {
        let base = gbr.data[plane] as *const f32;
        let stride = gbr.linesize[plane] as usize / std::mem::size_of::<f32>();
        for y in 0..h {
            let row = unsafe { std::slice::from_raw_parts(base.add(y * stride), w) };
            rgb[(channel * h + y) * w..(channel * h + y + 1) * w].copy_from_slice(row);
        }
    }
    rgb
}

/// Packs planar RGB float output into a `GBRPF32LE` frame carrying the source's timing.
fn rgb_planar_to_frame(rgb: &[f32], width: u32, height: u32, source: &AVFrame) -> Result<AVFrame> {
    let w = width as usize;
    let h = height as usize;
    if rgb.len() != 3 * w * h {
        bail!(
            "AI upscale output has {} samples; expected {} for {}x{}",
            rgb.len(),
            3 * w * h,
            width,
            height
        );
    }
    let mut out = AVFrame::new();
    out.set_width(width as i32);
    out.set_height(height as i32);
    out.set_format(ffi::AV_PIX_FMT_GBRPF32LE);
    out.alloc_buffer()
        .context("allocating AI upscale output frame")?;
    for (channel, plane) in [(0usize, 2usize), (1, 0), (2, 1)] {
        let base = out.data[plane] as *mut f32;
        let stride = out.linesize[plane] as usize / std::mem::size_of::<f32>();
        for y in 0..h {
            let row = unsafe { std::slice::from_raw_parts_mut(base.add(y * stride), w) };
            for (dst, src) in row
                .iter_mut()
                .zip(&rgb[(channel * h + y) * w..(channel * h + y + 1) * w])
            {
                *dst = src.clamp(0.0, 1.0);
            }
        }
    }
    out.set_pts(source.pts);
    out.set_time_base(source.time_base);
    unsafe {
        (*out.as_mut_ptr()).pkt_dts = source.pkt_dts;
        (*out.as_mut_ptr()).sample_aspect_ratio = source.sample_aspect_ratio;
        (*out.as_mut_ptr()).best_effort_timestamp = source.best_effort_timestamp;
        (*out.as_mut_ptr()).duration = source.duration;
        (*out.as_mut_ptr()).color_range = source.color_range;
        (*out.as_mut_ptr()).color_primaries = source.color_primaries;
        (*out.as_mut_ptr()).color_trc = source.color_trc;
        (*out.as_mut_ptr()).colorspace = source.colorspace;
    }
    Ok(out)
}

/// Fits `source` inside `cap` by enlarging when it is smaller, keeping aspect
/// ratio and even dimensions. Returns the source size when it already meets
/// or exceeds the cap on either axis.
pub(crate) fn upscale_dimensions(
    source_width: i32,
    source_height: i32,
    cap: (u32, u32),
) -> (i32, i32) {
    if source_width <= 0 || source_height <= 0 {
        return (source_width.max(2), source_height.max(2));
    }
    let (cap_w, cap_h) = (cap.0.max(2) as f64, cap.1.max(2) as f64);
    let scale = (cap_w / source_width as f64).min(cap_h / source_height as f64);
    if scale <= 1.0 {
        return (source_width, source_height);
    }
    let mut w = (source_width as f64 * scale).round() as i32;
    let mut h = (source_height as f64 * scale).round() as i32;
    // Sources such as 854x480 are rounded 16:9; snap to the cap when within a
    // couple of pixels so 480p lands on exactly 1920x1080.
    if (cap.0 as i32 - w).abs() <= 2 {
        w = cap.0 as i32;
    }
    if (cap.1 as i32 - h).abs() <= 2 {
        h = cap.1 as i32;
    }
    w -= w % 2;
    h -= h % 2;
    (w.max(2), h.max(2))
}

/// Human-readable reason recorded when AI upscaling forces a conversion.
pub(crate) fn upscale_reason(source: (i32, i32), target: (i32, i32), model: &str) -> String {
    format!(
        "AI upscale ({model}) requested: source {}x{} is below the {}x{} target",
        source.0, source.1, target.0, target.1
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn upscale_dimensions_enlarges_to_fit_cap_with_even_sizes() {
        assert_eq!(upscale_dimensions(854, 480, (1920, 1080)), (1920, 1080));
        assert_eq!(upscale_dimensions(640, 480, (1920, 1080)), (1440, 1080));
        assert_eq!(upscale_dimensions(720, 576, (1920, 1080)), (1350, 1080));
    }

    #[test]
    fn upscale_dimensions_keeps_sources_at_or_above_cap() {
        assert_eq!(upscale_dimensions(1920, 1080, (1920, 1080)), (1920, 1080));
        assert_eq!(upscale_dimensions(3840, 2160, (1920, 1080)), (3840, 2160));
        assert_eq!(upscale_dimensions(1920, 800, (1920, 1080)), (1920, 800));
    }

    #[test]
    fn upscale_settings_describe_custom_path() {
        let settings = UpscaleSettings {
            model: AiUpscaleModel::Custom,
            model_path: Some(PathBuf::from("/models/x.onnx")),
            device: AiUpscaleDevice::Auto,
            tile: 0,
        };
        assert!(settings.enabled());
        assert_eq!(settings.describe(), "custom (/models/x.onnx)");
        let off = UpscaleSettings {
            model: AiUpscaleModel::Off,
            model_path: None,
            device: AiUpscaleDevice::Auto,
            tile: 0,
        };
        assert!(!off.enabled());
    }

    #[test]
    fn yuv420p_frame_converts_to_rgb_in_unit_range() {
        let (w, h) = (16i32, 8i32);
        let mut frame = AVFrame::new();
        frame.set_width(w);
        frame.set_height(h);
        frame.set_format(ffi::AV_PIX_FMT_YUV420P);
        frame.alloc_buffer().unwrap();
        unsafe {
            for y in 0..h as usize {
                let row = frame.data[0].add(y * frame.linesize[0] as usize);
                std::ptr::write_bytes(row, 235, w as usize);
            }
            for plane in 1..3 {
                for y in 0..(h / 2) as usize {
                    let row = frame.data[plane].add(y * frame.linesize[plane] as usize);
                    std::ptr::write_bytes(row, 128, (w / 2) as usize);
                }
            }
        }
        let rgb = frame_to_rgb_planar(&frame).unwrap();
        assert_eq!(rgb.len(), (3 * w * h) as usize);
        for v in &rgb {
            assert!(*v > 0.95 && *v <= 1.0, "white pixel decoded as {v}");
        }
    }

    #[test]
    fn rgb_round_trip_through_frames_preserves_values() {
        let (w, h) = (6u32, 4u32);
        let mut rgb = vec![0f32; (3 * w * h) as usize];
        for (i, v) in rgb.iter_mut().enumerate() {
            *v = (i % 17) as f32 / 16.0;
        }
        let mut source = AVFrame::new();
        source.set_width(w as i32);
        source.set_height(h as i32);
        source.set_format(ffi::AV_PIX_FMT_GBRPF32LE);
        source.alloc_buffer().unwrap();
        let frame = rgb_planar_to_frame(&rgb, w, h, &source).unwrap();
        let back = frame_to_rgb_planar(&frame).unwrap();
        assert_eq!(back.len(), rgb.len());
        for (a, b) in back.iter().zip(&rgb) {
            assert!((a - b).abs() < 1e-6, "{a} != {b}");
        }
    }
}
