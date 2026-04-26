//! OCR engine selection and bootstrap.
//!
//! Resolves `auto` behavior, applies GPU policy gates, and returns initialized
//! converter instances for worker execution.

use anyhow::{anyhow, Result};
use log::{info, warn};
use std::env;
use std::sync::atomic::Ordering;

use super::{
    init_ort_environment, init_ppocr_engine, prefer_ppocr_v3_for_legacy_nvidia, require_gpu,
    resolve_model_dir, ExternalEngine, OcrEngine, PpOcrVariant, SubtitleConverter, TesseractEngine,
    DISABLE_TESS_FALLBACK_LOGGED,
};

/// Selects the effective OCR engine and initializes the first converter instance.
///
/// `Auto` mode tries PP-OCR first when a GPU execution provider is available, with
/// a deterministic fallback to Tesseract unless `DPN_OCR_REQUIRE_GPU=1` is set.
pub(in crate::subtitle_ocr) fn build_ocr_engine(
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

/// Creates a concrete OCR converter for a resolved engine value.
///
/// This function must never receive `Auto`; callers are responsible for resolution.
pub(in crate::subtitle_ocr) fn create_ocr_engine(
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

/// Returns automatic engine preference based on GPU capability and legacy-NVIDIA policy.
pub(in crate::subtitle_ocr) fn auto_engine_preference(gpu_available: bool) -> OcrEngine {
    auto_engine_preference_with_capability(gpu_available, prefer_ppocr_v3_for_legacy_nvidia())
}

/// Chooses the PP-OCR family once GPU support is known.
pub(in crate::subtitle_ocr) fn auto_engine_preference_with_capability(
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

/// Checks if PP-OCR quality fallback to Tesseract is disabled by environment policy.
pub(in crate::subtitle_ocr) fn disable_tesseract_quality_fallback() -> bool {
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
