//! Runtime initialization for PP-OCR sessions.
//!
//! Initializes ORT environment state, builds OcrLite instances per recognizer
//! profile, and manages per-thread CUDA device affinity for workers.

use anyhow::{anyhow, Result};
use log::{debug, info, warn};
use ort::session::builder::SessionBuilder;
use paddle_ocr_rs::ocr_lite::OcrLite;
use std::path::{Path, PathBuf};
use std::sync::atomic::Ordering;

use super::{
    build_execution_providers, ensure_ppocr_models, force_cpu_execution_providers,
    resolve_optional_cjk_rec_model, resolve_optional_japanese_rec_model,
    resolve_optional_korean_rec_model, resolve_optional_latin_rec_model,
    resolve_optional_multilingual_rec_model, skip_ppocr_cls, PpOcrEngine, PpOcrVariant,
    FORCE_CPU_EP, ORT_ENV_GPU_AVAILABLE, ORT_ENV_INIT,
};

/// Builds a clear GPU-required error with the original initialization failure attached.
pub(in crate::subtitle_ocr) fn ppocr_require_gpu_error(
    variant: PpOcrVariant,
    err: &anyhow::Error,
) -> anyhow::Error {
    anyhow!(
        "{} failed to initialize with DPN_OCR_REQUIRE_GPU=1. \
         Verify CUDA/ONNX Runtime GPU libraries are installed. Underlying error: {:#} (debug: {:?})",
        variant.label(),
        err,
        err
    )
}

/// Initializes PP-OCR models and retries with CPU-only providers when policy allows.
pub(in crate::subtitle_ocr) fn init_ppocr_engine(
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

/// Initializes ONNX Runtime once and caches GPU-availability for worker planning.
pub(in crate::subtitle_ocr) fn init_ort_environment() -> Result<bool> {
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

/// Stores data for OcrCudaDeviceGuard.
pub(in crate::subtitle_ocr) struct OcrCudaDeviceGuard {
    previous: Option<i32>,
}

/// Implements behavior for `OcrCudaDeviceGuard`.
impl Drop for OcrCudaDeviceGuard {
    /// Executes the drop routine.
    fn drop(&mut self) {
        OCR_CUDA_DEVICE_ID.with(|slot| slot.set(self.previous));
    }
}

/// Executes the set thread ocr cuda device routine.
pub(in crate::subtitle_ocr) fn set_thread_ocr_cuda_device(
    device_id: Option<i32>,
) -> OcrCudaDeviceGuard {
    let previous = OCR_CUDA_DEVICE_ID.with(|slot| {
        let prev = slot.get();
        slot.set(device_id);
        prev
    });
    OcrCudaDeviceGuard { previous }
}

/// Executes the thread ocr cuda device routine.
pub(in crate::subtitle_ocr) fn thread_ocr_cuda_device() -> Option<i32> {
    OCR_CUDA_DEVICE_ID.with(|slot| slot.get())
}

/// Implements behavior for `PpOcrEngine`.
impl PpOcrEngine {
    /// Executes the new routine.
    pub(in crate::subtitle_ocr) fn new(
        model_dir: &Path,
        variant: PpOcrVariant,
        skip_cls: bool,
    ) -> Result<Self> {
        let models = ensure_ppocr_models(model_dir, variant, skip_cls)?;
        let latin_rec = resolve_optional_latin_rec_model(model_dir, variant)?;
        let multilingual_rec = resolve_optional_multilingual_rec_model(model_dir, variant)?;
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

        let multilingual_ocr = init_optional_rec_profile(
            variant,
            "multilingual",
            &models.det,
            &models.cls,
            multilingual_rec,
        );
        let japanese_ocr =
            init_optional_rec_profile(variant, "japanese", &models.det, &models.cls, japanese_rec);
        let korean_ocr =
            init_optional_rec_profile(variant, "korean", &models.det, &models.cls, korean_rec);
        let cjk_ocr = init_optional_rec_profile(variant, "cjk", &models.det, &models.cls, cjk_rec);

        Ok(Self {
            english_ocr,
            latin_ocr,
            multilingual_ocr,
            japanese_ocr,
            korean_ocr,
            cjk_ocr,
            variant,
        })
    }
}

/// Initializes an optional recognizer profile and falls back cleanly when unavailable.
pub(in crate::subtitle_ocr) fn init_optional_rec_profile(
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

/// Initializes one OcrLite instance for a detector/classifier/recognizer triplet.
pub(in crate::subtitle_ocr) fn init_ocr_lite(
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

/// Applies execution provider selection and thread pool sizing to an ORT session builder.
pub(in crate::subtitle_ocr) fn configure_ort_builder(
    builder: SessionBuilder,
) -> Result<SessionBuilder, ort::Error> {
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
