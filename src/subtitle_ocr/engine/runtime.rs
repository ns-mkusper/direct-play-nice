//! Runtime initialization for PP-OCR sessions.
//!
//! Initializes ORT environment state, builds OcrLite instances per recognizer
//! profile, and manages per-thread CUDA device affinity for workers.

use anyhow::{anyhow, bail, Context, Result};
use log::{debug, info, warn};
use ort::session::builder::SessionBuilder;
use paddle_ocr_rs::ocr_lite::OcrLite;
use std::path::{Path, PathBuf};
use std::sync::atomic::Ordering;

use super::model_metadata::strip_character_metadata_lf;
use super::models::PpOcrModels;
use super::whole_line::{
    bitmap_line_policy_from_env, read_audited_model_for_profile, read_pinned_model,
    BitmapLinePolicy, ExperimentalLineRecognizer, ProfileLineRecognizers, WholeLineRecognizer,
};
use super::{
    build_execution_providers, ensure_ppocr_models, force_cpu_execution_providers,
    resolve_optional_cjk_rec_model, resolve_optional_japanese_rec_model,
    resolve_optional_korean_rec_model, resolve_optional_latin_rec_model,
    resolve_optional_multilingual_rec_model, skip_ppocr_cls, OcrRecProfile, PpOcrEngine,
    PpOcrVariant, FORCE_CPU_EP, ORT_ENV_GPU_AVAILABLE, ORT_ENV_INIT,
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
    // Configuration and model-policy errors must not be mistaken for GPU failures.
    let line_policy = bitmap_line_policy_from_env()?;
    let skip_cls = skip_ppocr_cls(variant, require_gpu);
    if line_policy == BitmapLinePolicy::Required {
        let models = ensure_ppocr_models(model_dir, variant, skip_cls)?;
        audited_line_models(
            line_policy,
            variant,
            &models,
            OcrRecProfile::English,
            &models.rec,
        )?;
        let latin = resolve_optional_latin_rec_model(model_dir, variant)?
            .context("bitmap-line OCR requires an audited Latin recognizer")?;
        audited_line_models(line_policy, variant, &models, OcrRecProfile::Latin, &latin)?;
    }
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

pub(in crate::subtitle_ocr) struct OcrCudaDeviceGuard {
    previous: Option<i32>,
}

impl Drop for OcrCudaDeviceGuard {
    fn drop(&mut self) {
        OCR_CUDA_DEVICE_ID.with(|slot| slot.set(self.previous));
    }
}

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

pub(in crate::subtitle_ocr) fn thread_ocr_cuda_device() -> Option<i32> {
    OCR_CUDA_DEVICE_ID.with(|slot| slot.get())
}

impl PpOcrEngine {
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
        let policy = bitmap_line_policy_from_env()?;
        let line_models = audited_line_models(
            policy,
            variant,
            &models,
            OcrRecProfile::English,
            &models.rec,
        )?;
        let mut latin_line_models = match latin_rec.as_deref() {
            Some(path) => {
                audited_line_models(policy, variant, &models, OcrRecProfile::Latin, path)?
            }
            None if policy == BitmapLinePolicy::Required => {
                bail!("bitmap-line OCR requires an audited Latin recognizer")
            }
            None => None,
        };
        let english_ocr = match &line_models {
            Some(line_models) => line_models.init_ocr(variant)?,
            None => init_ocr_lite(variant, "english", &models.det, &models.cls, &models.rec)?,
        };

        let latin_ocr = if let Some(latin_rec_path) = latin_rec {
            info!(
                "Initializing {} latin rec model at '{}'",
                variant.label(),
                latin_rec_path.display()
            );
            let result = match &latin_line_models {
                Some(audited) => match audited.init_ocr(variant) {
                    Ok(ocr) => Ok(ocr),
                    Err(err) if policy == BitmapLinePolicy::Required => Err(err),
                    Err(err) => {
                        warn!(
                            "Latin bitmap-line preflight failed; retaining detector path: {err:#}"
                        );
                        latin_line_models = None;
                        init_ocr_lite(variant, "latin", &models.det, &models.cls, &latin_rec_path)
                    }
                },
                None => init_ocr_lite(variant, "latin", &models.det, &models.cls, &latin_rec_path),
            };
            match result {
                Ok(ocr) => Some(ocr),
                Err(err) if policy == BitmapLinePolicy::Required => return Err(err),
                Err(err) => {
                    latin_line_models = None;
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

        let experimental_lines = ProfileLineRecognizers {
            english: line_models.map(|models| ExperimentalLineRecognizer::new(models.rec)),
            latin: latin_line_models.map(|models| ExperimentalLineRecognizer::new(models.rec)),
        };
        Ok(Self {
            english_ocr,
            latin_ocr,
            multilingual_ocr,
            japanese_ocr,
            korean_ocr,
            cjk_ocr,
            experimental_lines,
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

// Independently pinned to the exact files audited for ONNX external data.
const AUDITED_V3_DET: &str = "3439588c030faea393a54515f51e983d8e155b19a2e8aba7891934c1cf0de526";
const AUDITED_V3_CLS: &str = "70581b300b83babd9e0dd1d7d74c5b006869e8796da277a70c2e405bf9d77c82";
const AUDITED_V4_DET: &str = "d2a7720d45a54257208b1e13e36a8479894cb74155a5efe29462512d42f49da9";
const AUDITED_V4_CLS: &str = "e47acedf663230f8863ff1ab0e64dd2d82b838fceb5957146dab185a89d6215c";

/// Audited model bytes for the in-memory English recognizer and bitmap-line OCR.
struct AuditedLineModels {
    det: Vec<u8>,
    cls: Vec<u8>,
    rec: Vec<u8>,
}

impl AuditedLineModels {
    fn init_ocr(&self, variant: PpOcrVariant) -> Result<OcrLite> {
        // Validate the corrected class width, then release the preflight session
        // before loading the detector; direct-line recognition initializes lazily.
        drop(WholeLineRecognizer::preflight_audited_model(&self.rec)?);
        let corrected = strip_character_metadata_lf(&self.rec)?;
        init_ocr_lite_from_memory(variant, &self.det, &self.cls, &corrected)
    }
}

/// Loads the audited model bytes required by `policy`.
///
/// `Required` fails on any unaudited model. `Auto` quietly keeps the
/// path-loaded detector pipeline for custom models; only the exact detector and
/// classifier bytes audited for external ONNX data are loaded from memory.
fn audited_line_models(
    policy: BitmapLinePolicy,
    variant: PpOcrVariant,
    models: &PpOcrModels,
    profile: OcrRecProfile,
    rec_path: &Path,
) -> Result<Option<AuditedLineModels>> {
    if policy == BitmapLinePolicy::Off {
        return Ok(None);
    }
    let explicit = policy == BitmapLinePolicy::Required;
    let rec = match read_audited_model_for_profile(rec_path, profile) {
        Ok(rec) => rec,
        Err(err) if explicit => return Err(err),
        Err(err) => {
            debug!(
                "Bitmap-line OCR unavailable for this recognizer; retaining detector path: {err:#}"
            );
            return Ok(None);
        }
    };
    let (det_hash, cls_hash) = match variant {
        PpOcrVariant::V3 => (AUDITED_V3_DET, AUDITED_V3_CLS),
        PpOcrVariant::V4 => (AUDITED_V4_DET, AUDITED_V4_CLS),
    };
    let aux = read_pinned_model(&models.det, &[det_hash])
        .context("detector model")
        .and_then(|det| {
            let cls = read_pinned_model(&models.cls, &[cls_hash]).context("classifier model")?;
            Ok((det, cls))
        });
    match aux {
        Ok((det, cls)) => Ok(Some(AuditedLineModels { det, cls, rec })),
        Err(err) if explicit => Err(err),
        Err(err) => {
            warn!("Bitmap-line and corrected-dictionary OCR unavailable for custom detector/classifier models; using the existing detector path: {err:#}");
            Ok(None)
        }
    }
}

fn init_ocr_lite_from_memory(
    variant: PpOcrVariant,
    det_bytes: &[u8],
    cls_bytes: &[u8],
    rec_bytes: &[u8],
) -> Result<OcrLite> {
    let mut ocr = OcrLite::new();
    ocr.init_models_from_memory_custom(det_bytes, cls_bytes, rec_bytes, configure_ort_builder)
        .map_err(|err| {
            anyhow!(
                "failed to initialize {} experimental models from memory: {}",
                variant.label(),
                err
            )
        })?;
    Ok(ocr)
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

#[cfg(test)]
mod audited_aux_tests {
    use super::*;

    #[test]
    fn pins_match_provisioned_specs_and_unrecognized_bytes_are_rejected() {
        for (variant, det, cls) in [
            (PpOcrVariant::V3, AUDITED_V3_DET, AUDITED_V3_CLS),
            (PpOcrVariant::V4, AUDITED_V4_DET, AUDITED_V4_CLS),
        ] {
            let (det_spec, cls_spec, _) = variant.model_specs();
            assert_eq!(det, det_spec.sha256.to_ascii_lowercase());
            assert_eq!(cls, cls_spec.sha256.to_ascii_lowercase());
            let file = tempfile::NamedTempFile::new().unwrap();
            let models = PpOcrModels {
                det: file.path().to_path_buf(),
                cls: file.path().to_path_buf(),
                rec: file.path().to_path_buf(),
            };
            assert!(audited_line_models(
                BitmapLinePolicy::Required,
                variant,
                &models,
                OcrRecProfile::English,
                &models.rec
            )
            .is_err());
            for policy in [BitmapLinePolicy::Auto, BitmapLinePolicy::Off] {
                assert!(audited_line_models(
                    policy,
                    variant,
                    &models,
                    OcrRecProfile::English,
                    &models.rec
                )
                .unwrap()
                .is_none());
            }
        }
    }
}
