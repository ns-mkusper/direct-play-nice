//! Model provisioning and validation for PP-OCR.
//!
//! Resolves model directory paths, downloads expected ONNX assets, and handles
//! optional recognizer profiles (latin/japanese/korean/cjk).

use anyhow::{anyhow, bail, Context, Result};
use log::{debug, info, warn};
use sha2::{Digest, Sha256};
use std::env;
use std::fs;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

use super::PpOcrVariant;

/// Stores data for ModelSpec.
pub(in crate::subtitle_ocr) struct ModelSpec {
    pub(in crate::subtitle_ocr) filename: &'static str,
    pub(in crate::subtitle_ocr) url: &'static str,
    pub(in crate::subtitle_ocr) sha256: &'static str,
}

pub(in crate::subtitle_ocr) const PPOCR_V4_DET_MODEL: ModelSpec = ModelSpec {
    filename: "ch_PP-OCRv4_det_infer.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/v3.7.0/onnx/PP-OCRv4/det/ch_PP-OCRv4_det_infer.onnx",
    sha256: "D2A7720D45A54257208B1E13E36A8479894CB74155A5EFE29462512D42F49DA9",
};
pub(in crate::subtitle_ocr) const PPOCR_V4_CLS_MODEL: ModelSpec = ModelSpec {
    filename: "ch_ppocr_mobile_v2.0_cls_infer.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/v3.7.0/onnx/PP-OCRv4/cls/ch_ppocr_mobile_v2.0_cls_infer.onnx",
    sha256: "E47ACEDF663230F8863FF1AB0E64DD2D82B838FCEB5957146DAB185A89D6215C",
};
pub(in crate::subtitle_ocr) const PPOCR_V4_REC_MODEL: ModelSpec = ModelSpec {
    filename: "en_PP-OCRv4_rec_infer.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/v3.7.0/onnx/PP-OCRv4/rec/en_PP-OCRv4_rec_infer.onnx",
    sha256: "E8770C967605983D1570CDF5352041DFB68FA0C21664F49F47B155ABD3E0E318",
};

pub(in crate::subtitle_ocr) const PPOCR_V3_DET_MODEL: ModelSpec = ModelSpec {
    filename: "ch_PP-OCRv3_det_infer.onnx",
    url: "https://huggingface.co/SWHL/RapidOCR/resolve/main/PP-OCRv3/ch_PP-OCRv3_det_infer.onnx?download=true",
    sha256: "3439588C030FAEA393A54515F51E983D8E155B19A2E8ABA7891934C1CF0DE526",
};
pub(in crate::subtitle_ocr) const PPOCR_V3_CLS_MODEL: ModelSpec = ModelSpec {
    filename: "ch_ppocr_mobile_v2.0_cls_train.onnx",
    url: "https://huggingface.co/SWHL/RapidOCR/resolve/main/PP-OCRv3/ch_ppocr_mobile_v2.0_cls_train.onnx?download=true",
    sha256: "70581B300B83BABD9E0DD1D7D74C5B006869E8796DA277A70C2E405BF9D77C82",
};
pub(in crate::subtitle_ocr) const PPOCR_V3_REC_MODEL: ModelSpec = ModelSpec {
    filename: "en_PP-OCRv3_rec_infer.onnx",
    url: "https://huggingface.co/SWHL/RapidOCR/resolve/main/PP-OCRv3/en_PP-OCRv3_rec_infer.onnx?download=true",
    sha256: "EF7ABD8BD3629AE57EA2C28B425C1BD258A871B93FD2FE7C433946ADE9B5D9EA",
};

pub(in crate::subtitle_ocr) const PPOCR_V4_LATIN_REC_MODEL: ModelSpec = ModelSpec {
    filename: "latin_PP-OCRv3_rec_mobile.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/master/onnx/PP-OCRv4/rec/latin_PP-OCRv3_rec_mobile.onnx",
    sha256: "E9D7A33667E8AAA702862975186ADF2012E3F390CC0F9422865957125F8071CF",
};
pub(in crate::subtitle_ocr) const PPOCR_V4_JAPANESE_REC_MODEL: ModelSpec = ModelSpec {
    filename: "japan_PP-OCRv4_rec_mobile.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/master/onnx/PP-OCRv4/rec/japan_PP-OCRv4_rec_mobile.onnx",
    sha256: "E1075A67DBA758ECFC7EBC78A10AE61C95AC8FB66A9C86FAB5541E33F085CB7A",
};
pub(in crate::subtitle_ocr) const PPOCR_V4_KOREAN_REC_MODEL: ModelSpec = ModelSpec {
    filename: "korean_PP-OCRv4_rec_mobile.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/master/onnx/PP-OCRv4/rec/korean_PP-OCRv4_rec_mobile.onnx",
    sha256: "AB151BA9065ECCD98F884CF4D927DB091BE86137276392072EDD4F9D43AD7426",
};
pub(in crate::subtitle_ocr) const PPOCR_V4_CJK_REC_MODEL: ModelSpec = ModelSpec {
    filename: "chinese_cht_PP-OCRv3_rec_mobile.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/master/onnx/PP-OCRv4/rec/chinese_cht_PP-OCRv3_rec_mobile.onnx",
    sha256: "779656D044CE388045E02EA9244724616194E63928606436CDFC6DC3C9528CC6",
};

pub(in crate::subtitle_ocr) const PPOCR_V3_LATIN_REC_MODEL: ModelSpec = PPOCR_V4_LATIN_REC_MODEL;
pub(in crate::subtitle_ocr) const PPOCR_V3_JAPANESE_REC_MODEL: ModelSpec =
    PPOCR_V4_JAPANESE_REC_MODEL;
pub(in crate::subtitle_ocr) const PPOCR_V3_KOREAN_REC_MODEL: ModelSpec = PPOCR_V4_KOREAN_REC_MODEL;
pub(in crate::subtitle_ocr) const PPOCR_V3_CJK_REC_MODEL: ModelSpec = PPOCR_V4_CJK_REC_MODEL;

/// Stores data for PpOcrModels.
pub(in crate::subtitle_ocr) struct PpOcrModels {
    pub(in crate::subtitle_ocr) det: PathBuf,
    pub(in crate::subtitle_ocr) cls: PathBuf,
    pub(in crate::subtitle_ocr) rec: PathBuf,
}

/// Executes the resolve model dir routine.
pub(in crate::subtitle_ocr) fn resolve_model_dir() -> Result<PathBuf> {
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

/// Executes the ensure ppocr models routine.
pub(in crate::subtitle_ocr) fn ensure_ppocr_models(
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

/// Executes the resolve optional latin rec model routine.
pub(in crate::subtitle_ocr) fn resolve_optional_latin_rec_model(
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

/// Executes the resolve optional japanese rec model routine.
pub(in crate::subtitle_ocr) fn resolve_optional_japanese_rec_model(
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

/// Resolves an optional multilingual recognizer model from operator override or local cache.
///
/// This resolver is intentionally local-first and does not auto-download to avoid
/// pinning brittle upstream URLs/hashes for fast-moving multilingual model variants.
pub(in crate::subtitle_ocr) fn resolve_optional_multilingual_rec_model(
    model_dir: &Path,
    variant: PpOcrVariant,
) -> Result<Option<PathBuf>> {
    if let Some(path) = env::var_os("DPN_OCR_REC_MULTILINGUAL_MODEL") {
        let rec_path = PathBuf::from(path);
        if !rec_path.is_file() {
            bail!(
                "DPN_OCR_REC_MULTILINGUAL_MODEL is set but file does not exist: '{}'",
                rec_path.display()
            );
        }
        return Ok(Some(rec_path));
    }

    let candidates: Vec<String> = match variant {
        PpOcrVariant::V3 => vec![
            "multilingual_PP-OCRv3_rec_infer.onnx".to_string(),
            "multilingual_PP-OCRv4_rec_infer.onnx".to_string(),
            "multi_language_PP-OCRv3_rec_infer.onnx".to_string(),
            "arabic_PP-OCRv3_rec_infer.onnx".to_string(),
            "cyrillic_PP-OCRv3_rec_infer.onnx".to_string(),
            "devanagari_PP-OCRv3_rec_infer.onnx".to_string(),
        ],
        PpOcrVariant::V4 => vec![
            "multilingual_PP-OCRv4_rec_infer.onnx".to_string(),
            "multilingual_PP-OCRv3_rec_infer.onnx".to_string(),
            "multi_language_PP-OCRv4_rec_infer.onnx".to_string(),
            "arabic_PP-OCRv4_rec_infer.onnx".to_string(),
            "cyrillic_PP-OCRv4_rec_infer.onnx".to_string(),
            "devanagari_PP-OCRv4_rec_infer.onnx".to_string(),
        ],
    };
    for candidate in candidates {
        let path = model_dir.join(candidate);
        if path.is_file() {
            return Ok(Some(path));
        }
    }
    Ok(discover_local_multilingual_rec_model(model_dir))
}

/// Executes the resolve optional korean rec model routine.
pub(in crate::subtitle_ocr) fn resolve_optional_korean_rec_model(
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

/// Scans the model directory for multilingual/non-Latin PP-OCR recognizer files.
fn discover_local_multilingual_rec_model(model_dir: &Path) -> Option<PathBuf> {
    let entries = fs::read_dir(model_dir).ok()?;
    let mut preferred: Option<PathBuf> = None;
    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_file() {
            continue;
        }
        if path.extension().and_then(|ext| ext.to_str()) != Some("onnx") {
            continue;
        }
        let Some(file_name) = path.file_name().and_then(|n| n.to_str()) else {
            continue;
        };
        let lower = file_name.to_ascii_lowercase();
        if !(lower.contains("rec") && lower.contains("pp-ocr")) {
            continue;
        }
        if lower.contains("multilingual")
            || lower.contains("arabic")
            || lower.contains("cyrillic")
            || lower.contains("devanagari")
        {
            return Some(path);
        }
        // As a weak fallback, accept non-Latin dedicated rec models as "multilingual enough".
        if preferred.is_none()
            && (lower.contains("greek")
                || lower.contains("hebrew")
                || lower.contains("thai")
                || lower.contains("tamil")
                || lower.contains("bengali"))
        {
            preferred = Some(path);
        }
    }
    preferred
}

/// Executes the resolve optional cjk rec model routine.
pub(in crate::subtitle_ocr) fn resolve_optional_cjk_rec_model(
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

/// Executes the resolve optional rec model with candidates routine.
pub(in crate::subtitle_ocr) fn resolve_optional_rec_model_with_candidates(
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

/// Executes the ensure model file routine.
pub(in crate::subtitle_ocr) fn ensure_model_file(
    model_dir: &Path,
    spec: &ModelSpec,
) -> Result<PathBuf> {
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
/// Executes the ensure model file with values routine.
pub(in crate::subtitle_ocr) fn ensure_model_file_with_values(
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

/// Executes the download model with values routine.
pub(in crate::subtitle_ocr) fn download_model_with_values(
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

/// Executes the sha256 file routine.
pub(in crate::subtitle_ocr) fn sha256_file(path: &Path) -> Result<String> {
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

/// Executes the to hex lower routine.
pub(in crate::subtitle_ocr) fn to_hex_lower(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len() * 2);
    for byte in bytes {
        use std::fmt::Write as _;
        let _ = write!(&mut out, "{:02x}", byte);
    }
    out
}
