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

fn resolve_optional_latin_rec_model(model_dir: &Path, variant: PpOcrVariant) -> Result<Option<PathBuf>> {
    if let Some(path) = env::var_os("DPN_OCR_REC_LATIN_MODEL") {
        let rec_path = PathBuf::from(path);
        if !rec_path.is_file() {
            bail!(
                "DPN_OCR_REC_LATIN_MODEL is set but file does not exist: '{}'",
                rec_path.display()
            );
        }
        return Ok(Some(rec_path));
    }

    let candidates: &[&str] = match variant {
        PpOcrVariant::V3 => &[
            "latin_PP-OCRv3_rec_infer.onnx",
            "latin_ppocr_mobile_v2.0_rec_infer.onnx",
            "multilingual_PP-OCRv3_rec_infer.onnx",
        ],
        PpOcrVariant::V4 => &[
            "latin_PP-OCRv4_rec_infer.onnx",
            "latin_ppocr_mobile_v2.0_rec_infer.onnx",
            "multilingual_PP-OCRv4_rec_infer.onnx",
        ],
    };

    for candidate in candidates {
        let path = model_dir.join(candidate);
        if path.is_file() {
            return Ok(Some(path));
        }
    }
    Ok(None)
}

fn resolve_optional_japanese_rec_model(
    model_dir: &Path,
    variant: PpOcrVariant,
) -> Result<Option<PathBuf>> {
    resolve_optional_rec_model_with_candidates(
        "DPN_OCR_REC_JAPANESE_MODEL",
        model_dir,
        match variant {
            PpOcrVariant::V3 => &[
                "japan_PP-OCRv3_rec_infer.onnx",
                "japanese_PP-OCRv3_rec_infer.onnx",
                "ja_PP-OCRv3_rec_infer.onnx",
            ],
            PpOcrVariant::V4 => &[
                "japan_PP-OCRv4_rec_infer.onnx",
                "japanese_PP-OCRv4_rec_infer.onnx",
                "ja_PP-OCRv4_rec_infer.onnx",
            ],
        },
    )
}

fn resolve_optional_korean_rec_model(
    model_dir: &Path,
    variant: PpOcrVariant,
) -> Result<Option<PathBuf>> {
    resolve_optional_rec_model_with_candidates(
        "DPN_OCR_REC_KOREAN_MODEL",
        model_dir,
        match variant {
            PpOcrVariant::V3 => &[
                "korean_PP-OCRv3_rec_infer.onnx",
                "ko_PP-OCRv3_rec_infer.onnx",
            ],
            PpOcrVariant::V4 => &[
                "korean_PP-OCRv4_rec_infer.onnx",
                "ko_PP-OCRv4_rec_infer.onnx",
            ],
        },
    )
}

fn resolve_optional_cjk_rec_model(model_dir: &Path, variant: PpOcrVariant) -> Result<Option<PathBuf>> {
    resolve_optional_rec_model_with_candidates(
        "DPN_OCR_REC_CJK_MODEL",
        model_dir,
        match variant {
            PpOcrVariant::V3 => &[
                "cjk_PP-OCRv3_rec_infer.onnx",
                "chinese_PP-OCRv3_rec_infer.onnx",
                "zh_PP-OCRv3_rec_infer.onnx",
            ],
            PpOcrVariant::V4 => &[
                "cjk_PP-OCRv4_rec_infer.onnx",
                "chinese_PP-OCRv4_rec_infer.onnx",
                "zh_PP-OCRv4_rec_infer.onnx",
            ],
        },
    )
}

fn resolve_optional_rec_model_with_candidates(
    env_key: &str,
    model_dir: &Path,
    candidates: &[&str],
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
    Ok(None)
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
