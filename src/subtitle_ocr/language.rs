use super::*;
pub(super) fn timestamp_to_ms(value: i64, time_base: ffi::AVRational) -> Option<i64> {
    if value == ffi::AV_NOPTS_VALUE || time_base.num <= 0 || time_base.den <= 0 {
        return None;
    }
    Some(unsafe { ffi::av_rescale_q(value, time_base, ffi::AVRational { num: 1, den: 1000 }) })
}

pub(super) fn extract_language_tag_from_metadata(
    dict: &rsmpeg::avutil::AVDictionary,
) -> Option<String> {
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

pub(super) fn resolve_ocr_language(
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

pub(super) fn detect_system_ocr_language() -> Option<String> {
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

pub(super) fn map_language_tag_to_tesseract(input: &str) -> Option<String> {
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

pub(super) fn list_tesseract_languages() -> Result<HashSet<String>> {
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

pub(super) fn tesseract_languages_cached() -> Option<&'static HashSet<String>> {
    let cached = TESSERACT_LANG_CACHE.get_or_init(list_tesseract_languages);
    cached.as_ref().ok()
}

pub(super) fn resolve_tesseract_fallback_language(language: &str) -> Option<String> {
    let langs = tesseract_languages_cached()?;
    resolve_tesseract_fallback_language_with_available(language, langs)
}

pub(super) fn resolve_tesseract_fallback_language_with_available(
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

pub(super) fn codec_name(codec_id: ffi::AVCodecID) -> String {
    unsafe {
        CStr::from_ptr(ffi::avcodec_get_name(codec_id))
            .to_string_lossy()
            .into_owned()
    }
}

pub(super) fn is_image_based_subtitle(codec_id: ffi::AVCodecID) -> bool {
    matches!(
        codec_id,
        ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE
            | ffi::AV_CODEC_ID_DVD_SUBTITLE
            | ffi::AV_CODEC_ID_DVB_SUBTITLE
            | ffi::AV_CODEC_ID_XSUB
    )
}
