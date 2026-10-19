//! OCR language normalization utilities that map metadata tags to engine-specific recognition settings.
//!
//! This module maps stream metadata and environment defaults into normalized
//! OCR language codes and Tesseract-compatible pack names.

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

/// What redundancy detection needs to know about one subtitle stream.
#[derive(Debug, Clone)]
pub(crate) struct SubtitleStreamFacts {
    pub(crate) index: i32,
    pub(crate) bitmap: bool,
    pub(crate) forced: bool,
    pub(crate) language: Option<String>,
}

/// Normalizes a stream language tag so `en`, `eng` and `en-US` compare equal.
/// Tags without a known mapping compare by their lowercased primary subtag.
fn redundancy_language_key(tag: &str) -> Option<String> {
    let primary = tag
        .trim()
        .to_ascii_lowercase()
        .split(['-', '_'])
        .next()
        .unwrap_or_default()
        .to_string();
    if primary.is_empty() || matches!(primary.as_str(), "und" | "unk" | "mis" | "zxx") {
        return None;
    }
    Some(map_language_tag_to_tesseract(&primary).unwrap_or(primary))
}

/// Bitmap subtitle streams that duplicate a text subtitle stream: same
/// language and same forced flag. A bitmap stream with no usable language tag
/// is never redundant, so OCR still runs on it.
pub(crate) fn redundant_bitmap_subtitles(streams: &[SubtitleStreamFacts]) -> HashSet<i32> {
    let text_tracks: HashSet<(String, bool)> = streams
        .iter()
        .filter(|s| !s.bitmap)
        .filter_map(|s| {
            let key = redundancy_language_key(s.language.as_deref()?)?;
            Some((key, s.forced))
        })
        .collect();
    streams
        .iter()
        .filter(|s| s.bitmap)
        .filter(|s| {
            s.language
                .as_deref()
                .and_then(redundancy_language_key)
                .is_some_and(|key| text_tracks.contains(&(key, s.forced)))
        })
        .map(|s| s.index)
        .collect()
}

/// Collects [`SubtitleStreamFacts`] for every subtitle stream in an input.
pub(crate) fn subtitle_stream_facts(ictx: &AVFormatContextInput) -> Vec<SubtitleStreamFacts> {
    ictx.streams()
        .iter()
        .filter(|stream| stream.codecpar().codec_type == ffi::AVMEDIA_TYPE_SUBTITLE)
        .map(|stream| SubtitleStreamFacts {
            index: stream.index,
            bitmap: is_image_based_subtitle(stream.codecpar().codec_id),
            forced: stream.disposition & ffi::AV_DISPOSITION_FORCED as i32 != 0,
            language: stream
                .metadata()
                .as_deref()
                .and_then(extract_language_tag_from_metadata),
        })
        .collect()
}

#[cfg(test)]
mod redundancy_tests {
    use super::*;

    fn facts(
        index: i32,
        bitmap: bool,
        forced: bool,
        language: Option<&str>,
    ) -> SubtitleStreamFacts {
        SubtitleStreamFacts {
            index,
            bitmap,
            forced,
            language: language.map(str::to_string),
        }
    }

    #[test]
    fn bitmap_stream_with_matching_text_language_is_redundant() {
        let streams = [
            facts(2, false, false, Some("en")),
            facts(3, true, false, Some("eng")),
            facts(4, true, false, Some("fre")),
        ];
        assert_eq!(redundant_bitmap_subtitles(&streams), HashSet::from([3]));
    }

    #[test]
    fn forced_flag_must_match() {
        let streams = [
            facts(2, false, false, Some("eng")),
            facts(3, true, true, Some("eng")),
        ];
        assert!(redundant_bitmap_subtitles(&streams).is_empty());
    }

    #[test]
    fn untagged_or_undetermined_streams_are_never_redundant() {
        let streams = [
            facts(2, false, false, None),
            facts(3, true, false, None),
            facts(4, false, false, Some("und")),
            facts(5, true, false, Some("und")),
        ];
        assert!(redundant_bitmap_subtitles(&streams).is_empty());
    }

    #[test]
    fn unmapped_languages_compare_by_primary_subtag() {
        let streams = [
            facts(2, false, false, Some("tlh-Latn")),
            facts(3, true, false, Some("TLH")),
        ];
        assert_eq!(redundant_bitmap_subtitles(&streams), HashSet::from([3]));
    }
}
