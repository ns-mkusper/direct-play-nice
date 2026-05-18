//! Language requirement checks for Sonarr/Radarr-triggered runs.
//!
//! This module intentionally only inspects the already-imported media file.
//! When requirements are not met, the API layer may ask Servarr for manual-search
//! results and only grab a specific replacement when the release metadata proves
//! the desired languages are available.

use anyhow::Result;
use rsmpeg::avformat::AVFormatContextInput;
use rsmpeg::avutil::AVDictionary;
use rsmpeg::ffi;
use std::collections::BTreeSet;
use std::path::Path;

#[derive(Debug, Clone, Default)]
pub struct LanguageRequirements {
    pub enabled: bool,
    pub audio: Vec<String>,
    pub subtitles: Vec<String>,
}

impl LanguageRequirements {
    pub fn is_effective(&self) -> bool {
        self.enabled && (!self.audio.is_empty() || !self.subtitles.is_empty())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LanguageCheckReport {
    pub present_audio: Vec<String>,
    pub present_subtitles: Vec<String>,
    pub missing_audio: Vec<String>,
    pub missing_subtitles: Vec<String>,
}

impl LanguageCheckReport {
    pub fn satisfied(&self) -> bool {
        self.missing_audio.is_empty() && self.missing_subtitles.is_empty()
    }

    pub fn describe_missing(&self) -> String {
        let mut parts = Vec::new();
        if !self.missing_audio.is_empty() {
            parts.push(format!("audio [{}]", self.missing_audio.join(", ")));
        }
        if !self.missing_subtitles.is_empty() {
            parts.push(format!("subtitles [{}]", self.missing_subtitles.join(", ")));
        }
        parts.join("; ")
    }
}

pub fn parse_language_list(raw: Option<&str>) -> Vec<String> {
    raw.unwrap_or("")
        .split(',')
        .map(str::trim)
        .filter(|entry| !entry.is_empty())
        .filter_map(normalize_language_tag)
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect()
}

pub fn check_file(path: &Path, requirements: &LanguageRequirements) -> Result<LanguageCheckReport> {
    let path_cstr = path_to_cstr(path)?;
    let ictx = AVFormatContextInput::open(path_cstr.as_c_str())?;
    let mut audio = BTreeSet::new();
    let mut subtitles = BTreeSet::new();

    for stream in ictx.streams() {
        let cp = stream.codecpar();
        let language = stream
            .metadata()
            .as_deref()
            .and_then(extract_language_tag_from_metadata)
            .and_then(|tag| normalize_language_tag(&tag));
        let Some(language) = language else {
            continue;
        };
        match cp.codec_type {
            ffi::AVMEDIA_TYPE_AUDIO => {
                audio.insert(language);
            }
            ffi::AVMEDIA_TYPE_SUBTITLE => {
                subtitles.insert(language);
            }
            _ => {}
        }
    }

    Ok(check_sets(
        audio.into_iter().collect(),
        subtitles.into_iter().collect(),
        requirements,
    ))
}

fn check_sets(
    present_audio: Vec<String>,
    present_subtitles: Vec<String>,
    requirements: &LanguageRequirements,
) -> LanguageCheckReport {
    let audio_set = present_audio.iter().cloned().collect::<BTreeSet<_>>();
    let subtitle_set = present_subtitles.iter().cloned().collect::<BTreeSet<_>>();
    let missing_audio = requirements
        .audio
        .iter()
        .filter(|lang| !audio_set.contains(*lang))
        .cloned()
        .collect();
    let missing_subtitles = requirements
        .subtitles
        .iter()
        .filter(|lang| !subtitle_set.contains(*lang))
        .cloned()
        .collect();

    LanguageCheckReport {
        present_audio,
        present_subtitles,
        missing_audio,
        missing_subtitles,
    }
}

fn path_to_cstr(path: &Path) -> Result<std::ffi::CString> {
    let raw = path.to_string_lossy();
    Ok(std::ffi::CString::new(raw.as_bytes())?)
}

fn extract_language_tag_from_metadata(dict: &AVDictionary) -> Option<String> {
    for entry in dict.iter() {
        if entry
            .key()
            .to_string_lossy()
            .eq_ignore_ascii_case("language")
        {
            let value = entry.value().to_string_lossy().trim().to_string();
            if !value.is_empty() {
                return Some(value);
            }
        }
    }
    None
}

pub(super) fn normalize_language_tag(input: &str) -> Option<String> {
    let primary = input
        .trim()
        .to_ascii_lowercase()
        .split(['-', '_'])
        .next()
        .unwrap_or("")
        .to_string();
    if primary.is_empty() || primary == "und" || primary == "unknown" {
        return None;
    }

    let normalized = match primary.as_str() {
        "en" | "eng" | "english" => "eng",
        "fr" | "fre" | "fra" | "french" => "fra",
        "es" | "spa" | "spanish" => "spa",
        "de" | "ger" | "deu" | "german" => "deu",
        "it" | "ita" | "italian" => "ita",
        "pt" | "por" | "portuguese" => "por",
        "ja" | "jpn" | "japanese" => "jpn",
        "ko" | "kor" | "korean" => "kor",
        "zh" | "zho" | "chi" | "chinese" => "zho",
        "ru" | "rus" | "russian" => "rus",
        "nl" => "nld",
        "pl" => "pol",
        "sv" => "swe",
        "no" | "nb" => "nor",
        "da" => "dan",
        "fi" => "fin",
        "cs" => "ces",
        "cz" | "cze" => "ces",
        "el" | "gre" => "ell",
        "he" => "heb",
        "hi" => "hin",
        "ar" => "ara",
        "tr" => "tur",
        "th" => "tha",
        "vi" => "vie",
        "id" => "ind",
        "uk" => "ukr",
        _ => primary.as_str(),
    };

    Some(normalized.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_language_list_normalizes_and_deduplicates() {
        assert_eq!(
            parse_language_list(Some("en, eng, fr-FR, spa, und, ")),
            vec!["eng", "fra", "spa"]
        );
    }

    #[test]
    fn check_sets_reports_missing_required_languages() {
        let requirements = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".into(), "jpn".into()],
            subtitles: vec!["eng".into(), "spa".into()],
        };

        let report = check_sets(
            vec!["eng".into()],
            vec!["eng".into(), "fra".into()],
            &requirements,
        );

        assert!(!report.satisfied());
        assert_eq!(report.missing_audio, vec!["jpn"]);
        assert_eq!(report.missing_subtitles, vec!["spa"]);
        assert_eq!(report.describe_missing(), "audio [jpn]; subtitles [spa]");
    }
}
