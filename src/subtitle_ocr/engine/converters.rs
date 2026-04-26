//! Engine-specific OCR adapters.
//!
//! Implements `SubtitleConverter` for Tesseract, external command OCR, and
//! PP-OCR, including language/script to recognizer-profile routing.

use anyhow::{anyhow, Result};
use paddle_ocr_rs::ocr_lite::OcrLite;
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;
use std::sync::OnceLock;

use super::super::language::map_language_tag_to_tesseract;
use super::super::text_render::{
    bounding_box_from_points, load_image, merge_ocr_lines_with_spacing, normalize_utf8_text,
    run_external_ocr_command, run_tesseract,
};
use super::{ExternalEngine, OcrLine, OcrOutput, PpOcrEngine, SubtitleConverter, TesseractEngine};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::subtitle_ocr) enum OcrRecProfile {
    English,
    Latin,
    Multilingual,
    Japanese,
    Korean,
    Cjk,
}

static REC_PROFILE_OVERRIDES: OnceLock<Vec<(String, OcrRecProfile)>> = OnceLock::new();
static LANGUAGE_SCRIPT_HINTS: OnceLock<Vec<(String, String)>> = OnceLock::new();
static ROUTING_MANIFEST: OnceLock<RoutingManifest> = OnceLock::new();

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

impl SubtitleConverter for PpOcrEngine {
    fn extract_lines(&mut self, image_path: &Path, language: &str) -> Result<OcrOutput> {
        let PpOcrEngine {
            english_ocr,
            latin_ocr,
            multilingual_ocr,
            japanese_ocr,
            korean_ocr,
            cjk_ocr,
            variant,
        } = self;
        let rec_profile = rec_profile_for_language(language);
        let (ocr, rec_label) = match rec_profile {
            OcrRecProfile::Japanese => select_profile_ocr_with_fallback(
                japanese_ocr.as_mut(),
                cjk_ocr.as_mut(),
                english_ocr,
                "japanese",
                Some("cjk"),
            ),
            OcrRecProfile::Korean => select_profile_ocr_with_fallback(
                korean_ocr.as_mut(),
                cjk_ocr.as_mut(),
                english_ocr,
                "korean",
                Some("cjk"),
            ),
            OcrRecProfile::Cjk => {
                select_profile_ocr_with_fallback(cjk_ocr.as_mut(), None, english_ocr, "cjk", None)
            }
            OcrRecProfile::Latin => select_profile_ocr_with_fallback(
                latin_ocr.as_mut(),
                None,
                english_ocr,
                "latin",
                None,
            ),
            OcrRecProfile::Multilingual => select_profile_ocr_with_fallback(
                multilingual_ocr.as_mut(),
                cjk_ocr.as_mut(),
                english_ocr,
                "multilingual",
                Some("cjk"),
            ),
            OcrRecProfile::English => (english_ocr, "english"),
        };

        let img = load_image(image_path)?;
        let result = ocr
            .detect(&img, 50, 1024, 0.5, 0.3, 1.6, false, false)
            .map_err(|err| {
                anyhow!(
                    "{} failed (rec_profile={}, language={}): {} (debug: {:?})",
                    variant.label(),
                    rec_label,
                    language,
                    err,
                    err
                )
            })?;

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

        Ok(OcrOutput {
            lines: merge_ocr_lines_with_spacing(lines),
        })
    }
}

fn select_profile_ocr_with_fallback<'a>(
    primary: Option<&'a mut OcrLite>,
    secondary: Option<&'a mut OcrLite>,
    english: &'a mut OcrLite,
    primary_label: &'static str,
    secondary_label: Option<&'static str>,
) -> (&'a mut OcrLite, &'static str) {
    if let Some(ocr) = primary {
        return (ocr, primary_label);
    }
    if let (Some(ocr), Some(label)) = (secondary, secondary_label) {
        return (ocr, label);
    }
    (english, "english")
}

/// Maps a language tag to the PP-OCR recognition profile used for model selection.
///
/// Routing policy:
/// - explicit language aliases for profiles with dedicated recognizers
/// - BCP-47 script hints (`xx-Latn`, `xx-Hant`, ...)
/// - user overrides via `DPN_OCR_REC_PROFILE_OVERRIDES`
/// - final safe fallback (`Latin`) so OCR still runs for unclassified tags
pub(in crate::subtitle_ocr) fn rec_profile_for_language(language: &str) -> OcrRecProfile {
    let manifest = ROUTING_MANIFEST.get_or_init(load_routing_manifest);
    if let Some(profile) = rec_profile_override(language) {
        return profile;
    }

    let normalized =
        map_language_tag_to_tesseract(language).unwrap_or_else(|| language.to_ascii_lowercase());
    for key in candidate_language_keys(language, &normalized) {
        if let Some(name) = manifest.language_profiles.get(&key) {
            if let Some(profile) = parse_profile_name(name) {
                return profile;
            }
        }
    }

    if let Some(script) = resolved_script(language, &normalized, manifest) {
        return profile_for_script(&script, manifest);
    }

    manifest.default_profile
}

/// Resolves an optional operator override from `DPN_OCR_REC_PROFILE_OVERRIDES`.
///
/// Example:
/// `DPN_OCR_REC_PROFILE_OVERRIDES=rus=english,sr-Latn=latin,ja=japanese`
fn rec_profile_override(language: &str) -> Option<OcrRecProfile> {
    let overrides = REC_PROFILE_OVERRIDES.get_or_init(parse_rec_profile_overrides);
    if overrides.is_empty() {
        return None;
    }
    let input = language.trim().to_ascii_lowercase();
    let mapped = map_language_tag_to_tesseract(language).map(|s| s.to_ascii_lowercase());
    overrides.iter().find_map(|(code, profile)| {
        if code == &input || mapped.as_deref() == Some(code.as_str()) {
            Some(*profile)
        } else {
            None
        }
    })
}

/// Parses `DPN_OCR_REC_PROFILE_OVERRIDES` into `(language_code, profile)` entries.
fn parse_rec_profile_overrides() -> Vec<(String, OcrRecProfile)> {
    let Ok(raw) = std::env::var("DPN_OCR_REC_PROFILE_OVERRIDES") else {
        return Vec::new();
    };
    raw.split(',')
        .filter_map(|entry| {
            let (lang, profile) = entry.split_once('=')?;
            let lang = lang.trim().to_ascii_lowercase();
            if lang.is_empty() {
                return None;
            }
            let profile = match profile.trim().to_ascii_lowercase().as_str() {
                "english" | "eng" => OcrRecProfile::English,
                "latin" | "latn" => OcrRecProfile::Latin,
                "multilingual" | "multi" => OcrRecProfile::Multilingual,
                "japanese" | "jpn" | "ja" => OcrRecProfile::Japanese,
                "korean" | "kor" | "ko" => OcrRecProfile::Korean,
                "cjk" | "zh" | "zho" => OcrRecProfile::Cjk,
                _ => return None,
            };
            Some((lang, profile))
        })
        .collect()
}

fn resolved_script(language: &str, normalized: &str, manifest: &RoutingManifest) -> Option<String> {
    if let Some(script) = script_subtag(language) {
        return Some(script.to_string());
    }
    configured_script_hint(language, normalized)
        .or_else(|| likely_script_from_manifest(language, normalized, manifest))
}

/// Extracts the optional 4-letter BCP-47 script subtag (`Latn`, `Cyrl`, ...)
/// from a language tag such as `sr-Latn-RS`.
fn script_subtag(language: &str) -> Option<&str> {
    language
        .trim()
        .split(['-', '_'])
        .find(|part| part.len() == 4 && part.chars().all(|ch| ch.is_ascii_alphabetic()))
}

/// Resolves language->script hints from `DPN_OCR_LANGUAGE_SCRIPT_HINTS`.
///
/// Example:
/// `DPN_OCR_LANGUAGE_SCRIPT_HINTS=rus=Cyrl,ara=Arab,srp=Cyrl`
fn configured_script_hint(language: &str, normalized: &str) -> Option<String> {
    let hints = LANGUAGE_SCRIPT_HINTS.get_or_init(parse_language_script_hints);
    if hints.is_empty() {
        return None;
    }
    let input = language.trim().to_ascii_lowercase();
    let mapped = map_language_tag_to_tesseract(language).map(|s| s.to_ascii_lowercase());
    for (code, script) in hints {
        if code == &input || code == normalized || mapped.as_deref() == Some(code.as_str()) {
            return Some(script.clone());
        }
    }
    None
}

fn likely_script_from_manifest(
    language: &str,
    normalized: &str,
    manifest: &RoutingManifest,
) -> Option<String> {
    for key in candidate_language_keys(language, normalized) {
        if let Some(script) = manifest.likely_scripts.get(&key) {
            return Some(script.clone());
        }
    }
    None
}

fn candidate_language_keys(language: &str, normalized: &str) -> Vec<String> {
    let lower_input = language.trim().to_ascii_lowercase();
    let mut out = Vec::new();
    let mut seen = HashSet::new();
    for key in [
        Some(lower_input.as_str()),
        Some(normalized),
        primary_subtag(&lower_input),
        primary_subtag(normalized),
    ]
    .into_iter()
    .flatten()
    {
        if seen.insert(key.to_string()) {
            out.push(key.to_string());
        }
    }
    out
}

fn primary_subtag(tag: &str) -> Option<&str> {
    let trimmed = tag.trim();
    if trimmed.is_empty() {
        return None;
    }
    Some(trimmed.split(['-', '_']).next().unwrap_or(trimmed))
}

fn parse_language_script_hints() -> Vec<(String, String)> {
    let Ok(raw) = std::env::var("DPN_OCR_LANGUAGE_SCRIPT_HINTS") else {
        return Vec::new();
    };
    raw.split(',')
        .filter_map(|entry| {
            let (lang, script) = entry.split_once('=')?;
            let lang = lang.trim().to_ascii_lowercase();
            let script = script.trim().to_string();
            if lang.is_empty() || script.is_empty() {
                return None;
            }
            Some((lang, script))
        })
        .collect()
}

/// Maps a BCP-47 script subtag into a rec profile.
///
/// Scripts without dedicated recognizers route to `English` as the neutral fallback.
fn profile_for_script(script: &str, manifest: &RoutingManifest) -> OcrRecProfile {
    let key = script.to_ascii_lowercase();
    if let Some(name) = manifest.script_profiles.get(&key) {
        if let Some(profile) = parse_profile_name(name) {
            return profile;
        }
    }
    OcrRecProfile::English
}

fn parse_profile_name(value: &str) -> Option<OcrRecProfile> {
    match value.trim().to_ascii_lowercase().as_str() {
        "english" | "eng" => Some(OcrRecProfile::English),
        "latin" | "latn" => Some(OcrRecProfile::Latin),
        "multilingual" | "multi" => Some(OcrRecProfile::Multilingual),
        "japanese" | "jpn" | "ja" => Some(OcrRecProfile::Japanese),
        "korean" | "kor" | "ko" => Some(OcrRecProfile::Korean),
        "cjk" | "zh" | "zho" => Some(OcrRecProfile::Cjk),
        _ => None,
    }
}

#[derive(Debug, Clone, Deserialize)]
struct RoutingManifestFile {
    #[serde(default)]
    default_profile: String,
    #[serde(default)]
    language_profiles: HashMap<String, String>,
    #[serde(default)]
    script_profiles: HashMap<String, String>,
    #[serde(default)]
    likely_scripts: HashMap<String, String>,
}

#[derive(Debug, Clone)]
struct RoutingManifest {
    default_profile: OcrRecProfile,
    language_profiles: HashMap<String, String>,
    script_profiles: HashMap<String, String>,
    likely_scripts: HashMap<String, String>,
}

fn load_routing_manifest() -> RoutingManifest {
    const BUILTIN: &str = include_str!("../../../config/ocr-routing.toml");
    let raw = std::env::var("DPN_OCR_ROUTING_MANIFEST")
        .ok()
        .and_then(|path| fs::read_to_string(path).ok())
        .unwrap_or_else(|| BUILTIN.to_string());

    let parsed = toml::from_str::<RoutingManifestFile>(&raw)
        .ok()
        .or_else(|| toml::from_str::<RoutingManifestFile>(BUILTIN).ok())
        .unwrap_or_else(default_routing_manifest_file);

    let default_profile =
        parse_profile_name(&parsed.default_profile).unwrap_or(OcrRecProfile::Latin);
    RoutingManifest {
        default_profile,
        language_profiles: normalize_manifest_keys(parsed.language_profiles),
        script_profiles: normalize_manifest_keys(parsed.script_profiles),
        likely_scripts: normalize_manifest_keys(parsed.likely_scripts),
    }
}

fn normalize_manifest_keys(map: HashMap<String, String>) -> HashMap<String, String> {
    map.into_iter()
        .map(|(k, v)| (k.trim().to_ascii_lowercase(), v.trim().to_string()))
        .collect()
}

fn default_routing_manifest_file() -> RoutingManifestFile {
    RoutingManifestFile {
        default_profile: "latin".to_string(),
        language_profiles: HashMap::new(),
        script_profiles: HashMap::new(),
        likely_scripts: HashMap::new(),
    }
}
