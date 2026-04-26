use anyhow::{anyhow, Result};
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
    Japanese,
    Korean,
    Cjk,
}

static REC_PROFILE_OVERRIDES: OnceLock<Vec<(String, OcrRecProfile)>> = OnceLock::new();

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
        let rec_profile = rec_profile_for_language(language);
        let (ocr, rec_label) = match rec_profile {
            OcrRecProfile::Japanese => {
                if let Some(japanese_ocr) = self.japanese_ocr.as_mut() {
                    (japanese_ocr, "japanese")
                } else if let Some(cjk_ocr) = self.cjk_ocr.as_mut() {
                    (cjk_ocr, "cjk")
                } else {
                    (&mut self.english_ocr, "english")
                }
            }
            OcrRecProfile::Korean => {
                if let Some(korean_ocr) = self.korean_ocr.as_mut() {
                    (korean_ocr, "korean")
                } else if let Some(cjk_ocr) = self.cjk_ocr.as_mut() {
                    (cjk_ocr, "cjk")
                } else {
                    (&mut self.english_ocr, "english")
                }
            }
            OcrRecProfile::Cjk => {
                if let Some(cjk_ocr) = self.cjk_ocr.as_mut() {
                    (cjk_ocr, "cjk")
                } else {
                    (&mut self.english_ocr, "english")
                }
            }
            OcrRecProfile::Latin => {
                if let Some(latin_ocr) = self.latin_ocr.as_mut() {
                    (latin_ocr, "latin")
                } else {
                    (&mut self.english_ocr, "english")
                }
            }
            OcrRecProfile::English => (&mut self.english_ocr, "english"),
        };

        let img = load_image(image_path)?;
        let result = ocr
            .detect(&img, 50, 1024, 0.5, 0.3, 1.6, false, false)
            .map_err(|err| {
                anyhow!(
                    "{} failed (rec_profile={}, language={}): {} (debug: {:?})",
                    self.variant.label(),
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

/// Maps a language tag to the PP-OCR recognition profile used for model selection.
///
/// Routing policy:
/// - explicit language aliases for profiles with dedicated recognizers
/// - BCP-47 script hints (`xx-Latn`, `xx-Hant`, ...)
/// - user overrides via `DPN_OCR_REC_PROFILE_OVERRIDES`
/// - final safe fallback (`Latin`) so OCR still runs for unclassified tags
pub(in crate::subtitle_ocr) fn rec_profile_for_language(language: &str) -> OcrRecProfile {
    if let Some(profile) = rec_profile_override(language) {
        return profile;
    }

    let normalized =
        map_language_tag_to_tesseract(language).unwrap_or_else(|| language.to_ascii_lowercase());
    match normalized.as_str() {
        "eng" => OcrRecProfile::English,
        "jpn" | "ja" => OcrRecProfile::Japanese,
        "kor" | "ko" => OcrRecProfile::Korean,
        "chi_sim" | "chi_tra" | "chi" | "zho" | "zh" => OcrRecProfile::Cjk,
        _ => {
            if let Some(script) = script_subtag(language) {
                return profile_for_script(script);
            }
            if known_non_latin_code(&normalized) {
                return OcrRecProfile::English;
            }
            OcrRecProfile::Latin
        }
    }
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
                "japanese" | "jpn" | "ja" => OcrRecProfile::Japanese,
                "korean" | "kor" | "ko" => OcrRecProfile::Korean,
                "cjk" | "zh" | "zho" => OcrRecProfile::Cjk,
                _ => return None,
            };
            Some((lang, profile))
        })
        .collect()
}

/// Extracts the optional 4-letter BCP-47 script subtag (`Latn`, `Cyrl`, ...)
/// from a language tag such as `sr-Latn-RS`.
fn script_subtag(language: &str) -> Option<&str> {
    language
        .trim()
        .split(['-', '_'])
        .find(|part| part.len() == 4 && part.chars().all(|ch| ch.is_ascii_alphabetic()))
}

/// Maps a BCP-47 script subtag into a rec profile.
///
/// Scripts without dedicated recognizers route to `English` as the neutral fallback.
fn profile_for_script(script: &str) -> OcrRecProfile {
    match script.to_ascii_lowercase().as_str() {
        "latn" => OcrRecProfile::Latin,
        "jpan" | "hira" | "kana" => OcrRecProfile::Japanese,
        "hang" | "kore" => OcrRecProfile::Korean,
        "hani" | "hans" | "hant" | "bopo" => OcrRecProfile::Cjk,
        _ => OcrRecProfile::English,
    }
}

/// Returns `true` for language codes that are known to be predominantly non-Latin
/// and have no dedicated profile in the current PP-OCR model set.
fn known_non_latin_code(normalized: &str) -> bool {
    matches!(
        normalized,
        "ara"
            | "heb"
            | "rus"
            | "ukr"
            | "ell"
            | "hin"
            | "tha"
            | "fas"
            | "per"
            | "urd"
            | "pus"
            | "tam"
            | "tel"
            | "kan"
            | "mal"
            | "ben"
            | "lao"
            | "khm"
            | "mya"
            | "amh"
            | "tir"
            | "dzo"
            | "bod"
            | "tib"
    )
}
