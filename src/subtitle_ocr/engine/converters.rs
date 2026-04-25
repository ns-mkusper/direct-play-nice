use anyhow::{anyhow, Result};
use std::path::Path;

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
/// Unknown tags intentionally fall back to `English` so OCR can still proceed.
pub(in crate::subtitle_ocr) fn rec_profile_for_language(language: &str) -> OcrRecProfile {
    let normalized =
        map_language_tag_to_tesseract(language).unwrap_or_else(|| language.to_ascii_lowercase());
    match normalized.as_str() {
        "eng" => OcrRecProfile::English,
        "jpn" | "ja" => OcrRecProfile::Japanese,
        "kor" | "ko" => OcrRecProfile::Korean,
        "chi_sim" | "chi_tra" | "chi" | "zho" | "zh" => OcrRecProfile::Cjk,
        "fra" | "fre" | "spa" | "deu" | "ger" | "ita" | "por" | "nld" | "swe" | "dan" | "nor"
        | "fin" | "ron" | "pol" | "ces" | "slk" | "hun" | "tur" | "cat" | "glg" | "ind" | "vie" => {
            OcrRecProfile::Latin
        }
        _ => OcrRecProfile::English,
    }
}
