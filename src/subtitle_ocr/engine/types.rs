use anyhow::Result;
use paddle_ocr_rs::ocr_lite::OcrLite;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::{atomic::AtomicBool, OnceLock};

use super::{
    ModelSpec, PPOCR_V3_CJK_REC_MODEL, PPOCR_V3_CLS_MODEL, PPOCR_V3_DET_MODEL,
    PPOCR_V3_JAPANESE_REC_MODEL, PPOCR_V3_KOREAN_REC_MODEL, PPOCR_V3_LATIN_REC_MODEL,
    PPOCR_V3_REC_MODEL, PPOCR_V4_CJK_REC_MODEL, PPOCR_V4_CLS_MODEL, PPOCR_V4_DET_MODEL,
    PPOCR_V4_JAPANESE_REC_MODEL, PPOCR_V4_KOREAN_REC_MODEL, PPOCR_V4_LATIN_REC_MODEL,
    PPOCR_V4_REC_MODEL,
};

pub(in crate::subtitle_ocr) struct SubtitleCandidate {
    pub(in crate::subtitle_ocr) stream_index: i32,
    pub(in crate::subtitle_ocr) language_tag: Option<String>,
}

#[derive(Debug)]
pub(in crate::subtitle_ocr) struct OcrTask {
    pub(in crate::subtitle_ocr) order: usize,
    pub(in crate::subtitle_ocr) stream_index: i32,
    pub(in crate::subtitle_ocr) language: String,
    pub(in crate::subtitle_ocr) subtitle_path: PathBuf,
}

#[derive(Debug)]
pub(in crate::subtitle_ocr) struct OcrTaskOutput {
    pub(in crate::subtitle_ocr) order: usize,
    pub(in crate::subtitle_ocr) stream_index: i32,
    pub(in crate::subtitle_ocr) language: String,
    pub(in crate::subtitle_ocr) subtitle_path: PathBuf,
    pub(in crate::subtitle_ocr) cues: Vec<SubtitleCue>,
}

#[derive(Debug, Clone)]
pub(in crate::subtitle_ocr) struct SubtitleCue {
    pub(in crate::subtitle_ocr) start_ms: i64,
    pub(in crate::subtitle_ocr) end_ms: i64,
    pub(in crate::subtitle_ocr) text: String,
}

#[derive(Debug, Clone)]
pub(in crate::subtitle_ocr) struct OcrBoundingBox {
    pub(in crate::subtitle_ocr) left: i32,
    pub(in crate::subtitle_ocr) top: i32,
    pub(in crate::subtitle_ocr) right: i32,
    pub(in crate::subtitle_ocr) bottom: i32,
}

#[derive(Debug, Clone)]
pub(in crate::subtitle_ocr) struct OcrLine {
    pub(in crate::subtitle_ocr) text: String,
    pub(in crate::subtitle_ocr) bbox: Option<OcrBoundingBox>,
    pub(in crate::subtitle_ocr) score: Option<f32>,
    pub(in crate::subtitle_ocr) color: Option<(u8, u8, u8)>,
    pub(in crate::subtitle_ocr) italic: bool,
}

#[derive(Debug, Default)]
pub(in crate::subtitle_ocr) struct OcrOutput {
    pub(in crate::subtitle_ocr) lines: Vec<OcrLine>,
}

pub(in crate::subtitle_ocr) trait SubtitleConverter {
    /// Extracts OCR text lines from a subtitle image for the requested language.
    ///
    /// Implementations should return an empty `OcrOutput` when extraction succeeds but no text is found.
    fn extract_lines(&mut self, image_path: &Path, language: &str) -> Result<OcrOutput>;
}

pub(in crate::subtitle_ocr) struct TesseractEngine;

pub(in crate::subtitle_ocr) struct ExternalEngine {
    pub(in crate::subtitle_ocr) command: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::subtitle_ocr) enum PpOcrVariant {
    V3,
    V4,
}

impl PpOcrVariant {
    /// Returns a human-readable label used in logs and diagnostics.
    pub(in crate::subtitle_ocr) fn label(self) -> &'static str {
        match self {
            PpOcrVariant::V3 => "PP-OCRv3",
            PpOcrVariant::V4 => "PP-OCRv4",
        }
    }

    /// Returns the detector/classifier/recognizer model specs for this PP-OCR variant.
    pub(in crate::subtitle_ocr) fn model_specs(
        self,
    ) -> (&'static ModelSpec, &'static ModelSpec, &'static ModelSpec) {
        match self {
            PpOcrVariant::V3 => (
                &PPOCR_V3_DET_MODEL,
                &PPOCR_V3_CLS_MODEL,
                &PPOCR_V3_REC_MODEL,
            ),
            PpOcrVariant::V4 => (
                &PPOCR_V4_DET_MODEL,
                &PPOCR_V4_CLS_MODEL,
                &PPOCR_V4_REC_MODEL,
            ),
        }
    }

    /// Returns the default latin recognizer model for the current PP-OCR variant.
    pub(in crate::subtitle_ocr) fn default_latin_rec_spec(self) -> &'static ModelSpec {
        match self {
            PpOcrVariant::V3 => &PPOCR_V3_LATIN_REC_MODEL,
            PpOcrVariant::V4 => &PPOCR_V4_LATIN_REC_MODEL,
        }
    }

    /// Returns the default Japanese recognizer model for the current PP-OCR variant.
    pub(in crate::subtitle_ocr) fn default_japanese_rec_spec(self) -> &'static ModelSpec {
        match self {
            PpOcrVariant::V3 => &PPOCR_V3_JAPANESE_REC_MODEL,
            PpOcrVariant::V4 => &PPOCR_V4_JAPANESE_REC_MODEL,
        }
    }

    /// Returns the default Korean recognizer model for the current PP-OCR variant.
    pub(in crate::subtitle_ocr) fn default_korean_rec_spec(self) -> &'static ModelSpec {
        match self {
            PpOcrVariant::V3 => &PPOCR_V3_KOREAN_REC_MODEL,
            PpOcrVariant::V4 => &PPOCR_V4_KOREAN_REC_MODEL,
        }
    }

    /// Returns the default CJK recognizer model for the current PP-OCR variant.
    pub(in crate::subtitle_ocr) fn default_cjk_rec_spec(self) -> &'static ModelSpec {
        match self {
            PpOcrVariant::V3 => &PPOCR_V3_CJK_REC_MODEL,
            PpOcrVariant::V4 => &PPOCR_V4_CJK_REC_MODEL,
        }
    }
}

pub(in crate::subtitle_ocr) struct PpOcrEngine {
    pub(in crate::subtitle_ocr) english_ocr: OcrLite,
    pub(in crate::subtitle_ocr) latin_ocr: Option<OcrLite>,
    pub(in crate::subtitle_ocr) japanese_ocr: Option<OcrLite>,
    pub(in crate::subtitle_ocr) korean_ocr: Option<OcrLite>,
    pub(in crate::subtitle_ocr) cjk_ocr: Option<OcrLite>,
    pub(in crate::subtitle_ocr) variant: PpOcrVariant,
}

pub(in crate::subtitle_ocr) static TESSERACT_LANG_CACHE: OnceLock<Result<HashSet<String>>> =
    OnceLock::new();
pub(in crate::subtitle_ocr) static DISABLE_TESS_FALLBACK_LOGGED: AtomicBool = AtomicBool::new(false);
pub(in crate::subtitle_ocr) static FORCE_TESS_NON_ENGLISH_LOGGED: AtomicBool = AtomicBool::new(false);
