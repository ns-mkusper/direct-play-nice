//! Subtitle OCR subsystem for bitmap subtitle streams.
//!
//! This module owns end-to-end OCR conversion used by the transcoder path:
//! discover bitmap subtitle streams, run OCR, and remux text subtitles.
//! It is the integration layer over `engine`, `ocr_pipeline`, `text_processing`,
//! `text_render`, `language`, and `muxing`.

use anyhow::{anyhow, bail, Context, Result};
use log::{debug, info, warn};
use rsmpeg::avcodec::{AVCodec, AVCodecContext, AVPacket};
use rsmpeg::avformat::{AVFormatContextInput, AVFormatContextOutput};
use rsmpeg::avutil::{ra, AVDictionary};
use rsmpeg::ffi;
#[cfg(test)]
use sha2::Digest;
#[cfg(test)]
use sha2::Sha256;
use std::collections::HashSet;
use std::env;
use std::ffi::{CStr, CString};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::Ordering;

use crate::{OcrEngine, OcrFormat, SubMode};
use text_processing::{
    is_english_language, language_uses_spaces, lines_text_for_quality, ocr_text_quality_score,
    postprocess_ocr_text, ppocr_average_confidence, ppocr_needs_quality_fallback,
    prune_impossible_geometry,
};
#[cfg(test)]
use text_processing::{ppocr_spacing_needs_fallback, split_glued_ascii_token};

mod text_processing;

#[derive(Debug, Clone)]
pub struct OcrSubtitleTrack {
    pub language: String,
    pub subtitle_path: PathBuf,
    pub format: OcrFormat,
}

mod engine;
mod fixture_eval;
mod language;
mod muxing;
mod ocr_pipeline;
mod text_render;

pub(crate) use engine::convert_bitmap_subtitles;
use engine::*;
pub(crate) use fixture_eval::{evaluate_ocr_fixture_accuracy, render_ocr_fixture_report_markdown};
use language::*;
pub(crate) use muxing::{mux_text_tracks_from, remux_copy_streams};
#[cfg(test)]
use ocr_pipeline::{quality_fallback_thresholds, OcrQualityBaseline};
use text_render::*;

#[cfg(test)]
mod tests;
