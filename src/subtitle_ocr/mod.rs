use anyhow::{anyhow, bail, Context, Result};
use log::{debug, info, warn};
#[cfg(any(target_os = "linux", target_os = "windows"))]
use ort::execution_providers::cuda::{CUDAExecutionProvider, CuDNNConvAlgorithmSearch};
#[cfg(any(target_os = "linux", target_os = "windows"))]
use ort::execution_providers::ArenaExtendStrategy;
#[cfg(target_vendor = "apple")]
use ort::execution_providers::CoreMLExecutionProvider;
#[cfg(target_os = "windows")]
use ort::execution_providers::DirectMLExecutionProvider;
use ort::execution_providers::{
    CPUExecutionProvider, ExecutionProvider, ExecutionProviderDispatch,
};
use ort::session::builder::SessionBuilder;
use paddle_ocr_rs::ocr_lite::OcrLite;
use rsmpeg::avcodec::{AVCodec, AVCodecContext, AVPacket};
use rsmpeg::avformat::{AVFormatContextInput, AVFormatContextOutput};
use rsmpeg::avutil::{ra, AVDictionary};
use rsmpeg::ffi;
use sha2::{Digest, Sha256};
use std::collections::HashSet;
use std::env;
use std::ffi::{CStr, CString};
use std::fs;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::{
    atomic::{AtomicBool, AtomicUsize, Ordering},
    Arc, OnceLock,
};
use std::thread;

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

include!("engine_setup.rs");
include!("muxing.rs");
include!("ocr_pipeline.rs");
include!("text_render.rs");
include!("language.rs");
include!("fixture_eval.rs");

#[cfg(test)]
mod tests;
