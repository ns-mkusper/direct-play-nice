//! Transcoder module root defining subsystem boundaries, shared prelude imports, and public re-exports.

pub mod app;
pub mod ffmpeg_diagnostics;
pub mod ffmpeg_ext;
pub mod h264;
pub mod helpers;
pub mod pipeline;
pub mod quality;
pub mod stream_setup;
pub mod timestamp;
pub mod verification;

#[allow(unused_imports)]
pub(crate) mod prelude {
    // Centralized imports for transcoder submodules. This keeps leaf modules
    // focused on pipeline logic instead of repeating long FFmpeg type imports.
    pub(crate) use anyhow::{anyhow, bail, Context, Result};
    pub(crate) use clap::ValueEnum;
    pub(crate) use libc::EINVAL;
    pub(crate) use log::{debug, error, info, trace, warn, Level};
    pub(crate) use rsmpeg::avcodec::{AVCodec, AVCodecContext, AVCodecRef, AVPacket};
    pub(crate) use rsmpeg::avformat::{
        AVFormatContextInput, AVFormatContextOutput, AVStreamMut, AVStreamRef,
    };
    pub(crate) use rsmpeg::avutil::{
        ra, AVAudioFifo, AVChannelLayout, AVDictionary, AVFrame, AVSamples,
    };
    pub(crate) use rsmpeg::error::RsmpegError;
    pub(crate) use rsmpeg::ffi::{self};
    pub(crate) use rsmpeg::swresample::SwrContext;
    pub(crate) use rsmpeg::swscale::SwsContext;
    pub(crate) use serde::Deserialize;
    pub(crate) use std::{
        collections::HashSet,
        convert::TryFrom,
        ffi::{CStr, CString},
        path::{Path, PathBuf},
        ptr,
        sync::atomic::{AtomicI64, Ordering},
    };

    pub(crate) use crate::devices;
    pub(crate) use crate::devices::{
        ContainerFormat, H264Level, H264Profile, Resolution, StreamingDevice,
    };
    pub(crate) use crate::ffmpeg_utils::*;
    pub(crate) use crate::gpu::*;
    pub(crate) use crate::main_retry::select_primary_video_stream_index;
    pub(crate) use crate::subtitle_ocr;
    pub(crate) use crate::throttle::acquire_slot;
    pub(crate) use crate::transcoder::app::app_convert::{convert_video_file, ConversionParams};
    pub(crate) use crate::transcoder::ffmpeg_diagnostics::*;
    pub(crate) use crate::transcoder::ffmpeg_ext::*;
    pub(crate) use crate::transcoder::h264::*;
    pub(crate) use crate::transcoder::helpers::*;
    pub(crate) use crate::transcoder::pipeline::*;
    pub(crate) use crate::transcoder::quality::*;
    pub(crate) use crate::transcoder::stream_setup::*;
    pub(crate) use crate::transcoder::timestamp::*;
    pub(crate) use crate::transcoder::verification::*;
    pub(crate) use crate::types::*;
}

pub(crate) use app::app_convert::convert_video_file;
pub(crate) use app::app_entry::run;
pub(crate) use ffmpeg_diagnostics::configure_ffmpeg_logging;
pub(crate) use h264::*;
pub(crate) use pipeline::*;
pub(crate) use quality::{AudioQuality, VideoCodecPreference, VideoQuality};
