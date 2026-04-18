use anyhow::{anyhow, bail, Context, Result};
use clap::parser::ValueSource;
use clap::{value_parser, ArgMatches, CommandFactory, FromArgMatches, Parser, ValueEnum};
use devices::{ContainerFormat, DeviceFamily, H264Level, H264Profile, Resolution, StreamingDevice};
use libc::EINVAL;
use log::{debug, error, info, trace, warn, Level};
use logging::log_relevant_env;
use rsmpeg::avcodec::{AVCodec, AVCodecContext, AVCodecRef, AVPacket};
use rsmpeg::avformat::{AVFormatContextInput, AVFormatContextOutput, AVStreamMut, AVStreamRef};
use rsmpeg::avutil::{ra, AVAudioFifo, AVChannelLayout, AVDictionary, AVFrame, AVSamples};
use rsmpeg::error::RsmpegError;
use rsmpeg::ffi::{self};
use rsmpeg::swresample::SwrContext;
use rsmpeg::swscale::SwsContext;
use serde::{Deserialize, Serialize};
use servarr::{ArgsView as ServeArrArgsView, IntegrationPreparation, ReplacePlan};
use std::{
    collections::{HashMap, HashSet},
    convert::TryFrom,
    env,
    ffi::{c_char, CStr, CString},
    fs,
    os::raw::c_void,
    path::{Path, PathBuf},
    ptr,
    sync::atomic::{AtomicI64, Ordering},
};

mod config;
mod devices;
mod ffmpeg_utils;
mod gpu;
mod logging;
mod main_probe;
mod main_retry;
mod main_sidecar;
#[cfg(test)]
mod main_tests;
mod plex;
mod servarr;
mod subtitle_ocr;
mod throttle;
mod transcoder;
mod types;

use gpu::{
    acquire_hw_device, find_hw_encoder, gather_probe_json, print_probe, print_probe_codecs, HwAccel,
};
use main_probe::{gather_streams_info_json, print_streams_info};
use main_retry::{
    cleanup_partial_output, handle_hw_encoder_init_error, handle_hw_profile_mismatch,
    retry_with_software_encoder, select_primary_video_stream_index,
};
use main_sidecar::post_process_ocr_subtitles;
#[cfg(test)]
use main_sidecar::{sidecar_path_for_track, write_ocr_srt_sidecars};
use throttle::acquire_slot;

pub(crate) use ffmpeg_utils::*;
pub(crate) use transcoder::*;
pub(crate) use types::*;

fn main() -> Result<()> {
    if env::var_os("RUST_LOG").is_none() {
        env::set_var("RUST_LOG", "info");
    }
    let _ = env_logger::Builder::from_default_env()
        .format_timestamp(None)
        .target(env_logger::Target::Stderr)
        .try_init();

    configure_ffmpeg_logging();

    let mut matches = Args::command().get_matches();
    let matches_snapshot = matches.clone();
    let args = Args::from_arg_matches_mut(&mut matches).context("Failed to parse CLI arguments")?;

    transcoder::run(args, matches_snapshot)
}
