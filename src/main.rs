use anyhow::{Context, Result};
use clap::{CommandFactory, FromArgMatches};
use std::env;

mod cli;
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

pub(crate) use ffmpeg_utils::Args;
pub(crate) use transcoder::{
    configure_ffmpeg_logging, AudioQuality, VideoCodecPreference, VideoQuality,
};
pub(crate) use types::{
    OcrEngine, OcrFormat, PrimaryVideoCriteria, SubMode, UnsupportedVideoPolicy,
};

#[cfg(test)]
pub(crate) use transcoder::prelude::*;

/// Runs the main operation.
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
