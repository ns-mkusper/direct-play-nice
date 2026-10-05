//! Binary entrypoint for process setup and dispatch into the conversion/probe command flow.

use anyhow::{Context, Result};
use clap::{CommandFactory, FromArgMatches};
use std::env;

mod cli;
mod config;
mod config_merge;
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
mod upscale;

pub(crate) use ffmpeg_utils::Args;
pub(crate) use transcoder::{
    configure_ffmpeg_logging, AudioQuality, VideoCodecPreference, VideoQuality,
};
pub(crate) use types::{
    OcrEngine, OcrFormat, PrimaryVideoCriteria, ResizeBackend, ResizeQuality,
    ServarrLanguageAuditScope, ServarrLanguageCandidatePolicy, SubMode, SubtitleFailurePolicy,
    UnsupportedVideoPolicy,
};

#[cfg(test)]
pub(crate) use transcoder::prelude::*;

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

    let outcome = transcoder::run(args, matches_snapshot);
    finish(outcome)
}

/// Reports the outcome and exits. On macOS, ONNX Runtime 1.21 and newer abort
/// inside their own static destructors at process exit (libc++ "mutex lock
/// failed: Invalid argument"), after all of our work is done. When an ONNX
/// Runtime environment was created, leave through `_exit` so that teardown
/// never runs; every file promotion and cleanup has already happened by then.
fn finish(outcome: Result<()>) -> Result<()> {
    if !(cfg!(target_os = "macos") && upscale::ort_initialised()) {
        return outcome;
    }
    use std::io::Write;
    let code = match &outcome {
        Ok(()) => 0,
        Err(err) => {
            eprintln!("Error: {err:?}");
            1
        }
    };
    let _ = std::io::stdout().flush();
    let _ = std::io::stderr().flush();
    log::logger().flush();
    unsafe { libc::_exit(code) }
}
