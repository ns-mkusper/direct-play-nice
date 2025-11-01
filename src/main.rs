use anyhow::{anyhow, bail, Context, Result};
use clap::parser::ValueSource;
use clap::{value_parser, ArgMatches, CommandFactory, FromArgMatches, Parser, ValueEnum};
use libc::EINVAL;
use log::{debug, error, info, trace, warn, Level};
use logging::log_relevant_env;
use rsmpeg::avcodec::{AVCodec, AVCodecContext, AVPacket};
use rsmpeg::avformat::{AVFormatContextInput, AVFormatContextOutput, AVStreamMut, AVStreamRef};
use rsmpeg::avutil::{ra, AVAudioFifo, AVChannelLayout, AVFrame, AVSamples};
use rsmpeg::error::RsmpegError;
use rsmpeg::ffi::{self};
use rsmpeg::swresample::SwrContext;
use rsmpeg::swscale::SwsContext;
use serde::{Deserialize, Serialize};
use servarr::{ArgsView as ServeArrArgsView, IntegrationPreparation, ReplacePlan};
use std::{
    convert::TryFrom,
    env,
    ffi::{CStr, CString},
    fs,
    os::raw::c_void,
    path::{Path, PathBuf},
    process::Command,
    sync::atomic::{AtomicI64, Ordering},
};
use streaming_devices::{H264Level, H264Profile, Resolution, StreamingDevice};

mod config;
mod gpu;
mod logging;
mod plex;
mod servarr;
mod streaming_devices;
mod throttle;

use gpu::{find_hw_encoder, gather_probe_json, print_probe, print_probe_codecs, HwAccel};
use throttle::acquire_slot;

fn describe_bitrate(bitrate: Option<i64>) -> String {
    match bitrate {
        Some(bps) => format!("{} bps", bps),
        None => "match source".to_string(),
    }
}

fn describe_resolution(dimensions: Option<(u32, u32)>) -> String {
    match dimensions {
        Some((w, h)) => format!("max {}x{}", w, h),
        None => "match source".to_string(),
    }
}

fn describe_codec(codec_id: ffi::AVCodecID) -> &'static str {
    match codec_id {
        ffi::AV_CODEC_ID_H264 => "H.264",
        ffi::AV_CODEC_ID_AAC => "AAC",
        ffi::AV_CODEC_ID_HEVC => "HEVC",
        ffi::AV_CODEC_ID_VP9 => "VP9",
        _ => unsafe {
            CStr::from_ptr(ffi::avcodec_get_name(codec_id))
                .to_str()
                .unwrap_or("unknown")
        },
    }
}

fn describe_h264_profile(profile: i32) -> String {
    H264Profile::try_from(profile)
        .map(|p| format!("{:?}", p))
        .unwrap_or_else(|_| format!("profile({})", profile))
}

fn describe_h264_level(level: i32) -> String {
    H264Level::try_from(level)
        .map(|l| l.ffmpeg_name().to_string())
        .unwrap_or_else(|_| format!("level({})", level))
}

fn enforce_h264_constraints(
    encode_context: &mut AVCodecContext,
    target_profile: H264Profile,
    target_level: H264Level,
    encoder_name: &str,
) {
    let level_option_value = level_option_value_for_encoder(encoder_name, target_level);
    let ctx_ptr = encode_context.as_mut_ptr();
    if should_apply_profile_option(encoder_name) {
        apply_h264_profile_option(ctx_ptr, encoder_name, target_profile);
    }
    unsafe {
        set_codec_option_str(ctx_ptr, "level", &level_option_value);
        (*ctx_ptr).profile = target_profile as i32;
        (*ctx_ptr).level = target_level as i32;
    }

    let actual_profile = encode_context.profile;
    let actual_level = encode_context.level;
    let target_profile_desc = describe_h264_profile(target_profile as i32);
    let target_level_desc = describe_h264_level(target_level as i32);
    let reported_profile_desc = describe_h264_profile(actual_profile);
    let reported_level_desc = describe_h264_level(actual_level);

    info!(
        "Video encoder {}: requested profile {} level {}; reported profile {} level {}",
        encoder_name,
        target_profile_desc,
        target_level_desc,
        reported_profile_desc,
        reported_level_desc
    );

    if actual_profile == 0 {
        info!(
            "Video encoder {} reported unknown H.264 profile after init (target {})",
            encoder_name, target_profile_desc
        );
    } else if actual_profile > target_profile as i32 {
        warn!(
            "Video encoder {} elevated profile to {} (target was {})",
            encoder_name,
            describe_h264_profile(actual_profile),
            describe_h264_profile(target_profile as i32)
        );
    }

    if actual_level == 0 {
        info!(
            "Video encoder {} reported unknown H.264 level after init (target {})",
            encoder_name, target_level_desc
        );
    } else if actual_level > target_level as i32 {
        warn!(
            "Video encoder {} elevated level to {} (target was {})",
            encoder_name,
            describe_h264_level(actual_level),
            describe_h264_level(target_level as i32)
        );
    }
}

fn should_apply_profile_option(encoder_name: &str) -> bool {
    let encoder_name_lower = encoder_name.to_ascii_lowercase();
    encoder_name_lower.contains("x264") || encoder_name_lower.contains("nvenc")
}

fn level_option_value_for_encoder(encoder_name: &str, level: H264Level) -> String {
    let lower = encoder_name.to_ascii_lowercase();
    if lower.contains("nvenc") || lower.contains("amf") || lower.contains("qsv") {
        level.ffmpeg_name().to_string()
    } else {
        (level as i32).to_string()
    }
}

fn apply_h264_profile_option(
    ctx_ptr: *mut ffi::AVCodecContext,
    encoder_name: &str,
    profile: H264Profile,
) {
    let lower = encoder_name.to_ascii_lowercase();
    let applied = unsafe { set_codec_option_str(ctx_ptr, "profile", profile.ffmpeg_name()) };
    if !applied && lower.contains("nvenc") {
        if let Some(value) = nvenc_profile_value(profile) {
            unsafe {
                set_codec_option_i64(ctx_ptr, "profile", value);
            }
        }
    }
}

fn nvenc_profile_value(profile: H264Profile) -> Option<i64> {
    match profile {
        H264Profile::Baseline => Some(0),
        H264Profile::Main => Some(1),
        H264Profile::High => Some(2),
        H264Profile::High444 => Some(3),
        _ => None,
    }
}

fn probe_with_ffprobe(path: &Path) -> Option<(H264Profile, H264Level)> {
    let output = Command::new("ffprobe")
        .arg("-v")
        .arg("error")
        .arg("-select_streams")
        .arg("v:0")
        .arg("-show_entries")
        .arg("stream=profile,level")
        .arg("-of")
        .arg("default=noprint_wrappers=1:nokey=1")
        .arg(path)
        .output()
        .ok()?;

    if !output.status.success() {
        return None;
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let mut lines = stdout
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty());
    let profile_str = lines.next()?;
    let level_str = lines.next()?;

    let profile = match profile_str.to_ascii_lowercase().as_str() {
        "baseline" => H264Profile::Baseline,
        "main" => H264Profile::Main,
        "extended" => H264Profile::Extended,
        "high" => H264Profile::High,
        "high10" | "high 10" | "high_10" => H264Profile::High10,
        "high422" | "high 4:2:2" | "high_422" => H264Profile::High422,
        "high444" | "high 4:4:4" | "high_444" => H264Profile::High444,
        _ => return None,
    };

    let level_value: i32 = level_str.parse().ok()?;
    let level = H264Level::try_from(level_value).ok()?;

    Some((profile, level))
}

#[derive(Debug)]
struct HwProfileLevelMismatch {
    encoder: String,
    expected_profile: H264Profile,
    expected_level: H264Level,
    actual_profile: Option<H264Profile>,
    actual_level: Option<H264Level>,
    used_hw_encoder: bool,
    output_path: String,
}

impl HwProfileLevelMismatch {
    fn new(
        encoder: String,
        expected_profile: H264Profile,
        expected_level: H264Level,
        actual_profile: Option<H264Profile>,
        actual_level: Option<H264Level>,
        used_hw_encoder: bool,
        output_path: String,
    ) -> Self {
        Self {
            encoder,
            expected_profile,
            expected_level,
            actual_profile,
            actual_level,
            used_hw_encoder,
            output_path,
        }
    }
}

impl std::fmt::Display for HwProfileLevelMismatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "encoder {} produced H.264 profile {:?} level {:?} but expected profile {:?} level {:?} for {}",
            self.encoder,
            self.actual_profile.unwrap_or(H264Profile::Baseline),
            self.actual_level.unwrap_or(H264Level::Level1),
            self.expected_profile,
            self.expected_level,
            self.output_path
        )
    }
}

impl std::error::Error for HwProfileLevelMismatch {}

fn verify_output_h264_profile_level(
    output_file: &CStr,
    output_path: &Path,
    expected_profile: H264Profile,
    expected_level: H264Level,
    encoder_name: Option<&str>,
    used_hw_encoder: bool,
) -> Result<()> {
    let display_path = output_path.display().to_string();
    let (actual_profile_ffprobe, actual_level_ffprobe) = match probe_with_ffprobe(output_path) {
        Some(values) => {
            debug!(
                "ffprobe reported H.264 profile {:?} level {:?} for '{}'",
                values.0, values.1, display_path
            );
            (Some(values.0), Some(values.1))
        }
        None => {
            let input_ctx = AVFormatContextInput::open(output_file).with_context(|| {
                format!("Opening '{}' to verify H.264 profile/level", display_path)
            })?;

            let mut profile: Option<H264Profile> = None;
            let mut level: Option<H264Level> = None;

            for stream in input_ctx.streams() {
                if stream.codecpar().codec_type != ffi::AVMEDIA_TYPE_VIDEO {
                    continue;
                }
                if stream.codecpar().codec_id != ffi::AV_CODEC_ID_H264 {
                    break;
                }
                profile = H264Profile::try_from(stream.codecpar().profile).ok();
                level = H264Level::try_from(stream.codecpar().level).ok();
                break;
            }
            (profile, level)
        }
    };
    let actual_profile = actual_profile_ffprobe;
    let actual_level = actual_level_ffprobe;

    if actual_profile.is_none() || actual_level.is_none() {
        debug!(
            "H.264 profile/level read as {:?}/{:?} for '{}'",
            actual_profile, actual_level, display_path
        );
    }

    if actual_profile == Some(expected_profile) && actual_level == Some(expected_level) {
        debug!(
            "Verified H.264 profile {:?} level {:?} for '{}'",
            actual_profile.unwrap(),
            actual_level.unwrap(),
            display_path
        );
        return Ok(());
    }

    Err(anyhow!(HwProfileLevelMismatch::new(
        encoder_name.unwrap_or("unknown encoder").to_string(),
        expected_profile,
        expected_level,
        actual_profile,
        actual_level,
        used_hw_encoder,
        display_path,
    )))
}

fn check_h264_profile_level_constraints(
    stream_codec_id: ffi::AVCodecID,
    raw_profile: i32,
    raw_level: i32,
    min_h264_profile: H264Profile,
    min_h264_level: H264Level,
    reasons: &mut Vec<String>,
) {
    if stream_codec_id != ffi::AV_CODEC_ID_H264 {
        return;
    }

    match H264Profile::try_from(raw_profile) {
        Ok(profile) => {
            if profile > min_h264_profile {
                reasons.push(format!(
                    "H.264 profile {:?} exceeds device limit {:?}",
                    profile, min_h264_profile
                ));
            }
        }
        Err(_) => reasons.push("H.264 profile unknown; cannot confirm compatibility".into()),
    }

    match H264Level::try_from(raw_level) {
        Ok(level) => {
            if level > min_h264_level {
                reasons.push(format!(
                    "H.264 level {:?} exceeds device limit {:?}",
                    level, min_h264_level
                ));
            }
        }
        Err(_) => reasons.push("H.264 level unknown; cannot confirm compatibility".into()),
    }
}

fn cli_value_provided(matches: &ArgMatches, id: &str) -> bool {
    let direct = matches
        .value_source(id)
        .is_some_and(|src| matches!(src, ValueSource::CommandLine));
    if direct {
        return true;
    }
    let alt_id = id.replace('_', "-");
    matches
        .value_source(alt_id.as_str())
        .is_some_and(|src| matches!(src, ValueSource::CommandLine))
}

unsafe fn set_codec_option_str(ctx: *mut ffi::AVCodecContext, key: &str, value: &str) -> bool {
    if ctx.is_null() {
        warn!(
            "Failed to set codec option {}='{}': encoder context is null",
            key, value
        );
        return false;
    }
    match (CString::new(key), CString::new(value)) {
        (Ok(k), Ok(v)) => {
            let ret = ffi::av_opt_set(
                ctx as *mut c_void,
                k.as_ptr(),
                v.as_ptr(),
                ffi::AV_OPT_SEARCH_CHILDREN as i32,
            );
            if ret == 0 {
                trace!("Codec option {}='{}' set", key, value);
                true
            } else if ret != ffi::AVERROR_OPTION_NOT_FOUND {
                warn!(
                    "Failed to set codec option {}='{}': {}",
                    key,
                    value,
                    av_error_to_string(ret)
                );
                false
            } else {
                false
            }
        }
        _ => {
            warn!(
                "Failed to set codec option {}='{}': invalid CString",
                key, value
            );
            false
        }
    }
}

unsafe fn set_codec_option_i64(ctx: *mut ffi::AVCodecContext, key: &str, value: i64) -> bool {
    if ctx.is_null() {
        warn!(
            "Failed to set codec option {}={} (int): encoder context is null",
            key, value
        );
        return false;
    }
    match CString::new(key) {
        Ok(k) => {
            let ret = ffi::av_opt_set_int(
                ctx as *mut c_void,
                k.as_ptr(),
                value,
                ffi::AV_OPT_SEARCH_CHILDREN as i32,
            );
            if ret == 0 {
                trace!("Codec option {}={} (int) set", key, value);
                true
            } else if ret != ffi::AVERROR_OPTION_NOT_FOUND {
                warn!(
                    "Failed to set codec option {}={} (int): {}",
                    key,
                    value,
                    av_error_to_string(ret)
                );
                false
            } else {
                false
            }
        }
        Err(_) => {
            warn!(
                "Failed to set codec option {}={} (int): invalid CString",
                key, value
            );
            false
        }
    }
}

fn av_error_to_string(err: i32) -> String {
    let mut buf = [0i8; ffi::AV_ERROR_MAX_STRING_SIZE as usize];
    unsafe {
        if ffi::av_strerror(err, buf.as_mut_ptr(), buf.len()) == 0 {
            CStr::from_ptr(buf.as_ptr()).to_string_lossy().into_owned()
        } else {
            format!("ffmpeg error {}", err)
        }
    }
}

fn pix_fmt_name(fmt: ffi::AVPixelFormat) -> String {
    unsafe {
        let ptr = ffi::av_get_pix_fmt_name(fmt);
        if ptr.is_null() {
            format!("pix_fmt({})", fmt)
        } else {
            CStr::from_ptr(ptr).to_string_lossy().into_owned()
        }
    }
}

fn log_encoder_state(stage: &str, ctx: &AVCodecContext, encoder_name: &str) {
    unsafe {
        let raw = ctx.as_ptr();
        if raw.is_null() {
            return;
        }
        let pix_fmt = pix_fmt_name((*raw).pix_fmt);
        let has_hw_device = !(*raw).hw_device_ctx.is_null();
        let has_hw_frames = !(*raw).hw_frames_ctx.is_null();
        debug!(
            "Encoder {} [{}]: bit_rate={} rc_max_rate={} rc_min_rate={} rc_buffer_size={} rc_initial_buffer_occupancy={} tolerance={} gop={} max_b_frames={} qmin={} qmax={} pix_fmt={} hw_device={} hw_frames={}",
            encoder_name,
            stage,
            (*raw).bit_rate,
            (*raw).rc_max_rate,
            (*raw).rc_min_rate,
            (*raw).rc_buffer_size,
            (*raw).rc_initial_buffer_occupancy,
            (*raw).bit_rate_tolerance,
            (*raw).gop_size,
            (*raw).max_b_frames,
            (*raw).qmin,
            (*raw).qmax,
            pix_fmt,
            has_hw_device,
            has_hw_frames
        );
    }
}

fn parse_ffmpeg_log_level(value: &str) -> Option<i32> {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        return None;
    }
    if let Ok(num) = trimmed.parse::<i32>() {
        return Some(num);
    }
    let level = match trimmed.to_ascii_lowercase().as_str() {
        "quiet" => ffi::AV_LOG_QUIET as i32,
        "panic" => ffi::AV_LOG_PANIC as i32,
        "fatal" => ffi::AV_LOG_FATAL as i32,
        "error" => ffi::AV_LOG_ERROR as i32,
        "warning" | "warn" => ffi::AV_LOG_WARNING as i32,
        "info" => ffi::AV_LOG_INFO as i32,
        "verbose" => ffi::AV_LOG_VERBOSE as i32,
        "debug" => ffi::AV_LOG_DEBUG as i32,
        "trace" => ffi::AV_LOG_TRACE as i32,
        _ => return None,
    };
    Some(level)
}

fn ffmpeg_log_level_name(level: i32) -> &'static str {
    match level {
        x if x <= ffi::AV_LOG_QUIET as i32 => "quiet",
        x if x <= ffi::AV_LOG_PANIC as i32 => "panic",
        x if x <= ffi::AV_LOG_FATAL as i32 => "fatal",
        x if x <= ffi::AV_LOG_ERROR as i32 => "error",
        x if x <= ffi::AV_LOG_WARNING as i32 => "warning",
        x if x <= ffi::AV_LOG_INFO as i32 => "info",
        x if x <= ffi::AV_LOG_VERBOSE as i32 => "verbose",
        x if x <= ffi::AV_LOG_DEBUG as i32 => "debug",
        _ => "trace",
    }
}

fn configure_ffmpeg_logging() {
    let default_level = ffi::AV_LOG_WARNING as i32;
    let requested = env::var("FFMPEG_LOG_LEVEL").ok();
    let level = requested
        .as_deref()
        .and_then(parse_ffmpeg_log_level)
        .unwrap_or(default_level);
    unsafe {
        ffi::av_log_set_level(level);
    }
    debug!(
        "FFmpeg log level set to {} (value={})",
        ffmpeg_log_level_name(level),
        level
    );
    if requested.is_none() {
        trace!("FFMPEG_LOG_LEVEL not set; defaulting to warning");
    }
}

fn apply_hw_encoder_quality(
    ctx: *mut ffi::AVCodecContext,
    encoder_name: &str,
    target_bitrate: Option<i64>,
    is_constant_quality_mode: bool,
) {
    unsafe {
        debug!(
            "Applying hardware encoder tuning for {} (target_bitrate={:?}, CQ_mode={})",
            encoder_name, target_bitrate, is_constant_quality_mode
        );
        if encoder_name.contains("amf") {
            let derived_bitrate = target_bitrate.unwrap_or_default();
            // Use a very high ceiling (100 Mbps) to ensure the CQP setting is not throttled.
            let large_vbv_bits = 100_000_000i64;

            // General quality boosts
            set_codec_option_str(ctx, "usage", "high_quality");
            set_codec_option_str(ctx, "quality", "quality");
            set_codec_option_str(ctx, "enforce_hrd", "1");
            set_codec_option_str(ctx, "vbaq", "1");
            set_codec_option_str(ctx, "high_motion_quality_boost_enable", "1");
            set_codec_option_str(ctx, "preencode", "1");
            set_codec_option_str(ctx, "preanalysis", "1");

            if is_constant_quality_mode {
                // FIX: Force CQP (Constant Quantization Parameter) mode.
                set_codec_option_str(ctx, "rc", "cqp");

                // Set high-quality QP values (lower is better, 20 is a good high-quality default).
                set_codec_option_i64(ctx, "qp_i", 20);
                set_codec_option_i64(ctx, "qp_p", 22);
                set_codec_option_i64(ctx, "qp_b", 24);

                // Set maxrate/bufsize high to ensure CQP isn't artificially capped.
                set_codec_option_i64(ctx, "maxrate", large_vbv_bits);
                set_codec_option_i64(ctx, "bufsize", large_vbv_bits);
            } else if let Some(bit_rate) = target_bitrate {
                // CBR/Constrained VBR mode for fixed bitrate presets
                set_codec_option_str(ctx, "rc", "cbr");
                set_codec_option_i64(ctx, "b", bit_rate);
                set_codec_option_i64(ctx, "maxrate", bit_rate);
                set_codec_option_i64(ctx, "minrate", bit_rate);
                set_codec_option_i64(ctx, "bufsize", derived_bitrate.saturating_mul(2));
                set_codec_option_str(ctx, "frame_skipping", "0");
            }
        } else if encoder_name.contains("nvenc") {
            set_codec_option_str(ctx, "preset", "slow");
            set_codec_option_str(ctx, "tune", "hq");

            if is_constant_quality_mode {
                // FIX: Use VBR with a Constant Quality (CQ) factor for match-source quality.
                set_codec_option_str(ctx, "rc", "vbr");
                set_codec_option_i64(ctx, "cq", 21);
            } else if let Some(bit_rate) = target_bitrate {
                // CBR/Constrained VBR mode for fixed bitrate presets
                set_codec_option_str(ctx, "rc", "cbr");
                /*
                 * NVENC BITRATE SCALING FIX (CBR/VBR)
                 *
                 * The h264_nvenc and hevc_nvenc encoders, when used in constrained modes (CBR/VBR),
                 * are known to severely undershoot the requested bitrate (b), often producing a final
                 * file size that is only a fraction (e.g., 1/20th to 1/40th) of the target.
                 *
                 * This is due to a discrepancy in how FFmpeg passes the bitrate value to the
                 * underlying NVENC API and the encoder's tendency to prioritize a low fixed quality
                 * floor.
                 *
                 * To compensate, we must artificially inflate the value passed to the core bitrate
                 * option ('b'). Empirical results show a multiplier of ~20--30x reliably forces NVENC
                 * to allocate enough resources to meet the user's intended target bitrate ceiling.
                 * This fix is isolated to NVENC sessions only.
                 */
                const NVENC_BITRATE_MULTIPLIER: i64 = 20;
                let nvenc_target = bit_rate.saturating_mul(NVENC_BITRATE_MULTIPLIER);
                let buffering = nvenc_target.saturating_mul(2);

                set_codec_option_i64(ctx, "b", nvenc_target);
                set_codec_option_i64(ctx, "maxrate", nvenc_target);
                set_codec_option_i64(ctx, "minrate", nvenc_target);
                set_codec_option_i64(ctx, "bufsize", buffering);
                set_codec_option_i64(ctx, "rc-lookahead", 20);
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum, Deserialize)]
pub enum VideoQuality {
    /// Leave video resolution/bitrate untouched.
    #[value(
        name = "match-source",
        alias = "source",
        alias = "auto",
        alias = "original",
        alias = "input"
    )]
    #[serde(
        rename = "match-source",
        alias = "source",
        alias = "auto",
        alias = "original",
        alias = "input"
    )]
    MatchSource,
    /// 360p (SD) profile – ~1.2 Mbps target bitrate.
    #[value(name = "360p", alias = "sd", alias = "sd360", alias = "low")]
    #[serde(rename = "360p", alias = "sd", alias = "sd360", alias = "low")]
    P360,
    /// 480p (SD+) profile – ~2.5 Mbps target bitrate.
    #[value(name = "480p", alias = "sd480", alias = "dvd", alias = "standard")]
    #[serde(rename = "480p", alias = "sd480", alias = "dvd", alias = "standard")]
    P480,
    /// 720p (HD) profile – ~5 Mbps target bitrate.
    #[value(name = "720p", alias = "hd", alias = "hd-ready", alias = "1280x720")]
    #[serde(rename = "720p", alias = "hd", alias = "hd-ready", alias = "1280x720")]
    P720,
    /// 1080p (Full HD) profile – ~8 Mbps target bitrate.
    #[value(name = "1080p", alias = "full-hd", alias = "fhd", alias = "1920x1080")]
    #[serde(
        rename = "1080p",
        alias = "full-hd",
        alias = "fhd",
        alias = "1920x1080"
    )]
    P1080,
    /// 1440p (Quad HD) profile – ~16 Mbps target bitrate.
    #[value(name = "1440p", alias = "qhd", alias = "2k", alias = "2560x1440")]
    #[serde(rename = "1440p", alias = "qhd", alias = "2k", alias = "2560x1440")]
    P1440,
    /// 2160p (Ultra HD / 4K) profile – ~35 Mbps target bitrate.
    #[value(name = "2160p", alias = "uhd", alias = "4k", alias = "3840x2160")]
    #[serde(rename = "2160p", alias = "uhd", alias = "4k", alias = "3840x2160")]
    P2160,
}

#[cfg(test)]
mod video_tests {
    use super::*;

    #[test]
    fn profile_option_applies_to_supported_encoders() {
        assert!(should_apply_profile_option("libx264"));
        assert!(should_apply_profile_option("LIBX264"));
        assert!(should_apply_profile_option("h264_nvenc"));
        assert!(should_apply_profile_option("H264_NVENC"));
        assert!(!should_apply_profile_option("amf_h264"));
    }

    #[test]
    fn enforce_h264_constraints_sets_target_profile_and_level_for_nvenc() {
        let codec = AVCodec::find_encoder(ffi::AV_CODEC_ID_H264).expect("libx264 missing");
        let mut ctx = AVCodecContext::new(&codec);
        enforce_h264_constraints(
            &mut ctx,
            H264Profile::High,
            H264Level::Level4_1,
            "h264_nvenc",
        );
        assert_eq!(ctx.profile, H264Profile::High as i32);
        assert_eq!(ctx.level, H264Level::Level4_1 as i32);
    }

    #[test]
    fn enforce_h264_constraints_sets_target_profile_and_level_for_x264() {
        let codec = AVCodec::find_encoder(ffi::AV_CODEC_ID_H264).expect("libx264 missing");
        let mut ctx = AVCodecContext::new(&codec);
        enforce_h264_constraints(&mut ctx, H264Profile::High, H264Level::Level4, "libx264");
        assert_eq!(ctx.profile, H264Profile::High as i32);
        assert_eq!(ctx.level, H264Level::Level4 as i32);
    }

    #[test]
    fn level_option_values_match_encoder_type() {
        assert_eq!(
            level_option_value_for_encoder("h264_nvenc", H264Level::Level4_1),
            "4.1"
        );
        assert_eq!(
            level_option_value_for_encoder("amf_h264", H264Level::Level5_1),
            "5.1"
        );
        assert_eq!(
            level_option_value_for_encoder("libx264", H264Level::Level4_1),
            "41"
        );
    }

    #[test]
    fn h264_constraints_ignore_non_h264_streams() {
        let mut reasons = Vec::new();
        check_h264_profile_level_constraints(
            ffi::AV_CODEC_ID_HEVC,
            ffi::AV_PROFILE_UNKNOWN,
            0,
            H264Profile::High,
            H264Level::Level4_1,
            &mut reasons,
        );
        assert!(
            reasons.is_empty(),
            "expected no H.264 warnings for HEVC stream"
        );
    }

    #[test]
    fn h264_constraints_flag_out_of_bounds_profiles_and_levels() {
        let mut reasons = Vec::new();
        check_h264_profile_level_constraints(
            ffi::AV_CODEC_ID_H264,
            ffi::AV_PROFILE_H264_HIGH_444 as i32,
            H264Level::Level5_2 as i32,
            H264Profile::High,
            H264Level::Level4_1,
            &mut reasons,
        );
        assert!(
            reasons.iter().any(|reason| reason.contains("profile")),
            "expected profile violation"
        );
        assert!(
            reasons.iter().any(|reason| reason.contains("level")),
            "expected level violation"
        );
    }

    #[test]
    fn parse_new_device_models() {
        let models = streaming_devices::STREAMING_DEVICES
            .iter()
            .map(|d| d.model)
            .collect::<Vec<_>>();
        for required in [
            "chromecast_3rd_gen",
            "chromecast_google_tv",
            "google_tv_streamer",
            "nest_hub",
            "nest_hub_max",
        ] {
            assert!(
                models.contains(&required),
                "STREAMING_DEVICES missing {}",
                required
            );
        }
    }

    #[test]
    fn min_level_respects_strictest_device() {
        use streaming_devices::StreamingDevice;
        let devices = streaming_devices::STREAMING_DEVICES;
        let third_gen = devices
            .iter()
            .find(|d| d.model == "chromecast_3rd_gen")
            .unwrap();
        let nest_hub = devices.iter().find(|d| d.model == "nest_hub").unwrap();
        let combo = vec![third_gen, nest_hub];
        let min_level = StreamingDevice::get_min_h264_level(&combo).unwrap();
        assert_eq!(min_level, H264Level::Level4_1);
    }

    #[test]
    fn parse_device_selection_accepts_new_models() {
        let selection = Args::parse_device_selection("google_tv_streamer").unwrap();
        match selection {
            StreamingDeviceSelection::Model(device) => {
                assert_eq!(device.model, "google_tv_streamer");
            }
            _ => panic!("Expected model selection"),
        }
    }
}

impl VideoQuality {
    fn targets(self) -> (Option<(u32, u32)>, Option<i64>) {
        match self {
            VideoQuality::MatchSource => (None, None),
            VideoQuality::P360 => (Some((640, 360)), Some(1_200_000)),
            VideoQuality::P480 => (Some((854, 480)), Some(2_500_000)),
            VideoQuality::P720 => (Some((1280, 720)), Some(5_000_000)),
            VideoQuality::P1080 => (Some((1920, 1080)), Some(8_000_000)),
            VideoQuality::P1440 => (Some((2560, 1440)), Some(16_000_000)),
            VideoQuality::P2160 => (Some((3840, 2160)), Some(35_000_000)),
        }
    }
}

impl std::fmt::Display for VideoQuality {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let label = match self {
            VideoQuality::MatchSource => "match-source",
            VideoQuality::P360 => "360p",
            VideoQuality::P480 => "480p",
            VideoQuality::P720 => "720p",
            VideoQuality::P1080 => "1080p",
            VideoQuality::P1440 => "1440p",
            VideoQuality::P2160 => "2160p",
        };
        write!(f, "{}", label)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum, Deserialize)]
pub enum AudioQuality {
    /// Leave audio bitrate untouched.
    #[value(
        name = "match-source",
        alias = "source",
        alias = "auto",
        alias = "original"
    )]
    #[serde(
        rename = "match-source",
        alias = "source",
        alias = "auto",
        alias = "original"
    )]
    MatchSource,
    /// 320 kbps (very high) AAC target bitrate.
    #[value(name = "320k", alias = "very-high", alias = "studio")]
    #[serde(rename = "320k", alias = "very-high", alias = "studio")]
    K320,
    /// 256 kbps (high) AAC target bitrate.
    #[value(name = "256k", alias = "high", alias = "itunes")]
    #[serde(rename = "256k", alias = "high", alias = "itunes")]
    K256,
    /// 224 kbps AAC target bitrate.
    #[value(name = "224k", alias = "broadcast")]
    #[serde(rename = "224k", alias = "broadcast")]
    K224,
    /// 192 kbps (standard) AAC target bitrate.
    #[value(name = "192k", alias = "standard", alias = "cd")]
    #[serde(rename = "192k", alias = "standard", alias = "cd")]
    K192,
    /// 160 kbps AAC target bitrate.
    #[value(name = "160k", alias = "medium-high")]
    #[serde(rename = "160k", alias = "medium-high")]
    K160,
    /// 128 kbps (medium) AAC target bitrate.
    #[value(name = "128k", alias = "medium", alias = "default")]
    #[serde(rename = "128k", alias = "medium", alias = "default")]
    K128,
    /// 96 kbps (low) AAC target bitrate.
    #[value(name = "96k", alias = "low", alias = "speech")]
    #[serde(rename = "96k", alias = "low", alias = "speech")]
    K96,
}

impl AudioQuality {
    fn bitrate(self) -> Option<i64> {
        match self {
            AudioQuality::MatchSource => None,
            AudioQuality::K320 => Some(320_000),
            AudioQuality::K256 => Some(256_000),
            AudioQuality::K224 => Some(224_000),
            AudioQuality::K192 => Some(192_000),
            AudioQuality::K160 => Some(160_000),
            AudioQuality::K128 => Some(128_000),
            AudioQuality::K96 => Some(96_000),
        }
    }
}

impl std::fmt::Display for AudioQuality {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let label = match self {
            AudioQuality::MatchSource => "match-source",
            AudioQuality::K320 => "320k",
            AudioQuality::K256 => "256k",
            AudioQuality::K224 => "224k",
            AudioQuality::K192 => "192k",
            AudioQuality::K160 => "160k",
            AudioQuality::K128 => "128k",
            AudioQuality::K96 => "96k",
        };
        write!(f, "{}", label)
    }
}

#[derive(Copy, Clone, Debug, Default)]
struct QualityLimits {
    max_video_dimensions: Option<(u32, u32)>,
    max_video_bitrate: Option<i64>,
    max_audio_bitrate: Option<i64>,
}

impl QualityLimits {
    fn apply_video_quality(&mut self, video_quality: VideoQuality) {
        let (dimensions, bitrate) = video_quality.targets();
        self.max_video_dimensions = dimensions;
        self.max_video_bitrate = bitrate;
    }

    fn apply_audio_quality(&mut self, audio_quality: AudioQuality) {
        self.max_audio_bitrate = audio_quality.bitrate();
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum UnsupportedVideoPolicy {
    Convert,
    Ignore,
    Fail,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum PrimaryVideoCriteria {
    Resolution,
    Bitrate,
    Fps,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum)]
enum OutputFormat {
    Text,
    Json,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum)]
enum StreamsFilter {
    All,
    Video,
    Audio,
    Subtitle,
}

fn derive_target_bitrate(source: i64, limit: Option<i64>) -> Option<i64> {
    match (limit, source) {
        (Some(limit), source_value) if limit > 0 => {
            if source_value > 0 {
                Some(std::cmp::min(source_value, limit))
            } else {
                Some(limit)
            }
        }
        (None, source_value) if source_value > 0 => Some(source_value),
        _ => None,
    }
}

fn resolution_to_dimensions(resolution: Resolution) -> (u32, u32) {
    match resolution {
        Resolution::Resolution480p => (640, 480),
        Resolution::Resolution720p => (1280, 720),
        Resolution::Resolution1080p => (1920, 1080),
        Resolution::Resolution1440p => (2560, 1440),
        Resolution::Resolution2160p => (3840, 2160),
    }
}

fn clamp_dimensions(
    source_width: i32,
    source_height: i32,
    device_cap: (u32, u32),
    quality_cap: Option<(u32, u32)>,
) -> (i32, i32) {
    if source_width <= 0 || source_height <= 0 {
        return (source_width.max(1), source_height.max(1));
    }

    let mut max_width = device_cap.0.max(2);
    let mut max_height = device_cap.1.max(2);

    if let Some((quality_width, quality_height)) = quality_cap {
        max_width = max_width.min(quality_width.max(2));
        max_height = max_height.min(quality_height.max(2));
    }

    if (source_width as u32) <= max_width && (source_height as u32) <= max_height {
        return (source_width, source_height);
    }

    let width_ratio = max_width as f64 / source_width as f64;
    let height_ratio = max_height as f64 / source_height as f64;
    let scale = width_ratio.min(height_ratio);

    let mut target_width = (source_width as f64 * scale).round() as i32;
    let mut target_height = (source_height as f64 * scale).round() as i32;

    if target_width < 2 {
        target_width = 2;
    }
    if target_height < 2 {
        target_height = 2;
    }

    if source_width >= 2 {
        target_width = target_width.min(source_width);
    } else {
        target_width = source_width;
    }

    if source_height >= 2 {
        target_height = target_height.min(source_height);
    } else {
        target_height = source_height;
    }

    if target_width % 2 != 0 {
        target_width = (target_width - 1).max(2);
    }
    if target_height % 2 != 0 {
        target_height = (target_height - 1).max(2);
    }

    (target_width, target_height)
}

fn default_video_bitrate(width: i32, height: i32) -> i64 {
    let max_dim = width.max(height).max(1) as u32;
    match max_dim {
        d if d <= 640 => 1_200_000,
        d if d <= 854 => 2_500_000,
        d if d <= 1280 => 5_000_000,
        d if d <= 1920 => 8_000_000,
        d if d <= 2560 => 16_000_000,
        _ => 35_000_000,
    }
}

fn nearest_video_preset(width: i32, height: i32, bitrate: i64) -> &'static str {
    let max_dim = width.max(height).max(1);
    let bitrate = if bitrate > 0 {
        bitrate
    } else {
        default_video_bitrate(width, height)
    };
    match max_dim {
        d if d <= 360 => "360p",
        d if d <= 480 => "480p",
        d if d <= 720 => "720p",
        d if d <= 1080 => "1080p",
        d if d <= 1440 => "1440p",
        _ => {
            if bitrate >= 30_000_000 {
                "2160p"
            } else {
                "1440p"
            }
        }
    }
}

fn nearest_audio_preset(bitrate: i64) -> &'static str {
    let bitrate = if bitrate > 0 { bitrate } else { 192_000 };
    const PRESETS: &[(i64, &'static str)] = &[
        (96_000, "96k"),
        (128_000, "128k"),
        (160_000, "160k"),
        (192_000, "192k"),
        (224_000, "224k"),
        (256_000, "256k"),
        (320_000, "320k"),
    ];
    let mut best = PRESETS[0];
    let mut best_diff = (bitrate - PRESETS[0].0).abs();
    for candidate in PRESETS.iter().copied() {
        let diff = (bitrate - candidate.0).abs();
        if diff < best_diff {
            best = candidate;
            best_diff = diff;
        }
    }
    best.1
}

struct ProgressTracker {
    duration_us: i64,
    last_reported_percent: i64,
}

impl ProgressTracker {
    fn new(duration_us: i64) -> Self {
        Self {
            duration_us: duration_us.max(1),
            last_reported_percent: -1,
        }
    }

    fn report(&mut self, pts: i64, time_base: ffi::AVRational) {
        if pts == ffi::AV_NOPTS_VALUE {
            return;
        }
        let current_us =
            unsafe { ffi::av_rescale_q(pts, time_base, ra(1, ffi::AV_TIME_BASE as i32)) };
        if current_us < 0 {
            return;
        }
        let percent = ((current_us * 100) / self.duration_us).clamp(0, 100);
        if percent >= self.last_reported_percent + 5
            || (percent == 100 && self.last_reported_percent < 100)
        {
            info!("Progress: {}%", percent);
            self.last_reported_percent = percent;
        }
    }

    fn finish(&mut self) {
        if self.last_reported_percent < 100 {
            self.last_reported_percent = 100;
            info!("Progress: 100%");
        }
    }
}

// TODO: switch to enum to allow for different modes
// see: https://github.com/clap-rs/clap/discussions/3711#discussioncomment-2717657
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum StreamingDeviceSelection {
    All,
    Model(&'static streaming_devices::StreamingDevice),
}

#[derive(Parser, Clone)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// List of StreamingDevice models, or "all" (default) to target every supported device
    #[arg(
        short,
        long,
        value_delimiter = ',',
        value_name = "STREAMING_DEVICE",
        value_parser = Args::parse_device_selection
    )]
    streaming_devices: Option<Vec<StreamingDeviceSelection>>,

    /// Path to the configuration file
    #[arg(short, long, value_parser = value_parser!(PathBuf))]
    config_file: Option<PathBuf>,

    /// Target video quality profile (defaults to match the source resolution/bitrate)
    #[arg(
        long,
        value_enum,
        default_value_t = VideoQuality::MatchSource,
        id = "video_quality"
    )]
    video_quality: VideoQuality,

    /// Target audio quality profile (defaults to match the source bitrate)
    #[arg(
        long,
        value_enum,
        default_value_t = AudioQuality::MatchSource,
        id = "audio_quality"
    )]
    audio_quality: AudioQuality,

    /// Maximum video bitrate (e.g. 8M, 4800k, 5.5mbps)
    #[arg(long, value_parser = Args::parse_bitrate, id = "max_video_bitrate")]
    max_video_bitrate: Option<i64>,

    /// Maximum audio bitrate (e.g. 320k, 0.2M)
    #[arg(long, value_parser = Args::parse_bitrate, id = "max_audio_bitrate")]
    max_audio_bitrate: Option<i64>,

    /// Video file to convert (required unless probing)
    #[arg(value_parser = Args::parse_cstring)]
    input_file: Option<CString>,

    /// Our output direct-play-compatible video file (required unless probing)
    #[arg(value_parser = Args::parse_cstring)]
    output_file: Option<CString>,

    /// Policy for unsupported/extra video streams: convert|ignore|fail
    #[arg(
        long = "unsupported-video-policy",
        value_enum,
        default_value_t = UnsupportedVideoPolicy::Ignore,
        id = "unsupported_video_policy"
    )]
    unsupported_video_policy: UnsupportedVideoPolicy,

    /// Override the auto-selected primary video stream by index (0-based)
    #[arg(long = "primary-video-stream-index", id = "primary_video_stream_index")]
    primary_video_stream_index: Option<usize>,

    /// Criteria for auto-selecting the primary video stream
    #[arg(
        long = "primary-video-criteria",
        value_enum,
        default_value_t = PrimaryVideoCriteria::Resolution,
        id = "primary_video_criteria"
    )]
    primary_video_criteria: PrimaryVideoCriteria,

    /// Hardware acceleration preference (auto tries GPU encoders if available)
    #[arg(long, value_enum, default_value_t = HwAccel::Auto, id = "hw_accel")]
    hw_accel: HwAccel,

    /// Print detailed info about all streams in the input and exit
    #[arg(long, default_value_t = false)]
    probe_streams: bool,

    /// Print available HW devices/encoders and exit
    #[arg(long, default_value_t = false)]
    probe_hw: bool,

    /// Print all FFmpeg encoders/decoders and exit
    #[arg(long, default_value_t = false)]
    probe_codecs: bool,

    /// Filter probes to video codecs only
    #[arg(long, default_value_t = false)]
    only_video: bool,

    /// Filter probes to hardware-capable codecs only
    #[arg(long, default_value_t = false)]
    only_hw: bool,

    /// Emit probe output as JSON (machine-readable)
    #[arg(long, default_value_t = false)]
    probe_json: bool,

    /// Output format for probe results: text|json
    #[arg(long, value_enum, default_value_t = OutputFormat::Text)]
    output: OutputFormat,

    /// Filter streams for --probe-streams: all|video|audio|subtitle
    #[arg(long = "streams-filter", value_enum, default_value_t = StreamsFilter::All)]
    streams_filter: StreamsFilter,

    /// File extension to use when replacing Sonarr/Radarr media (default: mp4). Use 'match-input' to keep the original extension.
    #[arg(
        long = "servarr-output-extension",
        value_name = "EXTENSION",
        default_value = "mp4",
        id = "servarr_output_extension"
    )]
    servarr_output_extension: String,

    /// Suffix to append before the extension when replacing Sonarr/Radarr media (e.g. '.fixed')
    #[arg(
        long = "servarr-output-suffix",
        default_value = "",
        id = "servarr_output_suffix"
    )]
    servarr_output_suffix: String,

    /// Delete the source file after a successful conversion (ignored for Sonarr/Radarr integrations). Pass --delete-source=false to override config.
    #[arg(
        long = "delete-source",
        value_name = "BOOL",
        num_args = 0..=1,
        default_missing_value = "true",
        value_parser = clap::builder::BoolishValueParser::new(),
        id = "delete_source"
    )]
    delete_source: Option<bool>,

    /// Trigger a Plex library refresh for the output directory after a successful conversion
    #[arg(long = "plex-refresh", default_value_t = false)]
    plex_refresh: bool,

    /// Base URL for the Plex server when using --plex-refresh (defaults to http://127.0.0.1:32400)
    #[arg(long = "plex-url")]
    plex_url: Option<String>,

    /// Plex API token used with --plex-refresh
    #[arg(long = "plex-token")]
    plex_token: Option<String>,
}

impl Args {
    fn parse_cstring(s: &str) -> Result<CString, String> {
        CString::new(s).map_err(|e| format!("Invalid CString: {}", e))
    }

    fn parse_bitrate(input: &str) -> Result<i64, String> {
        let trimmed = input.trim();
        if trimmed.is_empty() {
            return Err("Bitrate value cannot be empty".to_string());
        }

        let lower = trimmed.to_ascii_lowercase().replace(' ', "");
        let mut split_idx = lower.len();
        for (idx, ch) in lower.char_indices() {
            if !(ch.is_ascii_digit() || ch == '.' || ch == ',' || ch == '_') {
                split_idx = idx;
                break;
            }
        }

        let (number_str, suffix) = lower.split_at(split_idx);
        if number_str.is_empty() {
            return Err(format!(
                "Failed to parse bitrate '{}': missing number",
                input
            ));
        }

        let numeric = number_str.replace(',', "").replace('_', "");
        let value: f64 = numeric
            .parse()
            .map_err(|_| format!("Failed to parse bitrate '{}': invalid number", input))?;

        let mut normalized_suffix = suffix.trim().to_string();
        for trailing in ["/s", "ps", "bps", "bits", "bit"] {
            if normalized_suffix.ends_with(trailing) {
                let new_len = normalized_suffix.len() - trailing.len();
                normalized_suffix.truncate(new_len);
            }
        }
        normalized_suffix = normalized_suffix.trim().to_string();

        let multiplier = match normalized_suffix.as_str() {
            "" | "b" => 1i64,
            "k" | "kb" | "kbit" => 1_000i64,
            "m" | "mb" | "mbit" => 1_000_000i64,
            "g" | "gb" | "gbit" => 1_000_000_000i64,
            other => {
                return Err(format!(
                    "Failed to parse bitrate '{}': unsupported suffix '{}'. Use plain numbers or k/m/g suffixes.",
                    input, other
                ));
            }
        };

        let bits_per_second = (value * multiplier as f64).round() as i64;
        if bits_per_second <= 0 {
            return Err(format!(
                "Failed to parse bitrate '{}': value must be positive",
                input
            ));
        }

        Ok(bits_per_second)
    }

    fn parse_device_selection(input: &str) -> Result<StreamingDeviceSelection, String> {
        let normalized = input.trim();
        if normalized.is_empty() {
            return Err("Streaming device value cannot be empty".to_string());
        }

        if normalized.eq_ignore_ascii_case("all") {
            return Ok(StreamingDeviceSelection::All);
        }

        streaming_devices::STREAMING_DEVICES
            .iter()
            .find(|device| device.model.eq_ignore_ascii_case(normalized))
            .map(StreamingDeviceSelection::Model)
            .ok_or_else(|| {
                let available = streaming_devices::STREAMING_DEVICES
                    .iter()
                    .map(|device| device.model)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "Provided streaming device model '{}' not found. Valid values: all, {}",
                    input, available
                )
            })
    }
}

pub enum StreamExtras {
    Some((SwrContext, AVAudioFifo)),
    None,
}

pub struct StreamProcessingContext {
    decode_context: AVCodecContext,
    encode_context: AVCodecContext,
    stream_index: i32,
    media_type: ffi::AVMediaType,
    frame_buffer: Option<AVAudioFifo>, // TODO: Support video stream buffers too?
    resample_context: Option<SwrContext>,
    pts: AtomicI64,
    last_written_dts: Option<i64>,
    skip_stream: bool,
    #[allow(dead_code)]
    hw_device_ctx: Option<*mut ffi::AVBufferRef>,
}

impl std::fmt::Debug for StreamProcessingContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StreamProcessingContext")
            .field("stream_index", &self.stream_index)
            .field("media_type", &self.media_type)
            .field("pts", &self.pts)
            .field("last_written_dts", &self.last_written_dts)
            .field("skip_stream", &self.skip_stream)
            .finish()
    }
}

fn init_audio_resampler(
    decode_context: &mut AVCodecContext,
    encode_context: &mut AVCodecContext,
) -> Result<SwrContext> {
    let mut resample_context = SwrContext::new(
        &encode_context.ch_layout,
        encode_context.sample_fmt,
        encode_context.sample_rate,
        &decode_context.ch_layout,
        decode_context.sample_fmt,
        decode_context.sample_rate,
    )
    .context("Could not allocate resample context")?;
    resample_context
        .init()
        .context("Could not open resample context")?;
    Ok(resample_context)
}

fn add_samples_to_fifo(
    fifo: &mut AVAudioFifo,
    samples_buffer: &AVSamples,
    frame_size: i32,
) -> Result<()> {
    fifo.realloc(fifo.size() + frame_size);
    unsafe { fifo.write(samples_buffer.audio_data.as_ptr(), frame_size) }
        .context("Could not write data to FIFO")?;
    Ok(())
}

fn init_output_audio_frame(
    nb_samples: i32,
    ch_layout: ffi::AVChannelLayout,
    sample_fmt: i32,
    sample_rate: i32,
) -> Result<AVFrame> {
    let mut frame = AVFrame::new();
    frame.set_nb_samples(nb_samples);
    frame.set_ch_layout(ch_layout);
    frame.set_format(sample_fmt);
    frame.set_sample_rate(sample_rate);

    frame
        .get_buffer(0)
        .context("Could not allocate output frame samples")?;

    Ok(frame)
}

fn encode_and_write_frame(
    encode_context: &mut AVCodecContext,
    output_format_context: &mut AVFormatContextOutput,
    stream_index: usize,
    frame: Option<AVFrame>,
) -> Result<()> {
    encode_context
        .send_frame(frame.as_ref())
        .context("Failed to send frame!")?;

    loop {
        let mut packet = match encode_context.receive_packet() {
            Ok(packet) => packet,
            Err(RsmpegError::EncoderDrainError) | Err(RsmpegError::EncoderFlushedError) => {
                break;
            }
            Err(e) => bail!(e),
        };

        packet.set_stream_index(stream_index as i32);
        packet.rescale_ts(
            encode_context.time_base,
            output_format_context
                .streams()
                .get(stream_index)
                .context("Failed to get stream")?
                .time_base,
        );

        output_format_context
            .write_frame(&mut packet)
            .context("Could not write frame")?;
    }

    Ok(())
}

fn apply_config_overrides(args: &mut Args, cfg: &config::Config, matches: &ArgMatches) {
    if args.streaming_devices.is_none() {
        if let Some(devices) = cfg.streaming_devices.as_ref() {
            let raw_values: Vec<String> = match devices {
                config::StreamingDevicesSetting::Single(value) => value
                    .split(',')
                    .map(str::trim)
                    .filter(|entry| !entry.is_empty())
                    .map(|s| s.to_string())
                    .collect(),
                config::StreamingDevicesSetting::List(values) => values
                    .iter()
                    .flat_map(|value| value.split(','))
                    .map(str::trim)
                    .filter(|entry| !entry.is_empty())
                    .map(|s| s.to_string())
                    .collect(),
            };

            let selections: std::result::Result<Vec<_>, _> = raw_values
                .iter()
                .map(|entry| Args::parse_device_selection(entry))
                .collect();
            match selections {
                Ok(list) if !list.is_empty() => args.streaming_devices = Some(list),
                Ok(_) => {}
                Err(err) => warn!("Failed to parse config streaming_devices: {}", err),
            }
        }
    }

    if args.max_video_bitrate.is_none() && !cli_value_provided(matches, "max_video_bitrate") {
        if let Some(bitrate) = cfg.max_video_bitrate.as_deref() {
            match Args::parse_bitrate(bitrate) {
                Ok(bps) => args.max_video_bitrate = Some(bps),
                Err(err) => warn!(
                    "Failed to parse config max_video_bitrate='{}': {}",
                    bitrate, err
                ),
            }
        }
    }

    if args.max_audio_bitrate.is_none() && !cli_value_provided(matches, "max_audio_bitrate") {
        if let Some(bitrate) = cfg.max_audio_bitrate.as_deref() {
            match Args::parse_bitrate(bitrate) {
                Ok(bps) => args.max_audio_bitrate = Some(bps),
                Err(err) => warn!(
                    "Failed to parse config max_audio_bitrate='{}': {}",
                    bitrate, err
                ),
            }
        }
    }

    if !cli_value_provided(matches, "video_quality") {
        if let Some(video_quality) = cfg.video_quality {
            args.video_quality = video_quality;
        }
    }

    if !cli_value_provided(matches, "audio_quality") {
        if let Some(audio_quality) = cfg.audio_quality {
            args.audio_quality = audio_quality;
        }
    }

    if !cli_value_provided(matches, "hw_accel") {
        if let Some(hw_accel) = cfg.hw_accel {
            args.hw_accel = hw_accel;
        }
    }

    if !cli_value_provided(matches, "unsupported_video_policy") {
        if let Some(policy) = cfg.unsupported_video_policy {
            args.unsupported_video_policy = policy;
        }
    }

    if !cli_value_provided(matches, "primary_video_stream_index") {
        if cfg.primary_video_stream_index.is_some() {
            args.primary_video_stream_index = cfg.primary_video_stream_index;
        }
    }

    if !cli_value_provided(matches, "primary_video_criteria") {
        if let Some(criteria) = cfg.primary_video_criteria {
            args.primary_video_criteria = criteria;
        }
    }

    if !cli_value_provided(matches, "servarr_output_extension") {
        if let Some(ext) = cfg.servarr_output_extension.as_ref() {
            args.servarr_output_extension = ext.clone();
        }
    }

    if !cli_value_provided(matches, "servarr_output_suffix") {
        if let Some(suffix) = cfg.servarr_output_suffix.as_ref() {
            args.servarr_output_suffix = suffix.clone();
        }
    }

    if args.delete_source.is_none() {
        if let Some(delete_source) = cfg.delete_source {
            args.delete_source = Some(delete_source);
        }
    }

    if cfg.plex.is_some() {
        debug!("Plex config overrides detected in configuration file.");
    }
}

fn process_video_stream(
    stream_processing_context: &mut StreamProcessingContext,
    input_stream: &AVStreamRef,
    output_format_context: &mut AVFormatContextOutput,
    packet: &mut AVPacket,
    mut progress: Option<&mut ProgressTracker>,
) -> Result<()> {
    packet.rescale_ts(
        input_stream.time_base,
        stream_processing_context.decode_context.time_base,
    );

    match stream_processing_context
        .decode_context
        .send_packet(Some(packet))
    {
        Ok(_) | Err(RsmpegError::DecoderFlushedError) => {}
        Err(e) => {
            bail!("Packet failed to send to decoder: {}", e); // TODO: reasonable to end here on error?
        }
    }

    let mut warned_drain = false;

    loop {
        let frame = match stream_processing_context.decode_context.receive_frame() {
            Ok(frame) => frame,
            Err(RsmpegError::DecoderDrainError) | Err(RsmpegError::DecoderFlushedError) => {
                if !std::mem::replace(&mut warned_drain, true) {
                    debug!(
                        "Video decoder drained/flushed for stream {}",
                        stream_processing_context.stream_index
                    );
                }
                break;
            }
            Err(e) => {
                error!("Decoder receive frame error: {}", e);
                break;
            }
        };

        if let Some(progress) = progress.as_deref_mut() {
            progress.report(
                frame.best_effort_timestamp,
                stream_processing_context.decode_context.time_base,
            );
        }

        let mut new_frame = AVFrame::new();
        new_frame.set_width(stream_processing_context.encode_context.width);
        new_frame.set_height(stream_processing_context.encode_context.height);
        new_frame.set_format(ffi::AV_PIX_FMT_YUV420P);
        new_frame.alloc_buffer().context("Error allocating ")?;

        let mut sws_context = SwsContext::get_context(
            stream_processing_context.decode_context.width,
            stream_processing_context.decode_context.height,
            stream_processing_context.decode_context.pix_fmt,
            stream_processing_context.encode_context.width,
            stream_processing_context.encode_context.height,
            stream_processing_context.encode_context.pix_fmt,
            ffi::SWS_FAST_BILINEAR | ffi::SWS_ACCURATE_RND,
            None,
            None,
            None,
        )
        .context("Failed to create a swscale context.")?;

        new_frame.set_pts(frame.best_effort_timestamp);

        sws_context
            .scale_frame(
                &frame,
                0,
                stream_processing_context.decode_context.height,
                &mut new_frame,
            )
            .context("Failed to scale frame.")?;

        encode_and_write_frame(
            &mut stream_processing_context.encode_context,
            output_format_context,
            packet.stream_index as usize,
            Some(new_frame),
        )?;
    }

    Ok(())
}

fn process_audio_stream(
    stream_processing_context: &mut StreamProcessingContext,
    input_stream: &AVStreamRef,
    output_format_context: &mut AVFormatContextOutput,
    packet: &mut AVPacket,
) -> Result<()> {
    // TODO: ensure audio streams have the same metadata
    // based on https://github.com/larksuite/rsmpeg/blob/master/tests/ffmpeg_examples/transcode_aac.rs
    packet.rescale_ts(
        input_stream.time_base,
        stream_processing_context.decode_context.time_base,
    );

    let Some(fifo) = stream_processing_context.frame_buffer.as_mut() else {
        panic!("Failed to get Audio FIFO buffer!");
    };

    match stream_processing_context
        .decode_context
        .send_packet(Some(packet))
    {
        Ok(_) | Err(RsmpegError::DecoderFlushedError) => {}
        Err(e) => {
            bail!("Packet failed to send to decoder: {}", e); // TODO: reasonable to end here on error?
        }
    }

    let mut warned_drain = false;

    loop {
        let frame = match stream_processing_context.decode_context.receive_frame() {
            Ok(frame) => frame,
            Err(RsmpegError::DecoderDrainError) | Err(RsmpegError::DecoderFlushedError) => {
                if !std::mem::replace(&mut warned_drain, true) {
                    debug!(
                        "Audio decoder drained/flushed for stream {}",
                        stream_processing_context.stream_index
                    );
                }
                break;
            }
            Err(e) => {
                error!("Decoder receive frame error: {}", e);
                break;
            }
        };

        let output_frame_size = stream_processing_context.encode_context.frame_size; // TODO: Why is this 0 in some cases and how to handle that?

        debug!("OUTPUT FRAME SIZE: {}", output_frame_size);

        let mut output_samples = AVSamples::new(
            stream_processing_context
                .encode_context
                .ch_layout
                .nb_channels,
            frame.nb_samples,
            stream_processing_context.encode_context.sample_fmt,
            0,
        )
        .context("Create samples buffer failed.")?;

        match &mut stream_processing_context.resample_context {
            Some(resampler) => unsafe {
                resampler
                    .convert(
                        output_samples.audio_data.as_mut_ptr(),
                        output_samples.nb_samples,
                        frame.extended_data as *const _,
                        frame.nb_samples,
                    )
                    .context("Could not convert input samples")?;
            },
            None => {}
        }

        add_samples_to_fifo(fifo, &output_samples, frame.nb_samples)?;

        debug!("FIFO SIZE: {}", fifo.size());
        debug!(
            "AUDIO STREAM INDEX: {}",
            stream_processing_context.stream_index
        );
        while fifo.size() >= output_frame_size {
            load_encode_and_write(
                fifo,
                output_format_context,
                &mut stream_processing_context.encode_context,
                stream_processing_context.stream_index,
                &mut stream_processing_context.pts,
            )?;
        }
    }

    Ok(())
}

fn process_subtitle_stream(
    stream_processing_context: &mut StreamProcessingContext,
    input_stream: &AVStreamRef,
    output_format_context: &mut AVFormatContextOutput,
    packet: &mut AVPacket,
) -> Result<()> {
    if stream_processing_context.skip_stream {
        trace!(
            "Subtitle stream {} already skipped; dropping packet.",
            stream_processing_context.stream_index
        );
        return Ok(());
    }

    packet.rescale_ts(
        input_stream.time_base,
        stream_processing_context.decode_context.time_base,
    );

    match stream_processing_context
        .decode_context
        .decode_subtitle(Some(packet))
    {
        Ok(sub) => {
            if let Some(subtitle) = sub {
                debug!(
                    "Subtitle stream {} raw packet pts={} dts={} duration={}",
                    stream_processing_context.stream_index, packet.pts, packet.dts, packet.duration
                );
                // TODO: Find the max size of subtitle data in a single packet
                const MAX_SUBTITLE_PACKET_SIZE: usize = 32 * 1024; // 32KB
                let mut subtitle_buffer = vec![0u8; MAX_SUBTITLE_PACKET_SIZE];
                stream_processing_context
                    .encode_context
                    .encode_subtitle(&subtitle, &mut subtitle_buffer)?;

                let encoded_size = subtitle_buffer
                    .iter()
                    .rposition(|&x| x != 0)
                    .map(|pos| pos + 1)
                    .unwrap_or(0);

                if encoded_size == 0 {
                    return Ok(());
                }

                // Create a new packet for the encoded subtitle
                let mut encoded_packet = AVPacket::new();
                unsafe {
                    ffi::av_new_packet(encoded_packet.as_mut_ptr(), encoded_size as i32);
                    std::ptr::copy_nonoverlapping(
                        subtitle_buffer.as_ptr(),
                        (*encoded_packet.as_mut_ptr()).data,
                        encoded_size,
                    );
                }

                let mut pts = if subtitle.pts != ffi::AV_NOPTS_VALUE {
                    subtitle.pts
                } else {
                    packet.pts
                };
                let mut dts = if packet.dts != ffi::AV_NOPTS_VALUE {
                    packet.dts
                } else {
                    pts
                };

                if pts == ffi::AV_NOPTS_VALUE {
                    pts = stream_processing_context
                        .last_written_dts
                        .map(|prev| prev + 1)
                        .unwrap_or(0);
                }

                if dts == ffi::AV_NOPTS_VALUE {
                    dts = pts;
                }

                encoded_packet.set_stream_index(stream_processing_context.stream_index);
                encoded_packet.set_pts(pts);
                encoded_packet.set_dts(dts);
                encoded_packet.set_duration(packet.duration);
                encoded_packet.set_flags(packet.flags);

                let output_time_base = output_format_context.streams()
                    [stream_processing_context.stream_index as usize]
                    .time_base;
                encoded_packet.rescale_ts(
                    stream_processing_context.decode_context.time_base,
                    output_time_base,
                );

                let packet_dts = encoded_packet.dts;
                if let Some(prev_dts) = stream_processing_context.last_written_dts {
                    if packet_dts <= prev_dts {
                        let adjusted = prev_dts + 1;
                        encoded_packet.set_dts(adjusted);
                        if encoded_packet.pts < adjusted {
                            encoded_packet.set_pts(adjusted);
                        }
                        debug!(
                            "Subtitle stream {} adjusted DTS from {} to {}",
                            stream_processing_context.stream_index, packet_dts, adjusted
                        );
                    }
                }

                stream_processing_context.last_written_dts = Some(encoded_packet.dts);
                debug!(
                    "Subtitle stream {} final pts={} dts={} duration={} (tb={}/{})",
                    stream_processing_context.stream_index,
                    encoded_packet.pts,
                    encoded_packet.dts,
                    encoded_packet.duration,
                    output_time_base.num,
                    output_time_base.den
                );

                match output_format_context.interleaved_write_frame(&mut encoded_packet) {
                    Ok(()) => {}
                    Err(rsmpeg::error::RsmpegError::AVError(code)) if code == -EINVAL => {
                        warn!(
                            "Subtitle stream {} produced invalid timestamps; skipping rest of stream.",
                            stream_processing_context.stream_index
                        );
                        stream_processing_context.skip_stream = true;
                    }
                    Err(e) => {
                        return Err(e).context("Could not write subtitle packet");
                    }
                }
            }
        }
        Err(rsmpeg::error::RsmpegError::DecoderDrainError) => {
            error!("Error: The decoder has been fully drained, no more subtitles to decode. Continuing...");
        }
        Err(rsmpeg::error::RsmpegError::DecoderFlushedError) => {
            error!(
                "Error: The decoder has been flushed, no more subtitles to decode. Continuing..."
            );
        }
        Err(e) => {
            error!("Error decoding subtitle: {}", e);
        }
    }

    Ok(())
}

fn is_image_based_subtitle(codec_id: ffi::AVCodecID) -> bool {
    matches!(
        codec_id,
        ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE
            | ffi::AV_CODEC_ID_DVD_SUBTITLE
            | ffi::AV_CODEC_ID_DVB_SUBTITLE
            | ffi::AV_CODEC_ID_XSUB
    )
}

fn load_encode_and_write(
    fifo: &mut AVAudioFifo,
    output_format_context: &mut AVFormatContextOutput,
    encode_context: &mut AVCodecContext,
    stream_index: i32,
    pts: &mut AtomicI64,
) -> Result<()> {
    let frame_size = fifo.size().min(encode_context.frame_size); // TODO: should be encode_context.frame_size but it reads 0

    let mut frame = init_output_audio_frame(
        frame_size,
        encode_context.ch_layout().clone().into_inner(),
        encode_context.sample_fmt,
        encode_context.sample_rate,
    )
    .context("Failed to initialize audio frame.")?;

    if unsafe {
        let read_frame_size = fifo.read(frame.data_mut().as_mut_ptr(), frame_size)?;
        debug!("Read audio frame size (bytes): {}", read_frame_size);
        read_frame_size
    } < frame_size
    {
        bail!("Could not read data from FIFO");
    }

    frame.set_pts(pts.fetch_add(frame.nb_samples as i64, Ordering::Relaxed));

    encode_and_write_frame(
        encode_context,
        output_format_context,
        stream_index as usize,
        Some(frame),
    )
    .context("Error encoding audio frame!")?;

    Ok(())
}

fn set_video_codec_par(
    decode_context: &mut AVCodecContext,
    encode_context: &mut AVCodecContext,
    _output_stream: &mut AVStreamMut,
    h264_profile: H264Profile, // TODO: handle cases somewhere when target video codec is NOT h264
    h264_level: H264Level,
    quality_limits: &QualityLimits,
    device_max_resolution: Resolution,
    source_bit_rate_hint: i64,
    encoder_name: &str,
    is_constant_quality_mode: bool,
) {
    encode_context.set_sample_rate(decode_context.sample_rate);
    let device_cap = resolution_to_dimensions(device_max_resolution);
    let (target_width, target_height) = clamp_dimensions(
        decode_context.width,
        decode_context.height,
        device_cap,
        quality_limits.max_video_dimensions,
    );

    if target_width != decode_context.width || target_height != decode_context.height {
        debug!(
            "Scaling video from {}x{} to {}x{}",
            decode_context.width, decode_context.height, target_width, target_height
        );
    }

    encode_context.set_width(target_width);
    encode_context.set_height(target_height);
    encode_context.set_time_base(decode_context.time_base);
    encode_context.set_pix_fmt(ffi::AV_PIX_FMT_YUV420P); // TODO: downgrade more intelligently?
    encode_context.set_max_b_frames(decode_context.max_b_frames);

    // START FIX: CONDITIONAL BITRATE SETTING
    let mut target_bit_rate: Option<i64> = None;

    if !is_constant_quality_mode {
        // Calculate the target bitrate only if we are in a fixed-bitrate mode
        let source_bit_rate = if decode_context.bit_rate > 0 {
            decode_context.bit_rate
        } else if source_bit_rate_hint > 0 {
            source_bit_rate_hint
        } else {
            default_video_bitrate(encode_context.width, encode_context.height)
        };

        target_bit_rate = derive_target_bitrate(source_bit_rate, quality_limits.max_video_bitrate);

        if let Some(bit_rate) = target_bit_rate {
            debug!(
                "Video bitrate target set to {} bps (Fixed Bitrate Mode)",
                bit_rate
            );
            encode_context.set_bit_rate(bit_rate);
            unsafe {
                let vbv = bit_rate.saturating_mul(2).clamp(1, i32::MAX as i64) as i32;
                (*encode_context.as_mut_ptr()).rc_max_rate = bit_rate;
                (*encode_context.as_mut_ptr()).rc_min_rate = bit_rate;
                (*encode_context.as_mut_ptr()).rc_buffer_size = vbv;
                (*encode_context.as_mut_ptr()).rc_initial_buffer_occupancy = vbv;
                (*encode_context.as_mut_ptr()).bit_rate_tolerance =
                    (bit_rate / 8).max(1).clamp(1, i32::MAX as i64) as i32;
            }
        } else {
            debug!("Video bitrate target not set; using encoder default");
        }
    } else {
        // In Constant Quality Mode, explicitly tell the FFmpeg context to ignore bitrate constraint
        // The hardware tuning function (below) will enforce the quality/QP.
        debug!("Video encoding set to Constant Quality (CQ/CQP) mode. Ignoring bitrate limits.");
        encode_context.set_bit_rate(0);
    }
    // END FIX: CONDITIONAL BITRATE SETTING

    apply_hw_encoder_quality(
        encode_context.as_mut_ptr(),
        encoder_name,
        target_bit_rate,
        is_constant_quality_mode, // <-- PASS NEW PARAMETER
    );
    log_encoder_state("video setup", encode_context, encoder_name);
    encode_context.set_gop_size(decode_context.gop_size);
    encode_context.set_sample_aspect_ratio(decode_context.sample_aspect_ratio);
    // TODO: find a safe way to do this
    unsafe {
        (*encode_context.as_mut_ptr()).profile = h264_profile as i32;
        (*encode_context.as_mut_ptr()).level = h264_level as i32;
    }
    if should_apply_profile_option(encoder_name) {
        apply_h264_profile_option(encode_context.as_mut_ptr(), encoder_name, h264_profile);
    }

    let level_option_value = level_option_value_for_encoder(encoder_name, h264_level);

    unsafe {
        set_codec_option_str(encode_context.as_mut_ptr(), "level", &level_option_value);
    }
    // Codec parameters are extracted after the encoder is opened.
}

fn set_audio_codec_par(
    decode_context: &mut AVCodecContext,
    encode_context: &mut AVCodecContext,
    _output_stream: &mut AVStreamMut,
    quality_limits: &QualityLimits,
    source_bit_rate_hint: i64,
) {
    // TODO: Read input to determine output audio codec params
    let encoder = AVCodec::find_encoder(ffi::AV_CODEC_ID_AAC).expect("Could not find AAC encoder");
    let decode_channels = decode_context.ch_layout.nb_channels;
    encode_context.set_ch_layout(AVChannelLayout::from_nb_channels(decode_channels).into_inner());
    // The input file's sample rate is used to avoid a sample rate conversion.
    encode_context.set_sample_rate(decode_context.sample_rate);
    encode_context.set_sample_fmt(encoder.sample_fmts().unwrap()[0]); // TODO: Are we actually getting the sample rate we want?
    let source_bit_rate = if decode_context.bit_rate > 0 {
        decode_context.bit_rate
    } else if source_bit_rate_hint > 0 {
        source_bit_rate_hint
    } else {
        192_000
    };
    if let Some(bit_rate) = derive_target_bitrate(source_bit_rate, quality_limits.max_audio_bitrate)
    {
        debug!("Audio bitrate target set to {} bps", bit_rate);
        encode_context.set_bit_rate(bit_rate);
        unsafe {
            (*encode_context.as_mut_ptr()).rc_max_rate = bit_rate;
            (*encode_context.as_mut_ptr()).rc_min_rate = bit_rate;
            (*encode_context.as_mut_ptr()).rc_buffer_size =
                bit_rate.clamp(1, i32::MAX as i64) as i32;
            (*encode_context.as_mut_ptr()).rc_initial_buffer_occupancy =
                bit_rate.clamp(1, i32::MAX as i64) as i32;
            (*encode_context.as_mut_ptr()).bit_rate_tolerance =
                (bit_rate / 4).clamp(1, i32::MAX as i64) as i32;
        }
    } else {
        debug!("Audio bitrate target not set; using encoder default");
    }

    // Codec parameters are extracted after the encoder is opened.
    _output_stream.set_time_base(ra(1, decode_context.sample_rate)); // use high-precision time base
    log_encoder_state("audio setup", encode_context, "aac");
}

fn set_subtitle_codec_par(
    decode_context: &mut AVCodecContext,
    encode_context: &mut AVCodecContext,
    _output_stream: &mut AVStreamMut,
) {
    // Set subtitle encoder parameters based on the input subtitle stream
    encode_context.set_time_base(decode_context.time_base);

    if decode_context.subtitle_header_size > 0 {
        let mut new_subtitle_header = vec![0u8; decode_context.subtitle_header_size as usize];
        new_subtitle_header.copy_from_slice(unsafe {
            std::slice::from_raw_parts(
                decode_context.subtitle_header,
                decode_context.subtitle_header_size as usize,
            )
        });

        // TODO: find safe way to do this
        unsafe {
            (*encode_context.as_mut_ptr()).subtitle_header =
                ffi::av_mallocz(new_subtitle_header.len()) as *mut _;
            (*encode_context.as_mut_ptr()).subtitle_header_size = new_subtitle_header.len() as i32;
            std::ptr::copy_nonoverlapping(
                new_subtitle_header.as_ptr(),
                (*encode_context.as_mut_ptr()).subtitle_header,
                new_subtitle_header.len(),
            );
        }
    }

    // Codec parameters are extracted after the encoder is opened.
}

struct DirectPlayAssessment {
    compatible: bool,
    reasons: Vec<String>,
}

fn assess_direct_play_compatibility(
    input_file: &CStr,
    target_video_codec: ffi::AVCodecID,
    target_audio_codec: ffi::AVCodecID,
    min_h264_profile: H264Profile,
    min_h264_level: H264Level,
    max_fps: u32,
    device_cap: (u32, u32),
    quality_limits: &QualityLimits,
    primary_video_stream_index: Option<usize>,
    primary_criteria: PrimaryVideoCriteria,
) -> Result<DirectPlayAssessment> {
    let ictx = AVFormatContextInput::open(input_file)?;
    let primary_idx =
        select_primary_video_stream_index(&ictx, primary_video_stream_index, primary_criteria)?;

    let streams: Vec<_> = ictx.streams().into_iter().collect();
    let video_stream = streams.get(primary_idx).ok_or_else(|| {
        anyhow!(
            "Primary video stream index {} out of range while checking direct-play compatibility",
            primary_idx
        )
    })?;

    let mut reasons = Vec::new();
    let video_par = video_stream.codecpar();

    for stream in &streams {
        let disposition_flags = unsafe { (*stream.as_ptr()).disposition as i32 };
        if (disposition_flags & ffi::AV_DISPOSITION_ATTACHED_PIC as i32) != 0 {
            reasons.push("input contains an attached picture stream".to_string());
            break;
        }
        if stream.codecpar().codec_type == ffi::AVMEDIA_TYPE_ATTACHMENT {
            reasons.push("input contains an attachment stream".to_string());
            break;
        }
    }

    if video_par.codec_id != target_video_codec {
        reasons.push(format!(
            "video codec {} is not compatible with required {}",
            describe_codec(video_par.codec_id),
            describe_codec(target_video_codec)
        ));
    }

    if video_par.width <= 0 || video_par.height <= 0 {
        reasons.push("video resolution unknown".to_string());
    } else if (video_par.width as u32) > device_cap.0 || (video_par.height as u32) > device_cap.1 {
        reasons.push(format!(
            "video resolution {}x{} exceeds device limit {}x{}",
            video_par.width, video_par.height, device_cap.0, device_cap.1
        ));
    }

    if let Some((quality_w, quality_h)) = quality_limits.max_video_dimensions {
        if video_par.width > 0
            && video_par.height > 0
            && ((video_par.width as u32) > quality_w || (video_par.height as u32) > quality_h)
        {
            reasons.push(format!(
                "video resolution {}x{} exceeds requested quality limit {}x{}",
                video_par.width, video_par.height, quality_w, quality_h
            ));
        }
    }

    if let Some(max_video_bitrate) = quality_limits.max_video_bitrate {
        let mut video_bit_rate = video_par.bit_rate;
        if video_bit_rate <= 0 {
            video_bit_rate = unsafe { (*(*video_stream.as_ptr()).codecpar).bit_rate };
        }
        if video_bit_rate <= 0 {
            reasons.push(
                "video bitrate unknown; cannot confirm compliance with requested quality limit"
                    .into(),
            );
        } else if video_bit_rate > max_video_bitrate {
            reasons.push(format!(
                "video bitrate {} bps exceeds requested limit {} bps",
                video_bit_rate, max_video_bitrate
            ));
        }
    }

    if max_fps > 0 {
        match estimate_stream_fps(video_stream) {
            Some(fps) => {
                if fps > max_fps as f64 + 0.5 {
                    reasons.push(format!(
                        "video frame rate {:.2} fps exceeds device limit {} fps",
                        fps, max_fps
                    ));
                }
            }
            None => reasons.push("video frame rate unknown; cannot confirm compatibility".into()),
        }
    }

    if target_video_codec == ffi::AV_CODEC_ID_H264 {
        check_h264_profile_level_constraints(
            video_par.codec_id,
            video_par.profile,
            video_par.level,
            min_h264_profile,
            min_h264_level,
            &mut reasons,
        );
    }

    let mut audio_ok = false;
    let mut audio_quality_reason: Option<String> = None;
    for stream in &streams {
        let codecpar = stream.codecpar();
        if codecpar.codec_type != ffi::AVMEDIA_TYPE_AUDIO {
            continue;
        }
        if codecpar.codec_id != target_audio_codec {
            continue;
        }

        if let Some(max_audio_bitrate) = quality_limits.max_audio_bitrate {
            let mut audio_bit_rate = codecpar.bit_rate;
            if audio_bit_rate <= 0 {
                audio_bit_rate = unsafe { (*(*stream.as_ptr()).codecpar).bit_rate };
            }

            if audio_bit_rate <= 0 {
                if audio_quality_reason.is_none() {
                    audio_quality_reason = Some(
                        "audio bitrate unknown; cannot confirm compliance with requested quality limit"
                            .into(),
                    );
                }
                continue;
            }

            if audio_bit_rate > max_audio_bitrate {
                if audio_quality_reason.is_none() {
                    audio_quality_reason = Some(format!(
                        "audio bitrate {} bps exceeds requested limit {} bps",
                        audio_bit_rate, max_audio_bitrate
                    ));
                }
                continue;
            }
        }

        audio_ok = true;
        break;
    }

    if !audio_ok {
        if let Some(reason) = audio_quality_reason {
            reasons.push(reason);
        } else {
            reasons.push(format!(
                "no audio stream with compatible codec {} found",
                describe_codec(target_audio_codec)
            ));
        }
    }

    Ok(DirectPlayAssessment {
        compatible: reasons.is_empty(),
        reasons,
    })
}

fn estimate_stream_fps(stream: &AVStreamRef) -> Option<f64> {
    if let Some(rational) = stream.guess_framerate() {
        rational_to_f64(rational)
    } else {
        let avg = unsafe { (*stream.as_ptr()).avg_frame_rate };
        rational_to_f64(avg)
    }
}

fn rational_to_f64(rational: ffi::AVRational) -> Option<f64> {
    if rational.num <= 0 || rational.den <= 0 {
        None
    } else {
        Some(rational.num as f64 / rational.den as f64)
    }
}

#[cfg(test)]
mod direct_play_tests {
    use super::*;

    #[test]
    fn rational_to_f64_handles_valid_fraction() {
        let val = rational_to_f64(ffi::AVRational {
            num: 60000,
            den: 1001,
        })
        .expect("should convert");
        // 59.94 fps typical NTSC
        assert!((val - 59.94).abs() < 0.01);
    }

    #[test]
    fn rational_to_f64_rejects_non_positive() {
        assert!(rational_to_f64(ffi::AVRational { num: 0, den: 1 }).is_none());
        assert!(rational_to_f64(ffi::AVRational { num: 1, den: 0 }).is_none());
    }
}

/// Takes input video files and outputs direct-play-compatible video files
fn convert_video_file(
    input_file: &CStr,
    output_file: &CStr,
    target_video_codec: ffi::AVCodecID,
    target_audio_codec: ffi::AVCodecID,
    min_h264_profile: H264Profile,
    min_h264_level: H264Level,
    _min_fps: u32,
    device_max_resolution: Resolution,
    quality_limits: &QualityLimits,
    uv_policy: UnsupportedVideoPolicy,
    primary_video_stream_index: Option<usize>,
    primary_criteria: PrimaryVideoCriteria,
    requested_video_quality: VideoQuality,
    requested_audio_quality: AudioQuality,
    hw_accel: HwAccel,
) -> Result<(), anyhow::Error> {
    let mut input_format_context = AVFormatContextInput::open(input_file)?;
    if log::log_enabled!(Level::Debug) {
        input_format_context.dump(0, input_file)?;
    }

    let mut output_format_context = AVFormatContextOutput::create(output_file)?;

    let output_path_str = output_file
        .to_str()
        .map_err(|_| anyhow!("Output path is not valid UTF-8"))?;
    let output_path = Path::new(output_path_str);
    let output_extension = output_path
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext.to_ascii_lowercase());
    let target_is_mp4 = matches!(output_extension.as_deref(), Some("mp4") | Some("m4v"));

    let mut stream_contexts: Vec<StreamProcessingContext> = Vec::new();
    let mut container_duration_us = unsafe { (*input_format_context.as_mut_ptr()).duration };
    if container_duration_us <= 0 {
        container_duration_us = 1;
    }
    let mut progress_tracker = Some(ProgressTracker::new(container_duration_us));

    info!(
        "Target codecs resolved: video={}, audio={}",
        describe_codec(target_video_codec),
        describe_codec(target_audio_codec)
    );

    // START FIX: Determine Constant Quality Mode
    let is_constant_quality_mode = requested_video_quality == VideoQuality::MatchSource;
    // END FIX: Determine Constant Quality Mode

    let mut logged_video_encoder = false;
    let mut logged_audio_encoder = false;
    let mut desired_h264_profile: Option<H264Profile> = None;
    let mut desired_h264_level: Option<H264Level> = None;
    let mut last_video_encoder_name: Option<String> = None;
    let mut hardware_encoder_used = false;

    let primary_index = select_primary_video_stream_index(
        &input_format_context,
        primary_video_stream_index,
        primary_criteria,
    )?;
    let mut _video_streams_seen = 0usize;
    let mut video_streams_added = 0usize;

    for stream in input_format_context.streams() {
        // TODO: ID streams either unsupported in output container type or without a supported decoder and skip them, producing a warning for each skipped.
        let input_codec_type = stream.codecpar().codec_type;
        // TODO: implement support for attachments
        if input_codec_type == ffi::AVMEDIA_TYPE_ATTACHMENT {
            warn!(
                "Skipping attachment stream {} ({}).",
                stream.index,
                unsafe {
                    CStr::from_ptr(ffi::avcodec_get_name(stream.codecpar().codec_id))
                        .to_string_lossy()
                }
            );
            continue;
        }

        if input_codec_type == ffi::AVMEDIA_TYPE_DATA {
            warn!("Skipping data stream {} ({}).", stream.index, unsafe {
                CStr::from_ptr(ffi::avcodec_get_name(stream.codecpar().codec_id)).to_string_lossy()
            });
            continue;
        }

        // Skip attached picture (cover art) streams; treat them like metadata.
        let disposition_flags = unsafe { (*stream.as_ptr()).disposition as i32 };
        if (disposition_flags & ffi::AV_DISPOSITION_ATTACHED_PIC as i32) != 0 {
            info!(
                "Skipping attached-picture stream {} ({}).",
                stream.index,
                unsafe {
                    CStr::from_ptr(ffi::avcodec_get_name(stream.codecpar().codec_id))
                        .to_string_lossy()
                }
            );
            continue;
        }

        let input_stream_codecpar = stream.codecpar();
        let input_codec_id = input_stream_codecpar.codec_id;
        let codec_name = unsafe {
            CStr::from_ptr(ffi::avcodec_get_name(input_codec_id))
                .to_string_lossy()
                .into_owned()
        };
        let decoder = match AVCodec::find_decoder(input_codec_id) {
            Some(dec) => dec,
            None if input_codec_type == ffi::AVMEDIA_TYPE_SUBTITLE => {
                warn!(
                    "Skipping subtitle stream {} (codec {}): decoder not available.",
                    stream.index, codec_name
                );
                continue;
            }
            None => {
                bail!(
                    "Decoder not found for stream {} (codec {}).",
                    stream.index,
                    codec_name
                );
            }
        };
        let mut decode_context = AVCodecContext::new(&decoder);
        decode_context.apply_codecpar(&input_stream_codecpar)?;
        decode_context.set_time_base(stream.time_base); // TODO: needed?
        if let Some(framerate) = stream.guess_framerate() {
            decode_context.set_framerate(framerate);
        }
        if let Err(err) = decode_context.open(None) {
            if input_codec_type == ffi::AVMEDIA_TYPE_SUBTITLE {
                warn!(
                    "Skipping subtitle stream {} (codec {}): failed to open decoder ({}).",
                    stream.index, codec_name, err
                );
                continue;
            } else {
                return Err(anyhow!(
                    "Error opening decoder for stream {} (codec {}): {}",
                    stream.index,
                    codec_name,
                    err
                ));
            }
        }

        let mut encode_context: AVCodecContext;
        let media_type: ffi::AVMediaType;
        let mut frame_buffer: Option<AVAudioFifo> = None;
        let mut resample_context: Option<SwrContext> = None;
        let mut hw_device_ctx_ptr: Option<*mut ffi::AVBufferRef> = None;
        let mut encoder_name_for_video: Option<String> = None;
        let mut target_h264_profile: Option<H264Profile> = None;
        let mut target_h264_level: Option<H264Level> = None;

        let is_video_stream = decode_context.codec_type == ffi::AVMEDIA_TYPE_VIDEO;
        if is_video_stream && stream.index as usize != primary_index {
            match uv_policy {
                UnsupportedVideoPolicy::Ignore => {
                    warn!(
                        "Ignoring extra video stream (index {}) due to policy 'ignore'",
                        stream.index
                    );
                    continue;
                }
                UnsupportedVideoPolicy::Fail => {
                    bail!(
                        "Encountered extra video stream (index {}). Rerun with --unsupported-video-policy=convert or ignore.",
                        stream.index
                    );
                }
                UnsupportedVideoPolicy::Convert => { /* continue */ }
            }
        }

        if decode_context.codec_type == ffi::AVMEDIA_TYPE_SUBTITLE
            && target_is_mp4
            && is_image_based_subtitle(decode_context.codec_id)
        {
            warn!(
                "Skipping subtitle stream {} (codec {}) for MP4 output; image-based subtitles are not supported.",
                stream.index,
                unsafe {
                    CStr::from_ptr(ffi::avcodec_get_name(decode_context.codec_id))
                        .to_string_lossy()
                }
            );
            continue;
        }

        let mut output_stream = output_format_context.new_stream();

        match decode_context.codec_type {
            ffi::AVMEDIA_TYPE_VIDEO => {
                output_stream.set_metadata(stream.metadata().as_deref().cloned());
                // Prefer HW encoder when available and requested
                let (maybe_hw_encoder, maybe_hw_dev) =
                    find_hw_encoder(target_video_codec, hw_accel);
                let (encoder, using_hw_encoder) = match maybe_hw_encoder {
                    Some(enc) => (enc, true),
                    None => (
                        AVCodec::find_encoder(target_video_codec)
                            .expect("Could not find H264 encoder"),
                        false,
                    ),
                };
                if using_hw_encoder {
                    hardware_encoder_used = true;
                }
                encode_context = AVCodecContext::new(&encoder);
                // Attach hardware device if present (must be set before open())
                if let Some(buf) = maybe_hw_dev {
                    unsafe {
                        (*encode_context.as_mut_ptr()).hw_device_ctx = ffi::av_buffer_ref(buf);
                    }
                    hw_device_ctx_ptr = Some(maybe_hw_dev.unwrap());
                }
                media_type = ffi::AVMEDIA_TYPE_VIDEO;

                let encoder_name_owned = encoder.name().to_string_lossy().into_owned();
                encoder_name_for_video = Some(encoder_name_owned.clone());
                target_h264_profile = Some(min_h264_profile);
                target_h264_level = Some(min_h264_level);
                desired_h264_profile = Some(min_h264_profile);
                desired_h264_level = Some(min_h264_level);
                last_video_encoder_name = Some(encoder_name_owned.clone());
                let encoder_name_lower = encoder_name_owned.to_ascii_lowercase();
                if encoder_name_lower.contains("nvenc")
                    || encoder_name_lower.contains("qsv")
                    || encoder_name_lower.contains("amf")
                    || encoder_name_lower.contains("vaapi")
                    || encoder_name_lower.contains("videotoolbox")
                    || encoder_name_lower.contains("_mf")
                {
                    hardware_encoder_used = true;
                }

                if !logged_video_encoder {
                    let encoder_name = &encoder_name_owned;
                    let is_hw = using_hw_encoder
                        || encoder_name.contains("nvenc")
                        || encoder_name.contains("qsv")
                        || encoder_name.contains("amf")
                        || encoder_name.contains("vaapi")
                        || encoder_name.contains("videotoolbox")
                        || encoder_name.contains("_mf");
                    let summary = if is_hw {
                        if maybe_hw_dev.is_some() {
                            format!("hardware (preference {:?}, {})", hw_accel, encoder_name)
                        } else {
                            format!("hardware ({})", encoder_name)
                        }
                    } else if matches!(hw_accel, HwAccel::None) {
                        format!("software ({}, hardware disabled)", encoder_name)
                    } else if matches!(hw_accel, HwAccel::Auto) {
                        format!("software ({}; auto fallback)", encoder_name)
                    } else {
                        format!("software ({}; {:?} unavailable)", encoder_name, hw_accel)
                    };
                    info!("Video encoder selected: {}", summary);
                    logged_video_encoder = true;
                }

                set_video_codec_par(
                    &mut decode_context,
                    &mut encode_context,
                    &mut output_stream,
                    min_h264_profile,
                    min_h264_level,
                    quality_limits,
                    device_max_resolution,
                    input_stream_codecpar.bit_rate,
                    &encoder_name_owned,
                    is_constant_quality_mode, // <-- PASS NEW PARAMETER
                );
                let src_par = stream.codecpar();
                info!(
                    "Video stream {}: {}x{} {} -> {}x{} {}{}",
                    output_stream.index,
                    src_par.width,
                    src_par.height,
                    unsafe {
                        CStr::from_ptr(ffi::avcodec_get_name(src_par.codec_id))
                            .to_str()
                            .unwrap_or("unknown")
                    },
                    encode_context.width,
                    encode_context.height,
                    describe_codec(target_video_codec),
                    if requested_video_quality == VideoQuality::MatchSource {
                        format!(
                            " (~{} approx)",
                            nearest_video_preset(
                                encode_context.width,
                                encode_context.height,
                                encode_context.bit_rate
                            )
                        )
                    } else {
                        String::new()
                    }
                );
                info!(
                    "Prepared video stream {} -> {}x{} (target {} bps)",
                    output_stream.index,
                    encode_context.width,
                    encode_context.height,
                    encode_context.bit_rate
                );
                _video_streams_seen += 1;
                video_streams_added += 1;
            }
            ffi::AVMEDIA_TYPE_AUDIO => {
                output_stream.set_metadata(stream.metadata().as_deref().cloned());
                let encoder =
                    AVCodec::find_encoder(target_audio_codec).expect("Could not find AAC encoder");

                encode_context = AVCodecContext::new(&encoder);
                media_type = ffi::AVMEDIA_TYPE_AUDIO;

                if !logged_audio_encoder {
                    let encoder_name = encoder.name().to_string_lossy().into_owned();
                    info!("Audio encoder selected: software ({})", encoder_name);
                    logged_audio_encoder = true;
                }

                set_audio_codec_par(
                    &mut decode_context,
                    &mut encode_context,
                    &mut output_stream,
                    quality_limits,
                    input_stream_codecpar.bit_rate,
                );
                let src_audio = stream.codecpar();
                let src_audio_channels = src_audio.ch_layout().nb_channels;
                info!(
                    "Audio stream {}: {} ch @ {} Hz {} -> {} ch @ {} Hz {}{}",
                    output_stream.index,
                    src_audio_channels,
                    src_audio.sample_rate,
                    unsafe {
                        CStr::from_ptr(ffi::avcodec_get_name(src_audio.codec_id))
                            .to_str()
                            .unwrap_or("unknown")
                    },
                    encode_context.ch_layout.nb_channels,
                    encode_context.sample_rate,
                    describe_codec(target_audio_codec),
                    if requested_audio_quality == AudioQuality::MatchSource {
                        format!(
                            " (~{} approx)",
                            nearest_audio_preset(encode_context.bit_rate)
                        )
                    } else {
                        String::new()
                    }
                );
                info!(
                    "Audio stream {} target bitrate: {} bps",
                    output_stream.index, encode_context.bit_rate
                );
                // Initialize the resampler to be able to convert audio sample formats.
                resample_context =
                    init_audio_resampler(&mut decode_context, &mut encode_context).ok();

                // Initialize the FIFO buffer to store audio samples to be encoded.
                frame_buffer = Some(AVAudioFifo::new(
                    encode_context.sample_fmt,
                    encode_context.ch_layout.nb_channels,
                    1,
                ));
            }
            ffi::AVMEDIA_TYPE_SUBTITLE => {
                // TODO: handle cases with no metadata?
                // TODO: always copy metadata or only in some cases?
                output_stream.set_metadata(stream.metadata().as_deref().cloned());

                let encoder = AVCodec::find_encoder(ffi::AV_CODEC_ID_MOV_TEXT)
                    .expect("Could not find MOV_TEXT encoder");
                encode_context = AVCodecContext::new(&encoder);
                media_type = ffi::AVMEDIA_TYPE_SUBTITLE;
                set_subtitle_codec_par(
                    &mut decode_context,
                    &mut encode_context,
                    &mut output_stream,
                );
                info!(
                    "Subtitle stream {}: {} -> {}",
                    output_stream.index,
                    unsafe {
                        CStr::from_ptr(ffi::avcodec_get_name(stream.codecpar().codec_id))
                            .to_str()
                            .unwrap_or("unknown")
                    },
                    describe_codec(ffi::AV_CODEC_ID_MOV_TEXT)
                );
            }
            // TODO: Handle metadata streams
            unsupported_type => {
                debug!(
                    "Encountered unsupported stream type ({}). Not setting up Codec.",
                    unsupported_type
                );
                continue;
            }
        }

        let media_label = match media_type {
            ffi::AVMEDIA_TYPE_VIDEO => "video",
            ffi::AVMEDIA_TYPE_AUDIO => "audio",
            ffi::AVMEDIA_TYPE_SUBTITLE => "subtitle",
            _ => "stream",
        };

        encode_context
            .open(None)
            .with_context(|| format!("Error opening {} encoder", media_label))?;

        if media_type == ffi::AVMEDIA_TYPE_VIDEO {
            if let (Some(profile), Some(level), Some(encoder_name)) = (
                target_h264_profile,
                target_h264_level,
                encoder_name_for_video.as_deref(),
            ) {
                enforce_h264_constraints(&mut encode_context, profile, level, encoder_name);
            }
        }

        output_stream.set_codecpar(encode_context.extract_codecpar());

        let stream_process_context = StreamProcessingContext {
            decode_context,
            encode_context,
            stream_index: output_stream.index,
            media_type,
            frame_buffer,
            resample_context,
            pts: AtomicI64::new(0),
            last_written_dts: None,
            skip_stream: false,
            hw_device_ctx: hw_device_ctx_ptr,
        };

        stream_contexts.push(stream_process_context);
    }

    // Write the header of the output file container.
    if let Err(e) = output_format_context.write_header(&mut None) {
        if video_streams_added > 1 {
            bail!(
                "Failed to write container header ({}). The output container may not support multiple video streams. Try --unsupported-video-policy=ignore to drop extra video streams.",
                e
            );
        } else {
            return Err(anyhow!(e)).context("Error writing output file header");
        }
    }

    // Demux streams
    loop {
        let mut packet = match input_format_context.read_packet()? {
            Some(x) => x,
            None => break,
        };

        let Some(stream_processing_context) = stream_contexts
            .iter_mut()
            .find(|context| context.stream_index == packet.stream_index)
        else {
            debug!(
                "Skipping packet for stream {} with no processing context (likely attachment).",
                packet.stream_index
            );
            continue;
        };

        let input_stream: &rsmpeg::avformat::AVStreamRef<'_> =
            &input_format_context.streams()[packet.stream_index as usize];
        match stream_processing_context.media_type {
            ffi::AVMEDIA_TYPE_VIDEO => {
                process_video_stream(
                    stream_processing_context,
                    input_stream,
                    &mut output_format_context,
                    &mut packet,
                    progress_tracker.as_mut(),
                )?;
            }
            ffi::AVMEDIA_TYPE_AUDIO => {
                process_audio_stream(
                    stream_processing_context,
                    input_stream,
                    &mut output_format_context,
                    &mut packet,
                )?;
            }
            ffi::AVMEDIA_TYPE_SUBTITLE => {
                process_subtitle_stream(
                    stream_processing_context,
                    input_stream,
                    &mut output_format_context,
                    &mut packet,
                )?;
            }

            unsupported_type => {
                debug!(
                    "Encountered unsupported stream type ({}). Not setting up Codec.",
                    unsupported_type
                );
            }
        }
    }

    // After processing all packets, flush each encoder
    for context in &mut stream_contexts {
        match context.media_type {
            ffi::AVMEDIA_TYPE_VIDEO => {
                encode_and_write_frame(
                    &mut context.encode_context,
                    &mut output_format_context,
                    context.stream_index as usize,
                    None,
                )
                .context("Failed to flush video encoder.")?;
                if let Some(mut dev) = context.hw_device_ctx {
                    unsafe {
                        ffi::av_buffer_unref(&mut dev);
                    }
                }
            }
            ffi::AVMEDIA_TYPE_AUDIO => {
                encode_and_write_frame(
                    &mut context.encode_context,
                    &mut output_format_context,
                    context.stream_index as usize,
                    None,
                )
                .context("Failed to flush audio encoder.")?;
            }
            ffi::AVMEDIA_TYPE_SUBTITLE => {}
            unsupported_type => {
                debug!(
                    "Encountered unsupported stream type ({}). Not flushing.",
                    unsupported_type
                );
            }
        }
    }

    if let Some(progress) = progress_tracker.as_mut() {
        progress.finish();
    }

    for context in &stream_contexts {
        match context.media_type {
            ffi::AVMEDIA_TYPE_VIDEO => {
                let preset = if requested_video_quality == VideoQuality::MatchSource {
                    format!(
                        " (~{} approx)",
                        nearest_video_preset(
                            context.encode_context.width,
                            context.encode_context.height,
                            context.encode_context.bit_rate,
                        )
                    )
                } else {
                    String::new()
                };
                info!(
                    "Output video stream {} summary: {}x{} {}{}, bitrate {} bps, profile {}, level {}",
                    context.stream_index,
                    context.encode_context.width,
                    context.encode_context.height,
                    describe_codec(target_video_codec),
                    preset,
                    context.encode_context.bit_rate,
                    describe_h264_profile(context.encode_context.profile),
                    describe_h264_level(context.encode_context.level)
                );
            }
            ffi::AVMEDIA_TYPE_AUDIO => {
                let preset = if requested_audio_quality == AudioQuality::MatchSource {
                    format!(
                        " (~{} approx)",
                        nearest_audio_preset(context.encode_context.bit_rate)
                    )
                } else {
                    String::new()
                };
                info!(
                    "Output audio stream {} summary: {} ch @ {} Hz {}{}, bitrate {} bps",
                    context.stream_index,
                    context.encode_context.ch_layout.nb_channels,
                    context.encode_context.sample_rate,
                    describe_codec(target_audio_codec),
                    preset,
                    context.encode_context.bit_rate
                );
            }
            ffi::AVMEDIA_TYPE_SUBTITLE => {
                info!(
                    "Output subtitle stream {} summary: {}",
                    context.stream_index,
                    describe_codec(ffi::AV_CODEC_ID_MOV_TEXT)
                );
            }
            _ => {}
        }
    }

    output_format_context.write_trailer()?;

    if target_video_codec == ffi::AV_CODEC_ID_H264 {
        if let (Some(expected_profile), Some(expected_level)) =
            (desired_h264_profile, desired_h264_level)
        {
            verify_output_h264_profile_level(
                output_file,
                output_path,
                expected_profile,
                expected_level,
                last_video_encoder_name.as_deref(),
                hardware_encoder_used,
            )?;
        }
    }

    Ok(())
}

fn select_primary_video_stream_index(
    input_ctx: &AVFormatContextInput,
    override_index: Option<usize>,
    criteria: PrimaryVideoCriteria,
) -> Result<usize> {
    if let Some(idx) = override_index {
        let streams = input_ctx.streams();
        if idx >= streams.len() {
            bail!(
                "--primary-video-stream-index={} out of range (streams: {})",
                idx,
                streams.len()
            );
        }
        let st = &streams[idx];
        if st.codecpar().codec_type != ffi::AVMEDIA_TYPE_VIDEO {
            bail!("--primary-video-stream-index={} is not a video stream", idx);
        }
        return Ok(idx);
    }

    let mut best_idx: Option<usize> = None;
    let mut best_score: u128 = 0;
    for st in input_ctx.streams() {
        if st.codecpar().codec_type != ffi::AVMEDIA_TYPE_VIDEO {
            continue;
        }
        let cp = st.codecpar();
        let (w, h) = (cp.width as u64, cp.height as u64);
        let area = w.saturating_mul(h);
        let br = if cp.bit_rate > 0 {
            cp.bit_rate as u64
        } else {
            0
        };
        let fps_milli: u64 = st
            .guess_framerate()
            .map(|tb| {
                let num = tb.num as i128;
                let den = if tb.den == 0 { 1 } else { tb.den } as i128;
                let v = (num * 1000) / den;
                if v < 0 {
                    0
                } else {
                    v as u64
                }
            })
            .unwrap_or(0);
        let score: u128 = match criteria {
            PrimaryVideoCriteria::Resolution => ((area as u128) << 40) + (br as u128),
            PrimaryVideoCriteria::Bitrate => ((br as u128) << 40) + (area as u128),
            PrimaryVideoCriteria::Fps => ((fps_milli as u128) << 56) + (area as u128),
        };
        if best_idx.is_none() || score > best_score {
            best_idx = Some(st.index as usize);
            best_score = score;
        }
    }
    best_idx.ok_or_else(|| anyhow!("No video streams found in input"))
}

fn print_streams_info(input_file: &CStr, filter: StreamsFilter) -> Result<()> {
    let ictx = AVFormatContextInput::open(input_file)?;
    println!("Input: {}", input_file.to_string_lossy());
    let duration_us = ictx.duration;
    if duration_us > 0 {
        println!("Duration: {} ms", duration_us / 1000);
    }
    for st in ictx.streams() {
        let idx = st.index;
        let tb = st.time_base;
        let cp = st.codecpar();
        let ctype = cp.codec_type;
        let kind_matches = match filter {
            StreamsFilter::All => true,
            StreamsFilter::Video => ctype == ffi::AVMEDIA_TYPE_VIDEO,
            StreamsFilter::Audio => ctype == ffi::AVMEDIA_TYPE_AUDIO,
            StreamsFilter::Subtitle => ctype == ffi::AVMEDIA_TYPE_SUBTITLE,
        };
        if !kind_matches {
            continue;
        }
        let cname = unsafe { std::ffi::CStr::from_ptr(ffi::avcodec_get_name(cp.codec_id)) };
        let (stream_id, disp_default, disp_forced, disp_hi, disp_vi) = unsafe {
            let s_ptr = st.as_ptr();
            let id = (*s_ptr).id;
            let d = (*s_ptr).disposition as i32;
            (
                id,
                (d & ffi::AV_DISPOSITION_DEFAULT as i32) != 0,
                (d & ffi::AV_DISPOSITION_FORCED as i32) != 0,
                (d & ffi::AV_DISPOSITION_HEARING_IMPAIRED as i32) != 0,
                (d & ffi::AV_DISPOSITION_VISUAL_IMPAIRED as i32) != 0,
            )
        };
        print!(
            "[stream {} id={}] type={:?} codec={} ",
            idx,
            stream_id,
            ctype,
            cname.to_string_lossy()
        );
        match ctype {
            ffi::AVMEDIA_TYPE_VIDEO => {
                let (w, h) = (cp.width, cp.height);
                let fps = st
                    .guess_framerate()
                    .map(|r| format!("{}/{}", r.num, r.den))
                    .unwrap_or_else(|| "?".into());
                println!(
                    "res={}x{} fps={} bitrate={} time_base={}/{}",
                    w, h, fps, cp.bit_rate, tb.num, tb.den
                );
            }
            ffi::AVMEDIA_TYPE_AUDIO => {
                let ch = cp.ch_layout.nb_channels;
                println!(
                    "channels={} sample_rate={} bitrate={} time_base={}/{}",
                    ch, cp.sample_rate, cp.bit_rate, tb.num, tb.den
                );
            }
            ffi::AVMEDIA_TYPE_SUBTITLE => {
                println!("subtitle time_base={}/{}", tb.num, tb.den);
            }
            _ => {
                println!("time_base={}/{}", tb.num, tb.den);
            }
        }
        println!(
            "  disposition: default={} forced={} hearing_impaired={} visual_impaired={}",
            disp_default, disp_forced, disp_hi, disp_vi
        );
    }
    Ok(())
}

#[derive(Serialize)]
struct JsonStreamInfo {
    index: i32,
    stream_id: i32,
    kind: String,
    codec: String,
    time_base: (i32, i32),
    #[serde(skip_serializing_if = "Option::is_none")]
    width: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    height: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    fps: Option<(i32, i32)>,
    #[serde(skip_serializing_if = "Option::is_none")]
    bitrate: Option<i64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    channels: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    sample_rate: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    language: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    title: Option<String>,
    disposition: JsonDisposition,
}

#[derive(Serialize)]
struct JsonProbe {
    input: String,
    duration_ms: Option<i64>,
    streams: Vec<JsonStreamInfo>,
}

#[derive(Serialize)]
struct JsonDisposition {
    default: bool,
    forced: bool,
    hearing_impaired: bool,
    visual_impaired: bool,
}

fn gather_streams_info_json(input_file: &CStr, filter: StreamsFilter) -> Result<JsonProbe> {
    let ictx = AVFormatContextInput::open(input_file)?;
    let duration_us = ictx.duration;
    let mut out: Vec<JsonStreamInfo> = Vec::new();
    for st in ictx.streams() {
        let idx = st.index;
        let tb = st.time_base;
        let cp = st.codecpar();
        let ctype = cp.codec_type;
        let kind = match ctype {
            ffi::AVMEDIA_TYPE_VIDEO => "video",
            ffi::AVMEDIA_TYPE_AUDIO => "audio",
            ffi::AVMEDIA_TYPE_SUBTITLE => "subtitle",
            ffi::AVMEDIA_TYPE_ATTACHMENT => "attachment",
            ffi::AVMEDIA_TYPE_DATA => "data",
            _ => "other",
        }
        .to_string();
        let kind_matches = match filter {
            StreamsFilter::All => true,
            StreamsFilter::Video => ctype == ffi::AVMEDIA_TYPE_VIDEO,
            StreamsFilter::Audio => ctype == ffi::AVMEDIA_TYPE_AUDIO,
            StreamsFilter::Subtitle => ctype == ffi::AVMEDIA_TYPE_SUBTITLE,
        };
        if !kind_matches {
            continue;
        }
        let cname = unsafe { std::ffi::CStr::from_ptr(ffi::avcodec_get_name(cp.codec_id)) };
        let (stream_id, disp_default, disp_forced, disp_hi, disp_vi) = unsafe {
            let s_ptr = st.as_ptr();
            let id = (*s_ptr).id;
            let d = (*s_ptr).disposition as i32;
            (
                id,
                (d & ffi::AV_DISPOSITION_DEFAULT as i32) != 0,
                (d & ffi::AV_DISPOSITION_FORCED as i32) != 0,
                (d & ffi::AV_DISPOSITION_HEARING_IMPAIRED as i32) != 0,
                (d & ffi::AV_DISPOSITION_VISUAL_IMPAIRED as i32) != 0,
            )
        };
        let (mut width, mut height, mut fps, mut channels, mut sample_rate) =
            (None, None, None, None, None);
        match ctype {
            ffi::AVMEDIA_TYPE_VIDEO => {
                width = Some(cp.width);
                height = Some(cp.height);
                fps = st.guess_framerate().map(|r| (r.num, r.den));
            }
            ffi::AVMEDIA_TYPE_AUDIO => {
                channels = Some(cp.ch_layout.nb_channels);
                sample_rate = Some(cp.sample_rate);
            }
            _ => {}
        }
        let (language, title): (Option<String>, Option<String>) = (None, None);
        out.push(JsonStreamInfo {
            index: idx,
            stream_id,
            kind,
            codec: cname.to_string_lossy().into_owned(),
            time_base: (tb.num, tb.den),
            width,
            height,
            fps,
            bitrate: if cp.bit_rate > 0 {
                Some(cp.bit_rate)
            } else {
                None
            },
            channels,
            sample_rate,
            language,
            title,
            disposition: JsonDisposition {
                default: disp_default,
                forced: disp_forced,
                hearing_impaired: disp_hi,
                visual_impaired: disp_vi,
            },
        });
    }
    Ok(JsonProbe {
        input: input_file.to_string_lossy().into_owned(),
        duration_ms: if duration_us > 0 {
            Some(duration_us / 1000)
        } else {
            None
        },
        streams: out,
    })
}

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
    let mut args = Args::from_arg_matches_mut(&mut matches).expect("Failed to parse CLI arguments");

    let loaded_config = config::load(args.config_file.as_deref())?;
    if let Some((_, source)) = &loaded_config {
        match source {
            config::ConfigSource::Cli(path) => {
                info!("Loaded configuration from '{}'.", path.display());
            }
            config::ConfigSource::Env(path) => {
                info!(
                    "Loaded configuration from '{}' (via {}).",
                    path.display(),
                    config::CONFIG_ENV_VAR
                );
            }
            config::ConfigSource::Default(path) => {
                info!("Loaded configuration from '{}'.", path.display());
            }
        }
    }
    if let Some((cfg, _)) = &loaded_config {
        apply_config_overrides(&mut args, cfg, &matches);
    }
    let config_plex = loaded_config
        .as_ref()
        .and_then(|(cfg, _)| cfg.plex.as_ref());

    let plex_refresher = plex::PlexRefresher::from_sources(
        config_plex,
        args.plex_refresh,
        args.plex_url.as_deref(),
        args.plex_token.as_deref(),
    )?;

    let servarr_view = ServeArrArgsView {
        has_input: args.input_file.is_some(),
        has_output: args.output_file.is_some(),
        desired_extension: &args.servarr_output_extension,
        desired_suffix: &args.servarr_output_suffix,
    };

    let servarr_preparation = servarr::prepare_from_env(servarr_view)?;
    if let IntegrationPreparation::Skip { reason } = &servarr_preparation {
        info!("{}", reason);
        return Ok(());
    }

    match &servarr_preparation {
        IntegrationPreparation::Replace(plan) => {
            log_relevant_env(plan.kind);
        }
        IntegrationPreparation::Batch(plans) => {
            if let Some(first) = plans.first() {
                log_relevant_env(first.kind);
            }
        }
        _ => {}
    }

    // Stream probing early exit
    if args.probe_streams {
        match args.output {
            OutputFormat::Json => {
                let input = args
                    .input_file
                    .as_ref()
                    .expect("<INPUT_FILE> required for --probe-streams");
                let j = gather_streams_info_json(input.as_c_str(), args.streams_filter)?;
                println!("{}", serde_json::to_string_pretty(&j).unwrap());
            }
            OutputFormat::Text => {
                let input = args
                    .input_file
                    .as_ref()
                    .expect("<INPUT_FILE> required for --probe-streams");
                print_streams_info(input.as_c_str(), args.streams_filter)?;
            }
        }
        return Ok(());
    }

    // Additional probe early exits (supports combined --probe-hw --probe-codecs)
    if args.probe_hw || args.probe_codecs {
        let want_json = args.probe_json || matches!(args.output, OutputFormat::Json);
        if want_json {
            let summary = gather_probe_json(
                args.only_video,
                args.only_hw,
                args.probe_hw,
                args.probe_codecs,
            );
            println!("{}", serde_json::to_string_pretty(&summary).unwrap());
        } else {
            if args.probe_hw {
                print_probe();
            }
            if args.probe_codecs {
                print_probe_codecs(args.only_video, args.only_hw);
            }
        }
        return Ok(());
    }
    let base_args = args.clone();
    let run_queue: Vec<Option<ReplacePlan>> = match servarr_preparation {
        IntegrationPreparation::None => vec![None],
        IntegrationPreparation::Replace(plan) => vec![Some(plan)],
        IntegrationPreparation::Batch(plans) => plans.into_iter().map(Some).collect(),
        IntegrationPreparation::Skip { .. } => unreachable!(),
    };

    for plan in run_queue {
        run_conversion(&base_args, plan, &plex_refresher)?;
    }

    Ok(())
}

fn run_conversion(
    base_args: &Args,
    plan: Option<ReplacePlan>,
    plex_refresher: &Option<plex::PlexRefresher>,
) -> Result<()> {
    let mut args = base_args.clone();

    if let Some(ref plan_ref) = plan {
        plan_ref.assign_to_args(&mut args.input_file, &mut args.output_file);
    }

    if args.input_file.is_none() || args.output_file.is_none() {
        bail!(
            "<INPUT_FILE> and <OUTPUT_FILE> are required unless you use --probe-* flags or run inside a Sonarr/Radarr Download event."
        );
    }

    let mut quality_limits = QualityLimits::default();
    quality_limits.apply_video_quality(args.video_quality);
    quality_limits.apply_audio_quality(args.audio_quality);
    if let Some(video_cap) = args.max_video_bitrate {
        quality_limits.max_video_bitrate = Some(video_cap);
    }
    if let Some(audio_cap) = args.max_audio_bitrate {
        quality_limits.max_audio_bitrate = Some(audio_cap);
    }

    debug!(
        "Video quality {}, audio quality {}, caps: resolution={:?}, video={:?} bps, audio={:?} bps",
        args.video_quality,
        args.audio_quality,
        quality_limits.max_video_dimensions,
        quality_limits.max_video_bitrate,
        quality_limits.max_audio_bitrate
    );

    let input_display = args
        .input_file
        .as_ref()
        .map(|c| c.to_string_lossy().into_owned())
        .unwrap_or_else(|| "<unset>".to_string());
    let output_display = args
        .output_file
        .as_ref()
        .map(|c| c.to_string_lossy().into_owned())
        .unwrap_or_else(|| "<unset>".to_string());

    let selections = args
        .streaming_devices
        .take()
        .unwrap_or_else(|| vec![StreamingDeviceSelection::All]);

    let mut streaming_devices: Vec<&StreamingDevice> = if selections
        .iter()
        .any(|selection| matches!(selection, StreamingDeviceSelection::All))
    {
        streaming_devices::STREAMING_DEVICES.iter().collect()
    } else {
        selections
            .into_iter()
            .filter_map(|selection| match selection {
                StreamingDeviceSelection::Model(device) => Some(device),
                StreamingDeviceSelection::All => None,
            })
            .collect()
    };

    streaming_devices.sort_by_key(|device| device.model);
    streaming_devices.dedup_by_key(|device| device.model);

    if streaming_devices.is_empty() {
        bail!("No streaming devices resolved from CLI arguments.");
    }

    let common_video_codec = StreamingDevice::get_common_video_codec(&streaming_devices)?;
    let common_audio_codec = StreamingDevice::get_common_audio_codec(&streaming_devices)?;
    let min_h264_profile = StreamingDevice::get_min_h264_profile(&streaming_devices)?;
    let min_h264_level = StreamingDevice::get_min_h264_level(&streaming_devices)?;
    let min_fps = StreamingDevice::get_min_fps(&streaming_devices)?;
    let min_resolution = StreamingDevice::get_min_resolution(&streaming_devices)?;
    let device_cap = resolution_to_dimensions(min_resolution);

    let device_names = streaming_devices
        .iter()
        .map(|device| device.name)
        .collect::<Vec<_>>();

    info!("Converting '{}' -> '{}'", input_display, output_display);
    info!(
        "Target streaming devices ({}): {}",
        device_names.len(),
        device_names.join(", ")
    );
    info!("Hardware acceleration preference: {:?}", args.hw_accel);
    info!(
        "Video quality preset: {} ({}; bitrate {})",
        args.video_quality,
        describe_resolution(quality_limits.max_video_dimensions),
        describe_bitrate(quality_limits.max_video_bitrate)
    );
    info!(
        "Audio quality preset: {} (bitrate {})",
        args.audio_quality,
        describe_bitrate(quality_limits.max_audio_bitrate)
    );
    info!(
        "Device capability ceiling: {}x{}, H.264 profile {:?}, level {:?}",
        device_cap.0, device_cap.1, min_h264_profile, min_h264_level
    );

    let input_file = args
        .input_file
        .as_ref()
        .map(|s| s.as_c_str())
        .expect("INPUT_FILE is required unless using --probe-* flags");
    let output_file = args
        .output_file
        .as_ref()
        .map(|s| s.as_c_str())
        .expect("OUTPUT_FILE is required unless using --probe-* flags");

    match assess_direct_play_compatibility(
        input_file,
        common_video_codec,
        common_audio_codec,
        min_h264_profile,
        min_h264_level,
        min_fps,
        device_cap,
        &quality_limits,
        args.primary_video_stream_index,
        args.primary_video_criteria,
    ) {
        Ok(assessment) => {
            if assessment.compatible {
                info!(
                    "Input is already direct-play compatible for the requested devices; skipping conversion."
                );
                return Ok(());
            }
            info!("Transcoding required to satisfy requested device constraints.");
            for reason in assessment.reasons {
                info!("  - {}", reason);
            }
        }
        Err(err) => {
            warn!(
                "Unable to determine direct-play compatibility automatically; proceeding with conversion: {}",
                err
            );
        }
    }
    let _conversion_slot = acquire_slot()?;

    let mut conversion_result = convert_video_file(
        input_file,
        output_file,
        common_video_codec,
        common_audio_codec,
        min_h264_profile,
        min_h264_level,
        min_fps,
        min_resolution,
        &quality_limits,
        args.unsupported_video_policy,
        args.primary_video_stream_index,
        args.primary_video_criteria,
        args.video_quality,
        args.audio_quality,
        args.hw_accel,
    );

    if let Err(err) = conversion_result {
        conversion_result = match err.downcast::<HwProfileLevelMismatch>() {
            Ok(mismatch) => {
                if args.hw_accel != HwAccel::None && mismatch.used_hw_encoder {
                    let actual_profile = mismatch
                        .actual_profile
                        .map(|p| format!("{:?}", p))
                        .unwrap_or_else(|| "unknown".to_string());
                    let actual_level = mismatch
                        .actual_level
                        .map(|l| l.ffmpeg_name().to_string())
                        .unwrap_or_else(|| "unknown".to_string());
                    warn!(
                        "Hardware encoder {} produced H.264 profile {} level {} for '{}' (expected profile {:?} level {:?}); retrying with software encoder (libx264)",
                        mismatch.encoder,
                        actual_profile,
                        actual_level,
                        mismatch.output_path,
                        mismatch.expected_profile,
                        mismatch.expected_level
                    );
                    if let Some(output_cstr) = args.output_file.as_ref() {
                        let output_path = PathBuf::from(output_cstr.to_string_lossy().into_owned());
                        if output_path.exists() {
                            if let Err(remove_err) = fs::remove_file(&output_path) {
                                warn!(
                                    "Failed to remove incompatible output '{}': {}",
                                    output_path.display(),
                                    remove_err
                                );
                            }
                        }
                    }
                    convert_video_file(
                        input_file,
                        output_file,
                        common_video_codec,
                        common_audio_codec,
                        min_h264_profile,
                        min_h264_level,
                        min_fps,
                        min_resolution,
                        &quality_limits,
                        args.unsupported_video_policy,
                        args.primary_video_stream_index,
                        args.primary_video_criteria,
                        args.video_quality,
                        args.audio_quality,
                        HwAccel::None,
                    )
                } else {
                    Err(anyhow!(mismatch))
                }
            }
            Err(err) => Err(err),
        };
    }

    match (plan, conversion_result) {
        (Some(plan), Ok(())) => {
            let final_path = plan.finalize_success()?;
            if let Some(ref refresher) = plex_refresher {
                if let Err(err) = refresher.refresh_path(&final_path) {
                    warn!(
                        "Plex refresh failed for '{}': {}",
                        final_path.display(),
                        err
                    );
                }
            }
            Ok(())
        }
        (Some(plan), Err(err)) => {
            if let Err(cleanup_err) = plan.abort_on_failure() {
                warn!(
                    "Failed to clean up after {:?} integration error: {}",
                    plan.kind, cleanup_err
                );
            }
            Err(err)
        }
        (None, Ok(())) => {
            if args.delete_source.unwrap_or(false) {
                if let (Some(input_cstr), Some(output_cstr)) =
                    (args.input_file.as_ref(), args.output_file.as_ref())
                {
                    let input_path = PathBuf::from(input_cstr.to_string_lossy().into_owned());
                    let output_path = PathBuf::from(output_cstr.to_string_lossy().into_owned());
                    if input_path != output_path {
                        match fs::remove_file(&input_path) {
                            Ok(_) => info!(
                                "Deleted source file '{}' after successful conversion",
                                input_path.display()
                            ),
                            Err(err) => warn!(
                                "Failed to delete source file '{}': {}",
                                input_path.display(),
                                err
                            ),
                        }
                    } else {
                        warn!(
                            "Skipping --delete-source because input and output paths are identical"
                        );
                    }
                }
            }
            if let Some(ref refresher) = plex_refresher {
                if let Some(output_cstr) = args.output_file.as_ref() {
                    let output_path = PathBuf::from(output_cstr.to_string_lossy().into_owned());
                    if let Err(err) = refresher.refresh_path(&output_path) {
                        warn!(
                            "Plex refresh failed for '{}': {}",
                            output_path.display(),
                            err
                        );
                    }
                }
            }
            Ok(())
        }
        (None, Err(err)) => Err(err),
    }
}
