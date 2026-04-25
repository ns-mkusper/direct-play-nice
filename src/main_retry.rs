use anyhow::{anyhow, bail, Result};
use log::warn;
use rsmpeg::avformat::AVFormatContextInput;
use rsmpeg::ffi;
use std::ffi::CStr;
use std::fs;
use std::path::PathBuf;

use crate::gpu::HwAccel;
use crate::{
    convert_video_file, Args, ConversionOutcome, ConversionParams, HwEncoderInitError,
    HwProfileLevelMismatch, PrimaryVideoCriteria,
};

pub(super) fn cleanup_partial_output(path: &CStr) {
    let output_path = PathBuf::from(path.to_string_lossy().into_owned());
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

pub(super) fn retry_with_software_encoder(
    input_file: &CStr,
    output_file: &CStr,
    params: ConversionParams<'_>,
) -> Result<ConversionOutcome> {
    convert_video_file(input_file, output_file, params.with_hw_accel(HwAccel::None))
}

pub(super) fn handle_hw_profile_mismatch(
    mismatch: HwProfileLevelMismatch,
    args: &Args,
    input_file: &CStr,
    output_file: &CStr,
    params: ConversionParams<'_>,
) -> Result<ConversionOutcome> {
    if args.hw_accel == HwAccel::None || !mismatch.used_hw_encoder {
        return Err(anyhow!(mismatch));
    }

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
    cleanup_partial_output(output_file);
    retry_with_software_encoder(input_file, output_file, params)
}

pub(super) fn handle_hw_encoder_init_error(
    init_error: HwEncoderInitError,
    args: &Args,
    input_file: &CStr,
    output_file: &CStr,
    params: ConversionParams<'_>,
) -> Result<ConversionOutcome> {
    if args.hw_accel == HwAccel::None {
        return Err(anyhow!(init_error));
    }

    warn!(
        "Hardware encoder {} failed to initialize ({}); retrying with software encoder",
        init_error.encoder, init_error.message
    );
    cleanup_partial_output(output_file);
    retry_with_software_encoder(input_file, output_file, params)
}

pub(super) fn select_primary_video_stream_index(
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
