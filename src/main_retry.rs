use crate::*;

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

#[allow(clippy::too_many_arguments)]
pub(super) fn retry_with_software_encoder(
    input_file: &CStr,
    output_file: &CStr,
    sub_mode: SubMode,
    target_video_codec: ffi::AVCodecID,
    target_audio_codec: ffi::AVCodecID,
    h264_constraints: Option<(H264Profile, H264Level)>,
    min_fps: u32,
    min_resolution: Resolution,
    quality_limits: &QualityLimits,
    uv_policy: UnsupportedVideoPolicy,
    primary_video_stream_index: Option<usize>,
    primary_video_criteria: PrimaryVideoCriteria,
    requested_video_quality: VideoQuality,
    requested_audio_quality: AudioQuality,
    skip_codec_check: bool,
) -> Result<ConversionOutcome, anyhow::Error> {
    convert_video_file(
        input_file,
        output_file,
        sub_mode,
        target_video_codec,
        target_audio_codec,
        h264_constraints,
        min_fps,
        min_resolution,
        quality_limits,
        uv_policy,
        primary_video_stream_index,
        primary_video_criteria,
        requested_video_quality,
        requested_audio_quality,
        skip_codec_check,
        HwAccel::None,
    )
}

#[allow(clippy::too_many_arguments)]
pub(super) fn handle_hw_profile_mismatch(
    mismatch: HwProfileLevelMismatch,
    args: &Args,
    input_file: &CStr,
    output_file: &CStr,
    sub_mode: SubMode,
    target_video_codec: ffi::AVCodecID,
    target_audio_codec: ffi::AVCodecID,
    h264_constraints: Option<(H264Profile, H264Level)>,
    min_fps: u32,
    min_resolution: Resolution,
    quality_limits: &QualityLimits,
) -> Result<ConversionOutcome, anyhow::Error> {
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
    retry_with_software_encoder(
        input_file,
        output_file,
        sub_mode,
        target_video_codec,
        target_audio_codec,
        h264_constraints,
        min_fps,
        min_resolution,
        quality_limits,
        args.unsupported_video_policy,
        args.primary_video_stream_index,
        args.primary_video_criteria,
        args.video_quality,
        args.audio_quality,
        args.skip_codec_check,
    )
}

#[allow(clippy::too_many_arguments)]
pub(super) fn handle_hw_encoder_init_error(
    init_error: HwEncoderInitError,
    args: &Args,
    input_file: &CStr,
    output_file: &CStr,
    sub_mode: SubMode,
    target_video_codec: ffi::AVCodecID,
    target_audio_codec: ffi::AVCodecID,
    h264_constraints: Option<(H264Profile, H264Level)>,
    min_fps: u32,
    min_resolution: Resolution,
    quality_limits: &QualityLimits,
) -> Result<ConversionOutcome, anyhow::Error> {
    if args.hw_accel == HwAccel::None {
        return Err(anyhow!(init_error));
    }

    warn!(
        "Hardware encoder {} failed to initialize ({}); retrying with software encoder",
        init_error.encoder, init_error.message
    );
    cleanup_partial_output(output_file);
    retry_with_software_encoder(
        input_file,
        output_file,
        sub_mode,
        target_video_codec,
        target_audio_codec,
        h264_constraints,
        min_fps,
        min_resolution,
        quality_limits,
        args.unsupported_video_policy,
        args.primary_video_stream_index,
        args.primary_video_criteria,
        args.video_quality,
        args.audio_quality,
        args.skip_codec_check,
    )
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
