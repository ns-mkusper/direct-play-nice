use crate::transcoder::prelude::*;

/// Executes the preferred audio frame size routine.
pub(crate) fn preferred_audio_frame_size(encode_context: &AVCodecContext, fifo_size: i32) -> i32 {
    if encode_context.frame_size > 0 {
        return encode_context.frame_size;
    }
    // Some AAC encoder backends report frame_size=0 before/while opening.
    // Use sane chunking so we can keep progress moving and drain the FIFO safely.
    if fifo_size > 0 {
        return fifo_size.clamp(1, 1024);
    }
    1024
}

const MAX_REASONABLE_FPS: f64 = 300.0;

/// Executes the is valid framerate routine.
fn is_valid_framerate(r: ffi::AVRational) -> bool {
    r.num > 0 && r.den > 0 && (r.num as f64 / r.den as f64) <= MAX_REASONABLE_FPS
}

/// Executes the derive stream framerate routine.
fn derive_stream_framerate(
    decode_context: &AVCodecContext,
    input_stream: &AVStreamRef,
) -> Option<ffi::AVRational> {
    unsafe {
        let ctx_ptr = decode_context.as_ptr();
        if !ctx_ptr.is_null() {
            let ctx_rate = (*ctx_ptr).framerate;
            if is_valid_framerate(ctx_rate) {
                return Some(ctx_rate);
            }
        }
    }

    if let Some(rate) = input_stream.guess_framerate() {
        if is_valid_framerate(rate) {
            return Some(rate);
        }
    }

    unsafe {
        let stream_ptr = input_stream.as_ptr();
        if !stream_ptr.is_null() {
            let avg = (*stream_ptr).avg_frame_rate;
            if is_valid_framerate(avg) {
                return Some(avg);
            }
            let reported = (*stream_ptr).r_frame_rate;
            if is_valid_framerate(reported) {
                return Some(reported);
            }
        }
    }

    None
}

/// Executes the configure video timing routine.
fn configure_video_timing(
    decode_context: &AVCodecContext,
    encode_context: &mut AVCodecContext,
    output_stream: &mut AVStreamMut,
    input_stream: &AVStreamRef,
) {
    let mut encode_time_base = decode_context.time_base;

    if let Some(framerate) = derive_stream_framerate(decode_context, input_stream) {
        if let Some(fps) = rational_to_f64(framerate) {
            debug!(
                "Using derived output frame rate {:.3} fps ({} / {})",
                fps, framerate.num, framerate.den
            );
        }
        encode_context.set_framerate(framerate);
        encode_time_base = ra(framerate.den, framerate.num);
    } else if encode_time_base.num > 0 {
        let implied_fps = encode_time_base.den as f64 / encode_time_base.num as f64;
        if implied_fps > MAX_REASONABLE_FPS {
            let fallback = ffi::AVRational {
                num: 1001,
                den: 24000,
            };
            debug!(
                "Input time base {} implies {:.1} fps; overriding to fallback time base {}",
                rational_to_string(encode_time_base),
                // Implements behavior for `ied_fps,`.
                implied_fps,
                rational_to_string(fallback)
            );
            encode_time_base = fallback;
        }
    }

    encode_context.set_time_base(encode_time_base);
    output_stream.set_time_base(encode_time_base);
}

/// Stores data for H264VideoCodecParams.
pub(crate) struct H264VideoCodecParams<'a> {
    pub(crate) h264_profile: H264Profile,
    pub(crate) h264_level: H264Level,
    pub(crate) quality_limits: &'a QualityLimits,
    pub(crate) device_max_resolution: Resolution,
    pub(crate) source_bit_rate_hint: i64,
    pub(crate) encoder_name: &'a str,
    pub(crate) is_constant_quality_mode: bool,
}

/// Executes the set h264 video codec par routine.
pub(crate) fn set_h264_video_codec_par(
    decode_context: &mut AVCodecContext,
    encode_context: &mut AVCodecContext,
    output_stream: &mut AVStreamMut,
    input_stream: &AVStreamRef,
    params: H264VideoCodecParams<'_>,
) {
    let H264VideoCodecParams {
        h264_profile,
        h264_level,
        quality_limits,
        device_max_resolution,
        source_bit_rate_hint,
        encoder_name,
        is_constant_quality_mode,
    } = params;
    encode_context.set_sample_rate(decode_context.sample_rate);
    let device_cap = device_max_resolution.to_dimensions();
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
    configure_video_timing(decode_context, encode_context, output_stream, input_stream);
    // Keep YUV420P for broad direct-play compatibility.
    encode_context.set_pix_fmt(ffi::AV_PIX_FMT_YUV420P);
    encode_context.set_max_b_frames(decode_context.max_b_frames);

    let default_hint = default_video_bitrate(target_width, target_height);
    let source_bit_rate = if decode_context.bit_rate > 0 {
        decode_context.bit_rate
    } else if source_bit_rate_hint > 0 {
        source_bit_rate_hint
    } else {
        default_hint
    };
    let rate_hint = derive_target_bitrate(source_bit_rate, quality_limits.max_video_bitrate)
        .or_else(|| (source_bit_rate > 0).then_some(source_bit_rate))
        .or(Some(default_hint));

    if !is_constant_quality_mode {
        if let Some(bit_rate) = rate_hint {
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
        debug!(
            "Video encoding set to Constant Quality (CQ/CQP) mode. VBV derived from {} bps.",
            rate_hint.unwrap_or(default_hint)
        );
        encode_context.set_bit_rate(0);
    }

    apply_hw_encoder_quality(
        encode_context.as_mut_ptr(),
        encoder_name,
        rate_hint,
        is_constant_quality_mode,
        Some(h264_level),
    );
    encode_context.set_gop_size(decode_context.gop_size);
    encode_context.set_sample_aspect_ratio(decode_context.sample_aspect_ratio);
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
    log_encoder_state("video setup", encode_context, encoder_name);
    // Codec parameters are extracted after the encoder is opened.
}

/// Stores data for HevcVideoCodecParams.
pub(crate) struct HevcVideoCodecParams<'a> {
    pub(crate) quality_limits: &'a QualityLimits,
    pub(crate) device_max_resolution: Resolution,
    pub(crate) source_bit_rate_hint: i64,
    pub(crate) encoder_name: &'a str,
    pub(crate) is_constant_quality_mode: bool,
}

/// Executes the set hevc video codec par routine.
pub(crate) fn set_hevc_video_codec_par(
    decode_context: &mut AVCodecContext,
    encode_context: &mut AVCodecContext,
    output_stream: &mut AVStreamMut,
    input_stream: &AVStreamRef,
    params: HevcVideoCodecParams<'_>,
) {
    let HevcVideoCodecParams {
        quality_limits,
        device_max_resolution,
        source_bit_rate_hint,
        encoder_name,
        is_constant_quality_mode,
    } = params;
    encode_context.set_sample_rate(decode_context.sample_rate);
    let device_cap = device_max_resolution.to_dimensions();
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
    configure_video_timing(decode_context, encode_context, output_stream, input_stream);
    encode_context.set_pix_fmt(ffi::AV_PIX_FMT_YUV420P);
    encode_context.set_max_b_frames(decode_context.max_b_frames);

    let default_hint = default_video_bitrate(target_width, target_height);
    let source_bit_rate = if decode_context.bit_rate > 0 {
        decode_context.bit_rate
    } else if source_bit_rate_hint > 0 {
        source_bit_rate_hint
    } else {
        default_hint
    };
    let rate_hint = derive_target_bitrate(source_bit_rate, quality_limits.max_video_bitrate)
        .or_else(|| (source_bit_rate > 0).then_some(source_bit_rate))
        .or(Some(default_hint));

    if !is_constant_quality_mode {
        if let Some(bit_rate) = rate_hint {
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
        debug!(
            "Video encoding set to Constant Quality (CQ/CQP) mode. VBV derived from {} bps.",
            rate_hint.unwrap_or(default_hint)
        );
        encode_context.set_bit_rate(0);
    }

    apply_hw_encoder_quality(
        encode_context.as_mut_ptr(),
        encoder_name,
        rate_hint,
        is_constant_quality_mode,
        None,
    );
    log_encoder_state("video setup", encode_context, encoder_name);
    encode_context.set_gop_size(decode_context.gop_size);
    encode_context.set_sample_aspect_ratio(decode_context.sample_aspect_ratio);
    // Codec parameters are extracted after the encoder is opened.
}

/// Executes the set audio codec par routine.
pub(crate) fn set_audio_codec_par(
    decode_context: &mut AVCodecContext,
    encode_context: &mut AVCodecContext,
    _output_stream: &mut AVStreamMut,
    quality_limits: &QualityLimits,
    source_bit_rate_hint: i64,
) -> Result<()> {
    let encoder = AVCodec::find_encoder(ffi::AV_CODEC_ID_AAC)
        .ok_or_else(|| anyhow!("Could not find AAC encoder"))?;
    let decode_channels = decode_context.ch_layout.nb_channels;
    encode_context.set_ch_layout(AVChannelLayout::from_nb_channels(decode_channels).into_inner());
    // The input file's sample rate is used to avoid a sample rate conversion.
    encode_context.set_sample_rate(decode_context.sample_rate);
    let sample_fmt = encoder
        .sample_fmts()
        .and_then(|formats| {
            formats
                .iter()
                .copied()
                .find(|fmt| *fmt == ffi::AV_SAMPLE_FMT_FLTP)
                .or_else(|| formats.first().copied())
        })
        .unwrap_or(ffi::AV_SAMPLE_FMT_FLTP);
    encode_context.set_sample_fmt(sample_fmt);
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
    Ok(())
}

/// Executes the set subtitle codec par routine.
pub(crate) fn set_subtitle_codec_par(
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

        // SAFETY: FFmpeg expects subtitle_header to be allocated with av_malloc* and owned by
        // the codec context. We allocate exactly new_subtitle_header.len() bytes and copy from a
        // valid in-memory buffer of the same length.
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
