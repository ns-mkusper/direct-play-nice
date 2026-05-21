//! FFmpeg diagnostics helpers that bridge native logging and format actionable runtime diagnostics.

use crate::transcoder::prelude::*;
use std::{env, ffi::c_char};

pub(crate) fn av_error_to_string(err: i32) -> String {
    let mut buf = [0 as c_char; ffi::AV_ERROR_MAX_STRING_SIZE as usize];
    unsafe {
        if ffi::av_strerror(err, buf.as_mut_ptr(), buf.len()) == 0 {
            CStr::from_ptr(buf.as_ptr()).to_string_lossy().into_owned()
        } else {
            format!("ffmpeg error {}", err)
        }
    }
}

pub(crate) fn pix_fmt_name(fmt: ffi::AVPixelFormat) -> String {
    unsafe {
        let ptr = ffi::av_get_pix_fmt_name(fmt);
        if ptr.is_null() {
            format!("pix_fmt({})", fmt)
        } else {
            CStr::from_ptr(ptr).to_string_lossy().into_owned()
        }
    }
}

fn sample_fmt_name(fmt: ffi::AVSampleFormat) -> String {
    unsafe {
        let ptr = ffi::av_get_sample_fmt_name(fmt);
        if ptr.is_null() {
            format!("sample_fmt({})", fmt)
        } else {
            CStr::from_ptr(ptr).to_string_lossy().into_owned()
        }
    }
}

fn sample_fmt_name_from_i32(fmt: i32) -> String {
    sample_fmt_name(fmt as ffi::AVSampleFormat)
}

fn media_type_name(media_type: ffi::AVMediaType) -> String {
    unsafe {
        let ptr = ffi::av_get_media_type_string(media_type);
        if ptr.is_null() {
            format!("media_type({})", media_type)
        } else {
            CStr::from_ptr(ptr).to_string_lossy().into_owned()
        }
    }
}

fn codec_id_name(codec_id: ffi::AVCodecID) -> String {
    unsafe {
        let ptr = ffi::avcodec_get_name(codec_id);
        if ptr.is_null() {
            format!("codec({})", codec_id)
        } else {
            CStr::from_ptr(ptr).to_string_lossy().into_owned()
        }
    }
}

fn profile_label(codec_id: ffi::AVCodecID, profile: i32) -> String {
    if codec_id == ffi::AV_CODEC_ID_H264 {
        format!("{} ({})", profile, describe_h264_profile(profile))
    } else {
        profile.to_string()
    }
}

fn level_label(codec_id: ffi::AVCodecID, level: i32) -> String {
    if codec_id == ffi::AV_CODEC_ID_H264 {
        format!("{} ({})", level, describe_h264_level(level))
    } else {
        level.to_string()
    }
}

fn field_order_name(order: ffi::AVFieldOrder) -> &'static str {
    match order {
        ffi::AV_FIELD_UNKNOWN => "unknown",
        ffi::AV_FIELD_PROGRESSIVE => "progressive",
        ffi::AV_FIELD_TT => "tt (top coded/display top)",
        ffi::AV_FIELD_BB => "bb (bottom coded/display bottom)",
        ffi::AV_FIELD_TB => "tb (top coded/bottom display)",
        ffi::AV_FIELD_BT => "bt (bottom coded/top display)",
        _ => "invalid",
    }
}

fn color_range_name(range: ffi::AVColorRange) -> String {
    unsafe {
        let ptr = ffi::av_color_range_name(range);
        if ptr.is_null() {
            format!("range({})", range)
        } else {
            CStr::from_ptr(ptr).to_string_lossy().into_owned()
        }
    }
}

fn color_primaries_name(primaries: ffi::AVColorPrimaries) -> String {
    unsafe {
        let ptr = ffi::av_color_primaries_name(primaries);
        if ptr.is_null() {
            format!("primaries({})", primaries)
        } else {
            CStr::from_ptr(ptr).to_string_lossy().into_owned()
        }
    }
}

fn color_trc_name(trc: ffi::AVColorTransferCharacteristic) -> String {
    unsafe {
        let ptr = ffi::av_color_transfer_name(trc);
        if ptr.is_null() {
            format!("transfer({})", trc)
        } else {
            CStr::from_ptr(ptr).to_string_lossy().into_owned()
        }
    }
}

fn color_space_name(space: ffi::AVColorSpace) -> String {
    unsafe {
        let ptr = ffi::av_color_space_name(space);
        if ptr.is_null() {
            format!("colorspace({})", space)
        } else {
            CStr::from_ptr(ptr).to_string_lossy().into_owned()
        }
    }
}

fn chroma_location_name(loc: ffi::AVChromaLocation) -> String {
    unsafe {
        let ptr = ffi::av_chroma_location_name(loc);
        if ptr.is_null() {
            format!("chroma_loc({})", loc)
        } else {
            CStr::from_ptr(ptr).to_string_lossy().into_owned()
        }
    }
}

fn channel_order_name(order: ffi::AVChannelOrder) -> &'static str {
    match order {
        ffi::AV_CHANNEL_ORDER_UNSPEC => "unspecified",
        ffi::AV_CHANNEL_ORDER_NATIVE => "native",
        ffi::AV_CHANNEL_ORDER_CUSTOM => "custom",
        ffi::AV_CHANNEL_ORDER_AMBISONIC => "ambisonic",
        _ => "invalid",
    }
}

fn describe_channel_layout(layout: &ffi::AVChannelLayout) -> String {
    unsafe {
        if layout.nb_channels <= 0 {
            return "unset".to_string();
        }

        let mut buf = [0 as c_char; 128];
        let res = ffi::av_channel_layout_describe(
            layout as *const ffi::AVChannelLayout,
            buf.as_mut_ptr(),
            buf.len(),
        );
        if res >= 0 {
            CStr::from_ptr(buf.as_ptr()).to_string_lossy().into_owned()
        } else if layout.order == ffi::AV_CHANNEL_ORDER_NATIVE {
            format!("mask=0x{:x}", layout.u.mask)
        } else {
            format!(
                "order={} channels={}",
                channel_order_name(layout.order),
                layout.nb_channels
            )
        }
    }
}

pub(crate) fn rational_to_string(r: ffi::AVRational) -> String {
    format!("{}/{}", r.num, r.den)
}

fn codec_params_format_string(media_type: ffi::AVMediaType, format: i32) -> String {
    match media_type {
        mt if mt == ffi::AVMEDIA_TYPE_VIDEO => {
            let name = pix_fmt_name(format as ffi::AVPixelFormat);
            format!("{} ({})", format, name)
        }
        mt if mt == ffi::AVMEDIA_TYPE_AUDIO => {
            let name = sample_fmt_name_from_i32(format);
            format!("{} ({})", format, name)
        }
        _ => format!("{}", format),
    }
}

fn build_codec_context_lines(raw: *const ffi::AVCodecContext) -> Vec<String> {
    unsafe {
        let mut lines = Vec::new();
        lines.push(format!("    [AVCodecContext @ {:p}]", raw));
        lines.push(format!(
            "      codec_type: {} ({})",
            media_type_name((*raw).codec_type),
            (*raw).codec_type
        ));
        lines.push(format!(
            "      codec_id: {} ({})",
            codec_id_name((*raw).codec_id),
            (*raw).codec_id
        ));
        lines.push(format!(
            "      profile: {}",
            profile_label((*raw).codec_id, (*raw).profile)
        ));
        lines.push(format!(
            "      level: {}",
            level_label((*raw).codec_id, (*raw).level)
        ));
        lines.push(format!(
            "      time_base: {}",
            rational_to_string((*raw).time_base)
        ));
        lines.push(format!(
            "      framerate: {}",
            rational_to_string((*raw).framerate)
        ));
        lines.push(format!("      bit_rate: {}", (*raw).bit_rate));
        lines.push(format!("      rc_max_rate: {}", (*raw).rc_max_rate));
        lines.push(format!("      rc_min_rate: {}", (*raw).rc_min_rate));
        lines.push(format!("      rc_buffer_size: {}", (*raw).rc_buffer_size));
        lines.push(format!(
            "      rc_initial_buffer_occupancy: {}",
            (*raw).rc_initial_buffer_occupancy
        ));
        lines.push(format!(
            "      bit_rate_tolerance: {}",
            (*raw).bit_rate_tolerance
        ));
        lines.push(format!("      width: {}", (*raw).width));
        lines.push(format!("      height: {}", (*raw).height));
        lines.push(format!("      coded_width: {}", (*raw).coded_width));
        lines.push(format!("      coded_height: {}", (*raw).coded_height));
        lines.push(format!("      gop_size: {}", (*raw).gop_size));
        lines.push(format!("      max_b_frames: {}", (*raw).max_b_frames));
        lines.push(format!("      has_b_frames: {}", (*raw).has_b_frames));
        lines.push(format!("      refs: {}", (*raw).refs));
        lines.push(format!("      pix_fmt: {}", pix_fmt_name((*raw).pix_fmt)));
        lines.push(format!(
            "      sample_fmt: {}",
            sample_fmt_name((*raw).sample_fmt)
        ));
        lines.push(format!("      sample_rate: {}", (*raw).sample_rate));
        lines.push(format!(
            "      channel_layout: {}",
            describe_channel_layout(&(*raw).ch_layout)
        ));
        lines.push(format!(
            "      channel_order: {} ({})",
            channel_order_name((*raw).ch_layout.order),
            (*raw).ch_layout.order
        ));
        if (*raw).ch_layout.order == ffi::AV_CHANNEL_ORDER_NATIVE {
            lines.push(format!(
                "      channel_mask: 0x{:x}",
                (*raw).ch_layout.u.mask
            ));
        }
        lines.push(format!("      thread_count: {}", (*raw).thread_count));
        lines.push(format!("      thread_type: {}", (*raw).thread_type));
        lines.push(format!("      flags: 0x{:x}", (*raw).flags));
        lines.push(format!("      flags2: 0x{:x}", (*raw).flags2));
        lines.push(format!(
            "      color_range: {} ({})",
            color_range_name((*raw).color_range),
            (*raw).color_range
        ));
        lines.push(format!(
            "      color_primaries: {} ({})",
            color_primaries_name((*raw).color_primaries),
            (*raw).color_primaries
        ));
        lines.push(format!(
            "      color_trc: {} ({})",
            color_trc_name((*raw).color_trc),
            (*raw).color_trc
        ));
        lines.push(format!(
            "      color_space: {} ({})",
            color_space_name((*raw).colorspace),
            (*raw).colorspace
        ));
        lines.push(format!(
            "      chroma_location: {} ({})",
            chroma_location_name((*raw).chroma_sample_location),
            (*raw).chroma_sample_location
        ));
        lines.push(format!("      hw_device_ctx: {:p}", (*raw).hw_device_ctx));
        lines.push(format!("      hw_frames_ctx: {:p}", (*raw).hw_frames_ctx));
        lines
    }
}

fn build_codec_parameters_lines(raw: *const ffi::AVCodecContext) -> Option<Vec<String>> {
    unsafe {
        let mut params = ffi::avcodec_parameters_alloc();
        if params.is_null() {
            return None;
        }
        let params_ptr = params;
        if ffi::avcodec_parameters_from_context(params, raw) < 0 {
            ffi::avcodec_parameters_free(&mut params);
            return None;
        }

        let mut lines = Vec::new();
        lines.push(format!("    [AVCodecParameters @ {:p}]", params_ptr));
        lines.push(format!(
            "      codec_type: {} ({})",
            media_type_name((*params).codec_type),
            (*params).codec_type
        ));
        lines.push(format!(
            "      codec_id: {} ({})",
            codec_id_name((*params).codec_id),
            (*params).codec_id
        ));
        lines.push(format!("      codec_tag: 0x{:08x}", (*params).codec_tag));
        lines.push(format!(
            "      format: {}",
            codec_params_format_string((*params).codec_type, (*params).format)
        ));
        lines.push(format!("      bit_rate: {}", (*params).bit_rate));
        lines.push(format!(
            "      bits_per_coded_sample: {}",
            (*params).bits_per_coded_sample
        ));
        lines.push(format!(
            "      bits_per_raw_sample: {}",
            (*params).bits_per_raw_sample
        ));
        lines.push(format!(
            "      profile: {}",
            profile_label((*params).codec_id, (*params).profile)
        ));
        lines.push(format!(
            "      level: {}",
            level_label((*params).codec_id, (*params).level)
        ));
        lines.push(format!("      width: {}", (*params).width));
        lines.push(format!("      height: {}", (*params).height));
        lines.push(format!(
            "      sample_aspect_ratio: {}",
            rational_to_string((*params).sample_aspect_ratio)
        ));
        lines.push(format!(
            "      field_order: {} ({})",
            field_order_name((*params).field_order),
            (*params).field_order
        ));
        lines.push(format!(
            "      color_range: {} ({})",
            color_range_name((*params).color_range),
            (*params).color_range
        ));
        lines.push(format!(
            "      color_primaries: {} ({})",
            color_primaries_name((*params).color_primaries),
            (*params).color_primaries
        ));
        lines.push(format!(
            "      color_trc: {} ({})",
            color_trc_name((*params).color_trc),
            (*params).color_trc
        ));
        lines.push(format!(
            "      color_space: {} ({})",
            color_space_name((*params).color_space),
            (*params).color_space
        ));
        lines.push(format!(
            "      chroma_location: {} ({})",
            chroma_location_name((*params).chroma_location),
            (*params).chroma_location
        ));
        lines.push(format!("      video_delay: {}", (*params).video_delay));
        lines.push(format!("      sample_rate: {}", (*params).sample_rate));
        lines.push(format!("      block_align: {}", (*params).block_align));
        lines.push(format!("      frame_size: {}", (*params).frame_size));
        lines.push(format!(
            "      initial_padding: {}",
            (*params).initial_padding
        ));
        lines.push(format!(
            "      trailing_padding: {}",
            (*params).trailing_padding
        ));
        lines.push(format!("      seek_preroll: {}", (*params).seek_preroll));
        lines.push(format!(
            "      channels: {}",
            (*params).ch_layout.nb_channels
        ));
        lines.push(format!(
            "      channel_order: {} ({})",
            channel_order_name((*params).ch_layout.order),
            (*params).ch_layout.order
        ));
        lines.push(format!(
            "      channel_layout: {}",
            describe_channel_layout(&(*params).ch_layout)
        ));
        if (*params).ch_layout.order == ffi::AV_CHANNEL_ORDER_NATIVE {
            lines.push(format!(
                "      channel_mask: 0x{:x}",
                (*params).ch_layout.u.mask
            ));
        }
        lines.push(format!(
            "      extradata_size: {}",
            (*params).extradata_size
        ));

        ffi::avcodec_parameters_free(&mut params);
        Some(lines)
    }
}

fn build_encoder_debug_dump(raw: *const ffi::AVCodecContext) -> Option<String> {
    if raw.is_null() {
        return None;
    }
    let mut lines = build_codec_context_lines(raw);
    if let Some(mut params_lines) = build_codec_parameters_lines(raw) {
        lines.push(String::new());
        lines.append(&mut params_lines);
    }

    Some(lines.join("\n"))
}

pub(crate) fn log_encoder_state(stage: &str, ctx: &AVCodecContext, encoder_name: &str) {
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
        if log::log_enabled!(Level::Debug) {
            if let Some(detail) = build_encoder_debug_dump(raw) {
                debug!(
                    "Encoder {} [{}] raw codec state:\n{}",
                    encoder_name, stage, detail
                );
            }
        }
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
        "quiet" => ffi::AV_LOG_QUIET,
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
        x if x <= ffi::AV_LOG_QUIET => "quiet",
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

pub(crate) fn configure_ffmpeg_logging() {
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

pub(crate) fn apply_hw_encoder_quality(
    ctx: *mut ffi::AVCodecContext,
    encoder_name: &str,
    target_bitrate: Option<i64>,
    is_constant_quality_mode: bool,
    h264_level: Option<H264Level>,
) {
    unsafe {
        debug!(
            "Applying hardware encoder tuning for {} (target_bitrate={:?}, CQ_mode={})",
            encoder_name, target_bitrate, is_constant_quality_mode
        );
        if encoder_name.contains("amf") {
            let derived_bitrate = target_bitrate.unwrap_or(10_000_000).max(1);

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

                let buf_bits = derived_bitrate.saturating_mul(2).max(derived_bitrate);
                set_codec_option_i64(ctx, "maxrate", derived_bitrate);
                set_codec_option_i64(ctx, "bufsize", buf_bits);
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
            // Maxwell-compatible defaults: fast preset, no extra tuning
            set_codec_option_str(ctx, "preset", "p2");
            set_codec_option_i64(ctx, "max_b_frames", 0);
            (*ctx).max_b_frames = 0;
            (*ctx).has_b_frames = 0;

            let level_caps = h264_level.and_then(h264_high_profile_rate_limits);

            if is_constant_quality_mode {
                set_codec_option_str(ctx, "rc", "vbr");
                set_codec_option_i64(ctx, "cq", 21);
                set_codec_option_i64(ctx, "rc-lookahead", 0);
                const DEFAULT_NVENC_VBV: i64 = 10_000_000;
                let mut desired_rate = target_bitrate.unwrap_or(DEFAULT_NVENC_VBV).max(1);
                if let Some(ref caps) = level_caps {
                    if desired_rate > caps.max_bitrate_bits {
                        debug!(
                            "Clamping NVENC CQ VBV {} -> {} to respect H.264 level limit",
                            desired_rate, caps.max_bitrate_bits
                        );
                        desired_rate = caps.max_bitrate_bits.max(1);
                    }
                }
                let mut desired_buffer = desired_rate.saturating_mul(2).max(desired_rate);
                if let Some(ref caps) = level_caps {
                    if desired_buffer > caps.max_buffer_bits {
                        desired_buffer = caps.max_buffer_bits.max(desired_rate);
                    }
                }

                set_codec_option_i64(ctx, "maxrate", desired_rate);
                set_codec_option_i64(ctx, "bufsize", desired_buffer);

                (*ctx).rc_max_rate = desired_rate;
                let buf_i32 = desired_buffer.clamp(1, i32::MAX as i64) as i32;
                (*ctx).rc_buffer_size = buf_i32;
                (*ctx).rc_initial_buffer_occupancy = buf_i32;
                debug!(
                    "Configured NVENC VBV for CQ mode: maxrate={} bufsize={}",
                    desired_rate, desired_buffer
                );
            } else if let Some(bit_rate) = target_bitrate {
                // CBR/Constrained VBR mode for fixed bitrate presets
                set_codec_option_str(ctx, "rc", "cbr");
                const DEFAULT_BUFFER_MULTIPLIER: i64 = 2;
                let mut desired_rate = bit_rate;
                if let Some(ref caps) = level_caps {
                    if desired_rate > caps.max_bitrate_bits {
                        debug!(
                            "Clamping NVENC target bitrate {} -> {} to respect H.264 level limit",
                            desired_rate, caps.max_bitrate_bits
                        );
                        desired_rate = caps.max_bitrate_bits;
                    }
                }

                let nvenc_rate = desired_rate.max(1);
                let mut nvenc_buffer = desired_rate
                    .saturating_mul(DEFAULT_BUFFER_MULTIPLIER)
                    .max(nvenc_rate);

                if let Some(ref caps) = level_caps {
                    if nvenc_buffer > caps.max_buffer_bits {
                        debug!(
                            "Clamping NVENC bufsize {} -> {} to respect H.264 level limit",
                            nvenc_buffer, caps.max_buffer_bits
                        );
                        nvenc_buffer = caps.max_buffer_bits;
                    }
                }

                set_codec_option_i64(ctx, "b", nvenc_rate);
                set_codec_option_i64(ctx, "maxrate", nvenc_rate);
                set_codec_option_i64(ctx, "minrate", bit_rate.max(1));
                set_codec_option_i64(ctx, "bufsize", nvenc_buffer);
                set_codec_option_i64(ctx, "rc-lookahead", 0);

                (*ctx).rc_max_rate = nvenc_rate;
                (*ctx).rc_min_rate = bit_rate.max(1);
                let buf_i32 = nvenc_buffer.clamp(1, i32::MAX as i64) as i32;
                (*ctx).rc_buffer_size = buf_i32;
                (*ctx).rc_initial_buffer_occupancy = buf_i32;
            }
        }
    }
}
