//! Common transcoder helpers for packet/frame timing and IO setup.

use crate::transcoder::prelude::*;

pub(crate) fn describe_bitrate(bitrate: Option<i64>) -> String {
    match bitrate {
        Some(bps) => format!("{} bps", bps),
        None => "match source".to_string(),
    }
}

pub(crate) fn describe_resolution(dimensions: Option<(u32, u32)>) -> String {
    match dimensions {
        Some((w, h)) => format!("max {}x{}", w, h),
        None => "match source".to_string(),
    }
}

pub(crate) fn describe_codec(codec_id: ffi::AVCodecID) -> &'static str {
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

pub(crate) fn ensure_decoder_pkt_time_base(ctx: &mut AVCodecContext, time_base: ffi::AVRational) {
    unsafe {
        let current = (*ctx.as_ptr()).pkt_timebase;
        if current.num <= 0 || current.den <= 0 {
            (*ctx.as_mut_ptr()).pkt_timebase = time_base;
        }
    }
}

pub(crate) fn enable_strict_decode_failure(ctx: &mut AVCodecContext) {
    // Fail fast on broken bitstreams instead of writing partially decoded output.
    unsafe {
        (*ctx.as_mut_ptr()).err_recognition |= ffi::AV_EF_EXPLODE as i32;
    }
}

const AV1_HW_DECODER_NAMES: &[&str] = &["av1_cuvid", "av1_nvdec"];
const AV1_SW_DECODER_NAMES: &[&str] = &["libdav1d", "libaom-av1", "av1"];
const H264_HW_DECODER_NAMES: &[&str] = &["h264_cuvid"];
const H264_SW_DECODER_NAMES: &[&str] = &["h264"];
const HEVC_HW_DECODER_NAMES: &[&str] = &["hevc_cuvid"];
const HEVC_SW_DECODER_NAMES: &[&str] = &["hevc"];

fn preferred_decoder_names(codec_id: ffi::AVCodecID, prefer_hw: bool) -> &'static [&'static str] {
    match codec_id {
        ffi::AV_CODEC_ID_AV1 => {
            if prefer_hw {
                AV1_HW_DECODER_NAMES
            } else {
                AV1_SW_DECODER_NAMES
            }
        }
        ffi::AV_CODEC_ID_H264 => {
            if prefer_hw {
                H264_HW_DECODER_NAMES
            } else {
                H264_SW_DECODER_NAMES
            }
        }
        ffi::AV_CODEC_ID_HEVC => {
            if prefer_hw {
                HEVC_HW_DECODER_NAMES
            } else {
                HEVC_SW_DECODER_NAMES
            }
        }
        _ => &[],
    }
}

pub(crate) fn find_decoder_with_fallback(
    codec_id: ffi::AVCodecID,
    prefer_hw: bool,
) -> Option<AVCodecRef<'static>> {
    let passes: &[bool] = if prefer_hw { &[true, false] } else { &[false] };

    for pass in passes {
        for &name in preferred_decoder_names(codec_id, *pass) {
            match CString::new(name) {
                Ok(cname) => {
                    if let Some(decoder) = AVCodec::find_decoder_by_name(cname.as_c_str()) {
                        debug!(
                            "Using preferred decoder '{}' for codec {}",
                            name,
                            describe_codec(codec_id)
                        );
                        return Some(decoder);
                    } else {
                        trace!(
                            "Preferred decoder '{}' for codec {} not available; continuing search",
                            name,
                            describe_codec(codec_id)
                        );
                    }
                }
                Err(_) => {
                    trace!("Decoder name '{}' contained interior NUL; skipping", name);
                }
            }
        }
    }

    AVCodec::find_decoder(codec_id)
}

unsafe extern "C" fn select_cuda_hw_format(
    _ctx: *mut ffi::AVCodecContext,
    pix_fmts: *const ffi::AVPixelFormat,
) -> ffi::AVPixelFormat {
    if pix_fmts.is_null() {
        return ffi::AV_PIX_FMT_NONE;
    }
    let mut ptr = pix_fmts;
    while (*ptr) != ffi::AV_PIX_FMT_NONE {
        if (*ptr) == ffi::AV_PIX_FMT_CUDA {
            return *ptr;
        }
        ptr = ptr.add(1);
    }
    *pix_fmts
}

pub(crate) fn configure_cuda_hw_decoder(
    decode_context: &mut AVCodecContext,
    device: *mut ffi::AVBufferRef,
) -> Result<()> {
    unsafe {
        if device.is_null() {
            bail!("No CUDA hw device available for hardware decode");
        }

        let ctx_ptr = decode_context.as_mut_ptr();
        (*ctx_ptr).get_format = Some(select_cuda_hw_format);
        let device_ref = ffi::av_buffer_ref(device);
        if device_ref.is_null() {
            bail!("Failed to acquire reference to CUDA device context");
        }
        (*ctx_ptr).hw_device_ctx = device_ref;

        (*ctx_ptr).hw_frames_ctx = ptr::null_mut();
    }

    Ok(())
}

pub(crate) fn devices_support_codec(devices: &[&StreamingDevice], codec: ffi::AVCodecID) -> bool {
    devices
        .iter()
        .all(|device| device.video_codecs.contains(&codec))
}

pub(crate) fn describe_h264_profile(profile: i32) -> String {
    H264Profile::try_from(profile)
        .map(|p| format!("{:?}", p))
        .unwrap_or_else(|_| format!("profile({})", profile))
}

pub(crate) fn describe_h264_level(level: i32) -> String {
    H264Level::try_from(level)
        .map(|l| l.ffmpeg_name().to_string())
        .unwrap_or_else(|_| format!("level({})", level))
}
