//! Small safe wrappers around raw FFmpeg fields used by the transcoder.
//!
//! rsmpeg exposes most FFmpeg structures safely, but a few metadata and
//! ownership operations still require direct pointer access. Keeping those
//! reads/writes here makes the pipeline modules easier to audit and gives each
//! unsafe operation a single documented purpose.

use std::ffi::CStr;

use rsmpeg::avformat::{AVFormatContextInput, AVStreamRef};
use rsmpeg::ffi;

use crate::transcoder::ffmpeg_diagnostics::av_error_to_string;

/// Returns FFmpeg's canonical codec name for user-facing logs and diagnostics.
pub(crate) fn codec_name(codec_id: ffi::AVCodecID) -> String {
    unsafe {
        CStr::from_ptr(ffi::avcodec_get_name(codec_id))
            .to_string_lossy()
            .into_owned()
    }
}

/// Reads stream disposition flags such as attached-picture/default/forced.
pub(crate) fn stream_disposition(stream: &AVStreamRef<'_>) -> i32 {
    unsafe { (*stream.as_ptr()).disposition }
}

/// Returns the raw codec-parameter bitrate for demuxers that do not populate
/// rsmpeg's copied `codecpar().bit_rate` field.
pub(crate) fn stream_raw_bit_rate(stream: &AVStreamRef<'_>) -> i64 {
    unsafe { (*(*stream.as_ptr()).codecpar).bit_rate }
}

/// Reads the container duration from the input format context in microseconds.
pub(crate) fn input_duration_us(input: &mut AVFormatContextInput) -> i64 {
    unsafe { (*input.as_mut_ptr()).duration }
}

/// Returns FFmpeg's detected input format short-name list, for example
/// `mov,mp4,m4a,3gp,3g2,mj2` or `matroska,webm`.
pub(crate) fn input_format_name(input: &AVFormatContextInput) -> String {
    input.iformat().name().to_string_lossy().into_owned()
}

/// Clears hardware decoder hooks after a failed hardware setup attempt so the
/// same codec context can be safely discarded or rebuilt for software decode.
pub(crate) fn clear_decoder_hardware_state(ctx: &mut rsmpeg::avcodec::AVCodecContext) {
    unsafe {
        let ctx_ptr = ctx.as_mut_ptr();
        (*ctx_ptr).hw_device_ctx = std::ptr::null_mut();
        (*ctx_ptr).hw_frames_ctx = std::ptr::null_mut();
        (*ctx_ptr).get_format = None;
    }
}

/// Releases an owned FFmpeg buffer reference if present.
pub(crate) fn unref_buffer_ref(mut buffer: *mut ffi::AVBufferRef) {
    unsafe {
        ffi::av_buffer_unref(&mut buffer);
    }
}

/// Allocates an FFmpeg packet large enough to hold `payload` and copies the
/// payload into it.
pub(crate) fn copy_payload_into_packet(
    packet: &mut rsmpeg::avcodec::AVPacket,
    payload: &[u8],
) -> anyhow::Result<()> {
    unsafe {
        let ret = ffi::av_new_packet(packet.as_mut_ptr(), payload.len() as i32);
        if ret < 0 {
            anyhow::bail!("Could not allocate packet: {}", av_error_to_string(ret));
        }
        std::ptr::copy_nonoverlapping(payload.as_ptr(), (*packet.as_mut_ptr()).data, payload.len());
    }
    Ok(())
}
