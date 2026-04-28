//! Stream setup helpers used before the packet-processing loop starts.
//!
//! These routines classify non-playable streams, create decoder contexts, and
//! initialize progress tracking. Keeping them outside the main conversion
//! function makes the high-level pipeline easier to audit.

use anyhow::Result;
use log::{info, warn};
use rsmpeg::avcodec::{AVCodecContext, AVCodecRef};
use rsmpeg::avformat::{AVFormatContextInput, AVStreamRef};
use rsmpeg::ffi;

use crate::ffmpeg_utils::ProgressTracker;
use crate::transcoder::ffmpeg_ext::{codec_name, input_duration_us, stream_disposition};
use crate::transcoder::helpers::enable_strict_decode_failure;

/// Creates progress tracking from container duration while tolerating files
/// where the demuxer cannot determine duration up front.
pub(crate) fn create_progress_tracker(
    input_format_context: &mut AVFormatContextInput,
) -> Option<ProgressTracker> {
    let mut container_duration_us = input_duration_us(input_format_context);
    if container_duration_us <= 0 {
        // Keep progress reporting alive for containers with missing/invalid duration.
        container_duration_us = 1;
    }
    Some(ProgressTracker::new(container_duration_us))
}

/// Filters streams that are metadata or muxer side data rather than playable
/// audio/video/subtitle content for direct-play outputs.
pub(crate) fn should_skip_auxiliary_stream(
    stream: &AVStreamRef<'_>,
    input_codec_type: ffi::AVMediaType,
) -> bool {
    if input_codec_type == ffi::AVMEDIA_TYPE_ATTACHMENT {
        warn!(
            "Skipping attachment stream {} ({}).",
            stream.index,
            codec_name(stream.codecpar().codec_id)
        );
        return true;
    }

    if input_codec_type == ffi::AVMEDIA_TYPE_DATA {
        warn!(
            "Skipping data stream {} ({}).",
            stream.index,
            codec_name(stream.codecpar().codec_id)
        );
        return true;
    }

    if (stream_disposition(stream) & ffi::AV_DISPOSITION_ATTACHED_PIC as i32) != 0 {
        info!(
            "Skipping attached-picture stream {} ({}).",
            stream.index,
            codec_name(stream.codecpar().codec_id)
        );
        return true;
    }

    false
}

/// Builds a decoder context from an input stream and applies the strict decode
/// policy used by the conversion pipeline.
pub(crate) fn new_decode_context(
    decoder: &AVCodecRef<'_>,
    stream: &AVStreamRef<'_>,
) -> Result<AVCodecContext> {
    let mut decode_context = AVCodecContext::new(decoder);
    decode_context.apply_codecpar(&stream.codecpar())?;
    enable_strict_decode_failure(&mut decode_context);
    decode_context.set_time_base(stream.time_base);
    if let Some(framerate) = stream.guess_framerate() {
        decode_context.set_framerate(framerate);
    }
    Ok(decode_context)
}
