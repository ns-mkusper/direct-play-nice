//! Post-conversion validation helpers.
//!
//! This module verifies the final container after all conversion, OCR, and
//! remuxing steps have completed. It is intentionally separate from the packet
//! loop so validation remains an observable contract rather than another muxing
//! side effect.

use std::ffi::CStr;

use anyhow::{bail, Context, Result};
use log::info;
use rsmpeg::avformat::AVFormatContextInput;
use rsmpeg::ffi;

use crate::transcoder::helpers::describe_codec;

/// Reopens the completed output and verifies the minimum stream contract the
/// converter promised: a primary video stream with the requested codec, a
/// compatible audio stream, sane video dimensions, and no attachment/data
/// streams in the output.
pub(crate) fn validate_output_file(
    output_file: &CStr,
    expected_video_codec: ffi::AVCodecID,
    expected_audio_codec: ffi::AVCodecID,
) -> Result<()> {
    let ctx = AVFormatContextInput::open(output_file).with_context(|| {
        format!(
            "Failed to reopen output '{}'",
            output_file.to_string_lossy()
        )
    })?;
    let mut saw_expected_video = false;
    let mut saw_expected_audio = false;

    for stream in ctx.streams() {
        let codecpar = stream.codecpar();
        match codecpar.codec_type {
            ffi::AVMEDIA_TYPE_VIDEO => {
                if codecpar.width <= 0 || codecpar.height <= 0 {
                    bail!(
                        "Output validation failed: video stream {} has invalid dimensions {}x{}",
                        stream.index,
                        codecpar.width,
                        codecpar.height
                    );
                }
                if codecpar.codec_id == expected_video_codec {
                    saw_expected_video = true;
                }
            }
            ffi::AVMEDIA_TYPE_AUDIO if codecpar.codec_id == expected_audio_codec => {
                saw_expected_audio = true;
            }
            ffi::AVMEDIA_TYPE_AUDIO => {}
            ffi::AVMEDIA_TYPE_ATTACHMENT | ffi::AVMEDIA_TYPE_DATA => {
                bail!(
                    "Output validation failed: unsupported auxiliary stream {} remains in '{}'",
                    stream.index,
                    output_file.to_string_lossy()
                );
            }
            _ => {}
        }
    }

    if !saw_expected_video {
        bail!(
            "Output validation failed: no video stream with expected codec {}",
            describe_codec(expected_video_codec)
        );
    }
    if !saw_expected_audio {
        bail!(
            "Output validation failed: no audio stream with expected codec {}",
            describe_codec(expected_audio_codec)
        );
    }

    info!(
        "Output validation passed for '{}'.",
        output_file.to_string_lossy()
    );
    Ok(())
}
