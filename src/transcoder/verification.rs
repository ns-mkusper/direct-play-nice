//! Post-conversion validation helpers.
//!
//! This module verifies the final container after all conversion, OCR, and
//! remuxing steps have completed. It is intentionally separate from the packet
//! loop so validation remains an observable contract rather than another muxing
//! side effect.

use std::ffi::CStr;

use anyhow::{bail, Context, Result};
use log::info;
use rsmpeg::avformat::{AVFormatContextInput, AVStreamRef};
use rsmpeg::ffi;

use crate::transcoder::helpers::describe_codec;
use crate::transcoder::pipeline_assessment::rational_to_f64;

const MIN_SOURCE_FPS_FOR_COLLAPSE_CHECK: f64 = 10.0;
const MIN_OUTPUT_DURATION_RATIO_FOR_COLLAPSE_CHECK: f64 = 0.90;
const MIN_OUTPUT_FPS_RATIO: f64 = 0.80;

/// Reopens the completed output and verifies the minimum stream contract the
/// converter promised: a primary video stream with the requested codec, a
/// compatible audio stream, sane video dimensions, no attachment/data streams in
/// the output, and no severe temporal collapse relative to the source video.
pub(crate) fn validate_output_file(
    input_file: &CStr,
    output_file: &CStr,
    expected_video_codec: ffi::AVCodecID,
    expected_audio_codec: ffi::AVCodecID,
) -> Result<()> {
    let input_ctx = AVFormatContextInput::open(input_file)
        .with_context(|| format!("Failed to reopen input '{}'", input_file.to_string_lossy()))?;
    let output_ctx = AVFormatContextInput::open(output_file).with_context(|| {
        format!(
            "Failed to reopen output '{}'",
            output_file.to_string_lossy()
        )
    })?;
    let mut saw_expected_video = false;
    let mut saw_expected_audio = false;

    for stream in output_ctx.streams() {
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

    validate_video_temporal_consistency(&input_ctx, &output_ctx)?;

    info!(
        "Output validation passed for '{}'.",
        output_file.to_string_lossy()
    );
    Ok(())
}

#[derive(Clone, Copy, Debug, Default)]
struct VideoTemporalInfo {
    duration_seconds: Option<f64>,
    fps: Option<f64>,
}

fn validate_video_temporal_consistency(
    input_ctx: &AVFormatContextInput,
    output_ctx: &AVFormatContextInput,
) -> Result<()> {
    let source = primary_video_temporal_info(input_ctx);
    let output = primary_video_temporal_info(output_ctx);

    if let (Some(source), Some(output)) = (source, output) {
        if let Some(message) = video_temporal_collapse_error(source, output) {
            bail!("Output validation failed: {message}");
        }
    }

    Ok(())
}

fn primary_video_temporal_info(ctx: &AVFormatContextInput) -> Option<VideoTemporalInfo> {
    ctx.streams()
        .iter()
        .find(|stream| stream.codecpar().codec_type == ffi::AVMEDIA_TYPE_VIDEO)
        .map(|stream| video_temporal_info(stream, ctx))
}

fn video_temporal_info(stream: &AVStreamRef, ctx: &AVFormatContextInput) -> VideoTemporalInfo {
    let duration_seconds = stream_duration_seconds(stream).or_else(|| format_duration_seconds(ctx));
    let frame_count = stream_frame_count(stream);
    let fps = stream_fps(stream).or_else(|| fps_from_frame_count(frame_count, duration_seconds));

    VideoTemporalInfo {
        duration_seconds,
        fps,
    }
}

fn stream_duration_seconds(stream: &AVStreamRef) -> Option<f64> {
    let duration = unsafe { (*stream.as_ptr()).duration };
    if duration <= 0 || duration == ffi::AV_NOPTS_VALUE {
        return None;
    }
    rational_to_f64(stream.time_base).map(|time_base| duration as f64 * time_base)
}

fn format_duration_seconds(ctx: &AVFormatContextInput) -> Option<f64> {
    if ctx.duration <= 0 || ctx.duration == ffi::AV_NOPTS_VALUE {
        return None;
    }
    Some(ctx.duration as f64 / ffi::AV_TIME_BASE as f64)
}

fn stream_frame_count(stream: &AVStreamRef) -> Option<i64> {
    let frame_count = unsafe { (*stream.as_ptr()).nb_frames };
    (frame_count > 0).then_some(frame_count)
}

fn stream_fps(stream: &AVStreamRef) -> Option<f64> {
    stream
        .guess_framerate()
        .and_then(rational_to_f64)
        .or_else(|| rational_to_f64(unsafe { (*stream.as_ptr()).avg_frame_rate }))
        .or_else(|| rational_to_f64(unsafe { (*stream.as_ptr()).r_frame_rate }))
}

fn fps_from_frame_count(frame_count: Option<i64>, duration_seconds: Option<f64>) -> Option<f64> {
    let frame_count = frame_count?;
    let duration_seconds = duration_seconds?;
    (frame_count > 0 && duration_seconds > 0.0).then_some(frame_count as f64 / duration_seconds)
}

fn video_temporal_collapse_error(
    source: VideoTemporalInfo,
    output: VideoTemporalInfo,
) -> Option<String> {
    let source_fps = source.fps?;
    let output_fps = output.fps?;
    let source_duration = source.duration_seconds?;
    let output_duration = output.duration_seconds?;

    if source_fps < MIN_SOURCE_FPS_FOR_COLLAPSE_CHECK || source_duration <= 0.0 {
        return None;
    }

    let duration_ratio = output_duration / source_duration;
    let fps_ratio = output_fps / source_fps;
    if duration_ratio >= MIN_OUTPUT_DURATION_RATIO_FOR_COLLAPSE_CHECK
        && fps_ratio < MIN_OUTPUT_FPS_RATIO
    {
        return Some(format!(
            "video frame rate collapsed after conversion (source {:.3} fps over {:.2}s; output {:.3} fps over {:.2}s)",
            source_fps, source_duration, output_fps, output_duration
        ));
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    fn temporal(duration_seconds: Option<f64>, fps: Option<f64>) -> VideoTemporalInfo {
        VideoTemporalInfo {
            duration_seconds,
            fps,
        }
    }

    #[test]
    fn video_temporal_check_rejects_same_duration_frame_rate_collapse() {
        let source = temporal(Some(1405.47), Some(23.976));
        let output = temporal(Some(1405.45), Some(9.67));

        let err = video_temporal_collapse_error(source, output)
            .expect("collapsed output should fail validation");

        assert!(err.contains("video frame rate collapsed"));
    }

    #[test]
    fn video_temporal_check_allows_matching_frame_rate() {
        let source = temporal(Some(1405.47), Some(23.976));
        let output = temporal(Some(1405.45), Some(23.97));

        assert!(video_temporal_collapse_error(source, output).is_none());
    }

    #[test]
    fn video_temporal_check_ignores_short_outputs_for_existing_duration_handling() {
        let source = temporal(Some(1405.47), Some(23.976));
        let output = temporal(Some(100.0), Some(9.67));

        assert!(video_temporal_collapse_error(source, output).is_none());
    }

    #[test]
    fn video_temporal_check_ignores_low_fps_sources() {
        let source = temporal(Some(1405.47), Some(8.0));
        let output = temporal(Some(1405.45), Some(3.0));

        assert!(video_temporal_collapse_error(source, output).is_none());
    }

    #[test]
    fn video_temporal_check_ignores_unknown_values() {
        let source = temporal(Some(1405.47), None);
        let output = temporal(Some(1405.45), Some(9.67));

        assert!(video_temporal_collapse_error(source, output).is_none());
    }

    #[test]
    fn fps_from_frame_count_uses_duration() {
        assert_eq!(fps_from_frame_count(Some(2400), Some(100.0)), Some(24.0));
        assert_eq!(fps_from_frame_count(Some(0), Some(100.0)), None);
        assert_eq!(fps_from_frame_count(Some(2400), Some(0.0)), None);
    }
}
