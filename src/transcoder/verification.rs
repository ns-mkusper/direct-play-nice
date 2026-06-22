//! Post-conversion validation helpers.
//!
//! This module verifies the final container after all conversion, OCR, and
//! remuxing steps have completed. It is intentionally separate from the packet
//! loop so validation remains an observable contract rather than another muxing
//! side effect.

use std::ffi::CStr;

use anyhow::{anyhow, bail, Context, Result};
use log::{debug, info, warn};
use rsmpeg::avcodec::{AVCodec, AVCodecContext};
use rsmpeg::avformat::{AVFormatContextInput, AVStreamRef};
use rsmpeg::error::RsmpegError;
use rsmpeg::ffi;

use crate::ffmpeg_utils::is_eagain_error;
use crate::transcoder::helpers::{describe_codec, enable_strict_decode_failure};
use crate::transcoder::pipeline_assessment::rational_to_f64;

const MIN_SOURCE_FPS_FOR_COLLAPSE_CHECK: f64 = 10.0;
const MIN_OUTPUT_DURATION_RATIO_FOR_COLLAPSE_CHECK: f64 = 0.90;
const MIN_OUTPUT_FPS_RATIO: f64 = 0.80;
const VISUAL_FRAMES_TO_SCAN: usize = 120;
const VISUAL_SAMPLE_INTERVAL: usize = 15;

#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct OutputValidationOptions {
    pub(crate) visual_validate: bool,
    pub(crate) visual_quality_report: bool,
}

/// Reopens the completed output and verifies the minimum stream contract the
/// converter promised: a primary video stream with the requested codec, a
/// compatible audio stream, sane video dimensions, no attachment/data streams in
/// the output, and no severe temporal collapse relative to the source video.
pub(crate) fn validate_output_file(
    input_file: &CStr,
    output_file: &CStr,
    expected_video_codec: ffi::AVCodecID,
    expected_audio_codec: ffi::AVCodecID,
    options: OutputValidationOptions,
) -> Result<()> {
    let input_ctx = AVFormatContextInput::open(input_file)
        .with_context(|| format!("Failed to reopen input '{}'", input_file.to_string_lossy()))?;
    let mut output_ctx = AVFormatContextInput::open(output_file).with_context(|| {
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

    if options.visual_validate || options.visual_quality_report {
        validate_visual_output(
            &mut output_ctx,
            output_file,
            options.visual_validate,
            options.visual_quality_report,
        )?;
    }

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

#[derive(Clone, Copy, Debug, Default)]
struct FrameVisualStats {
    luma_mean: f64,
    luma_stddev: f64,
    chroma_u_mean: Option<f64>,
    chroma_v_mean: Option<f64>,
}

impl FrameVisualStats {
    fn suspicious_green_screen(self) -> bool {
        let (Some(u), Some(v)) = (self.chroma_u_mean, self.chroma_v_mean) else {
            return false;
        };
        let green_bias = (128.0 - u).max(0.0) + (128.0 - v).max(0.0);
        self.luma_mean > 25.0
            && self.luma_stddev < 55.0
            && u < 100.0
            && v < 115.0
            && green_bias > 45.0
    }

    fn near_solid_nonblack(self) -> bool {
        self.luma_mean > 25.0 && self.luma_mean < 235.0 && self.luma_stddev < 2.5
    }
}

#[derive(Clone, Debug, Default)]
struct VisualInspectionReport {
    decoded_frames: usize,
    inspected_frames: usize,
    suspicious_green_frames: usize,
    near_solid_nonblack_frames: usize,
    luma_mean_sum: f64,
    luma_stddev_sum: f64,
}

impl VisualInspectionReport {
    fn record_decoded_frame(&mut self) {
        self.decoded_frames += 1;
    }

    fn record_stats(&mut self, stats: FrameVisualStats) {
        self.inspected_frames += 1;
        self.luma_mean_sum += stats.luma_mean;
        self.luma_stddev_sum += stats.luma_stddev;
        if stats.suspicious_green_screen() {
            self.suspicious_green_frames += 1;
        }
        if stats.near_solid_nonblack() {
            self.near_solid_nonblack_frames += 1;
        }
    }

    fn average_luma_mean(&self) -> Option<f64> {
        (self.inspected_frames > 0).then_some(self.luma_mean_sum / self.inspected_frames as f64)
    }

    fn average_luma_stddev(&self) -> Option<f64> {
        (self.inspected_frames > 0).then_some(self.luma_stddev_sum / self.inspected_frames as f64)
    }

    fn green_failure_threshold(&self) -> usize {
        ((self.inspected_frames as f64) * 0.60).ceil().max(2.0) as usize
    }

    fn green_failure(&self) -> bool {
        self.inspected_frames >= 3 && self.suspicious_green_frames >= self.green_failure_threshold()
    }
}

fn validate_visual_output(
    output_ctx: &mut AVFormatContextInput,
    output_file: &CStr,
    fail_on_corruption: bool,
    force_report: bool,
) -> Result<()> {
    let (video_stream_index, video_time_base, mut decode_ctx, decoder_name) = {
        let video_stream = output_ctx
            .streams()
            .iter()
            .find(|stream| stream.codecpar().codec_type == ffi::AVMEDIA_TYPE_VIDEO)
            .ok_or_else(|| anyhow!("Output visual validation failed: no video stream"))?;
        let codec_id = video_stream.codecpar().codec_id;
        let decoder = AVCodec::find_decoder(codec_id).ok_or_else(|| {
            anyhow!(
                "Output visual validation failed: decoder unavailable for {}",
                describe_codec(codec_id)
            )
        })?;
        let decoder_name = decoder.name().to_string_lossy().into_owned();
        let mut decode_ctx = AVCodecContext::new(&decoder);
        decode_ctx
            .apply_codecpar(&video_stream.codecpar())
            .context("Output visual validation failed: applying video codec parameters")?;
        decode_ctx.set_time_base(video_stream.time_base);
        enable_strict_decode_failure(&mut decode_ctx);
        decode_ctx.open(None).with_context(|| {
            format!("Output visual validation failed: opening decoder '{decoder_name}'")
        })?;
        (
            video_stream.index,
            video_stream.time_base,
            decode_ctx,
            decoder_name,
        )
    };

    let mut report = VisualInspectionReport::default();
    let mut video_packets_seen = 0usize;

    while report.decoded_frames < VISUAL_FRAMES_TO_SCAN {
        let Some(mut packet) = output_ctx.read_packet()? else {
            break;
        };
        if packet.stream_index != video_stream_index {
            continue;
        }
        video_packets_seen += 1;
        packet.rescale_ts(video_time_base, decode_ctx.time_base);
        match decode_ctx.send_packet(Some(&packet)) {
            Ok(_) | Err(RsmpegError::DecoderFlushedError) => {}
            Err(err) if is_eagain_error(&err) => {}
            Err(err) => {
                bail!("Output visual validation failed: decoder send_packet failed: {err}");
            }
        }
        drain_visual_frames(&mut decode_ctx, &mut report)?;
    }

    match decode_ctx.send_packet(None) {
        Ok(_) | Err(RsmpegError::DecoderFlushedError) => {}
        Err(err) if is_eagain_error(&err) => {}
        Err(err) => bail!("Output visual validation failed: decoder flush failed: {err}"),
    }
    drain_visual_frames(&mut decode_ctx, &mut report)?;

    if report.decoded_frames == 0 && video_packets_seen > 0 {
        bail!("Output visual validation failed: no video frames decoded from output");
    }

    if fail_on_corruption && report.green_failure() {
        bail!(
            "Output visual validation failed: {} of {} sampled frames look like green-screen corruption in '{}'",
            report.suspicious_green_frames,
            report.inspected_frames,
            output_file.to_string_lossy()
        );
    }

    if force_report || report.suspicious_green_frames > 0 || report.near_solid_nonblack_frames > 0 {
        info!(
            "Output visual quality report for '{}': decoder={}, decoded_frames={}, inspected_frames={}, suspicious_green_frames={}, near_solid_nonblack_frames={}, avg_luma_mean={}, avg_luma_stddev={}",
            output_file.to_string_lossy(),
            decoder_name,
            report.decoded_frames,
            report.inspected_frames,
            report.suspicious_green_frames,
            report.near_solid_nonblack_frames,
            format_optional_f64(report.average_luma_mean()),
            format_optional_f64(report.average_luma_stddev())
        );
    } else {
        debug!(
            "Output visual validation passed for '{}' (decoder={}, decoded_frames={}, inspected_frames={}).",
            output_file.to_string_lossy(),
            decoder_name,
            report.decoded_frames,
            report.inspected_frames
        );
    }

    if report.inspected_frames == 0 && report.decoded_frames > 0 {
        warn!(
            "Output visual validation decoded {} frame(s) from '{}' but could not inspect pixel statistics for the decoded format.",
            report.decoded_frames,
            output_file.to_string_lossy()
        );
    }

    Ok(())
}

fn drain_visual_frames(
    decode_ctx: &mut AVCodecContext,
    report: &mut VisualInspectionReport,
) -> Result<()> {
    loop {
        match decode_ctx.receive_frame() {
            Ok(frame) => {
                let frame_index = report.decoded_frames;
                report.record_decoded_frame();
                if frame_index.is_multiple_of(VISUAL_SAMPLE_INTERVAL) {
                    if let Some(stats) = frame_visual_stats(&frame) {
                        report.record_stats(stats);
                    }
                }
            }
            Err(RsmpegError::DecoderDrainError | RsmpegError::DecoderFlushedError) => break,
            Err(err) if is_eagain_error(&err) => break,
            Err(err) => {
                bail!("Output visual validation failed: decoder receive_frame failed: {err}")
            }
        }
    }
    Ok(())
}

fn format_optional_f64(value: Option<f64>) -> String {
    value
        .map(|value| format!("{value:.2}"))
        .unwrap_or_else(|| "n/a".to_string())
}

fn frame_visual_stats(frame: &rsmpeg::avutil::AVFrame) -> Option<FrameVisualStats> {
    let width = frame.width.max(0) as usize;
    let height = frame.height.max(0) as usize;
    if width == 0 || height == 0 {
        return None;
    }

    unsafe {
        let raw = frame.as_ptr();
        let data = (*raw).data;
        let linesize = (*raw).linesize;
        let luma = sample_plane(data[0], linesize[0], width, height)?;
        let pix_fmt = frame.format as ffi::AVPixelFormat;
        let (chroma_u_mean, chroma_v_mean) = match pix_fmt {
            ffi::AV_PIX_FMT_YUV420P | ffi::AV_PIX_FMT_YUVJ420P => {
                sample_planar_chroma(data, linesize, width.div_ceil(2), height.div_ceil(2))
            }
            ffi::AV_PIX_FMT_YUV422P | ffi::AV_PIX_FMT_YUVJ422P => {
                sample_planar_chroma(data, linesize, width.div_ceil(2), height)
            }
            ffi::AV_PIX_FMT_YUV444P | ffi::AV_PIX_FMT_YUVJ444P => {
                sample_planar_chroma(data, linesize, width, height)
            }
            ffi::AV_PIX_FMT_NV12 => sample_interleaved_chroma(
                data[1],
                linesize[1],
                width.div_ceil(2),
                height.div_ceil(2),
                true,
            ),
            ffi::AV_PIX_FMT_NV21 => sample_interleaved_chroma(
                data[1],
                linesize[1],
                width.div_ceil(2),
                height.div_ceil(2),
                false,
            ),
            _ => (None, None),
        };

        Some(FrameVisualStats {
            luma_mean: luma.mean,
            luma_stddev: luma.stddev,
            chroma_u_mean,
            chroma_v_mean,
        })
    }
}

#[derive(Clone, Copy, Debug)]
struct PlaneStats {
    mean: f64,
    stddev: f64,
}

unsafe fn sample_planar_chroma(
    data: [*mut u8; 8],
    linesize: [i32; 8],
    width: usize,
    height: usize,
) -> (Option<f64>, Option<f64>) {
    let u = sample_plane(data[1], linesize[1], width, height).map(|stats| stats.mean);
    let v = sample_plane(data[2], linesize[2], width, height).map(|stats| stats.mean);
    (u, v)
}

unsafe fn sample_interleaved_chroma(
    ptr: *const u8,
    linesize: i32,
    width: usize,
    height: usize,
    uv_order: bool,
) -> (Option<f64>, Option<f64>) {
    if ptr.is_null() || linesize <= 0 || width == 0 || height == 0 {
        return (None, None);
    }
    let step = sample_step(width, height);
    let mut u_sum = 0f64;
    let mut v_sum = 0f64;
    let mut count = 0usize;
    for y in (0..height).step_by(step) {
        let row = ptr.add(y * linesize as usize);
        for x in (0..width).step_by(step) {
            let pair = row.add(x * 2);
            let first = *pair as f64;
            let second = *pair.add(1) as f64;
            if uv_order {
                u_sum += first;
                v_sum += second;
            } else {
                v_sum += first;
                u_sum += second;
            }
            count += 1;
        }
    }
    if count == 0 {
        return (None, None);
    }
    (Some(u_sum / count as f64), Some(v_sum / count as f64))
}

unsafe fn sample_plane(
    ptr: *const u8,
    linesize: i32,
    width: usize,
    height: usize,
) -> Option<PlaneStats> {
    if ptr.is_null() || linesize <= 0 || width == 0 || height == 0 {
        return None;
    }
    let step = sample_step(width, height);
    let mut sum = 0f64;
    let mut sum_sq = 0f64;
    let mut count = 0usize;
    for y in (0..height).step_by(step) {
        let row = ptr.add(y * linesize as usize);
        for x in (0..width).step_by(step) {
            let value = *row.add(x) as f64;
            sum += value;
            sum_sq += value * value;
            count += 1;
        }
    }
    if count == 0 {
        return None;
    }
    let mean = sum / count as f64;
    let variance = (sum_sq / count as f64 - mean * mean).max(0.0);
    Some(PlaneStats {
        mean,
        stddev: variance.sqrt(),
    })
}

fn sample_step(width: usize, height: usize) -> usize {
    (((width.saturating_mul(height)) as f64 / 4096.0)
        .sqrt()
        .ceil() as usize)
        .max(1)
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
    fn visual_stats_flags_green_screen_like_frames() {
        let stats = FrameVisualStats {
            luma_mean: 145.0,
            luma_stddev: 4.0,
            chroma_u_mean: Some(54.0),
            chroma_v_mean: Some(34.0),
        };

        assert!(stats.suspicious_green_screen());
    }

    #[test]
    fn visual_stats_do_not_flag_varied_natural_frames_as_green_screen() {
        let stats = FrameVisualStats {
            luma_mean: 110.0,
            luma_stddev: 70.0,
            chroma_u_mean: Some(80.0),
            chroma_v_mean: Some(90.0),
        };

        assert!(!stats.suspicious_green_screen());
    }

    #[test]
    fn visual_report_requires_majority_green_samples_before_failing() {
        let mut report = VisualInspectionReport::default();
        let green = FrameVisualStats {
            luma_mean: 145.0,
            luma_stddev: 4.0,
            chroma_u_mean: Some(54.0),
            chroma_v_mean: Some(34.0),
        };
        let normal = FrameVisualStats {
            luma_mean: 110.0,
            luma_stddev: 40.0,
            chroma_u_mean: Some(128.0),
            chroma_v_mean: Some(128.0),
        };

        report.record_stats(green);
        report.record_stats(green);
        report.record_stats(normal);

        assert!(report.green_failure());

        let mut mostly_normal = VisualInspectionReport::default();
        mostly_normal.record_stats(green);
        mostly_normal.record_stats(normal);
        mostly_normal.record_stats(normal);

        assert!(!mostly_normal.green_failure());
    }

    #[test]
    fn fps_from_frame_count_uses_duration() {
        assert_eq!(fps_from_frame_count(Some(2400), Some(100.0)), Some(24.0));
        assert_eq!(fps_from_frame_count(Some(0), Some(100.0)), None);
        assert_eq!(fps_from_frame_count(Some(2400), Some(0.0)), None);
    }
}
