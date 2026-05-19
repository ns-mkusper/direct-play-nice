//! Stream-processing routines that execute per-stream decode/encode flows for video, audio, and subtitle data.

use anyhow::{anyhow, bail, Context, Result};
use libc::EINVAL;
use log::{debug, error, trace, warn};
use rsmpeg::avcodec::{AVCodecContext, AVPacket};
use rsmpeg::avformat::{AVFormatContextOutput, AVStreamRef};
use rsmpeg::avutil::{AVAudioFifo, AVFrame, AVSamples};
use rsmpeg::error::RsmpegError;
use rsmpeg::ffi;
use rsmpeg::swscale::SwsContext;
use std::sync::atomic::{AtomicI64, Ordering};

use crate::ffmpeg_utils::{
    add_samples_to_fifo, encode_and_write_frame, init_output_audio_frame, is_eagain_error,
    ProgressTracker, StreamProcessingContext,
};
use crate::transcoder::ffmpeg_diagnostics::{av_error_to_string, pix_fmt_name};
use crate::transcoder::ffmpeg_ext::copy_payload_into_packet;
use crate::transcoder::h264::DecoderError;
use crate::transcoder::pipeline_codec::preferred_audio_frame_size;
use crate::transcoder::timestamp::{
    best_effort_frame_pts, enforce_monotonic_dts, rescale_timestamp, subtitle_dts, subtitle_pts,
};
use crate::types::{ResizeQuality, SubtitleFailurePolicy};

#[cfg(windows)]
fn sws_flags_for_resize_quality(quality: ResizeQuality) -> i32 {
    let kernel = match quality {
        ResizeQuality::FastBilinear => ffi::SWS_FAST_BILINEAR,
        ResizeQuality::Bilinear => ffi::SWS_BILINEAR,
        ResizeQuality::Bicubic => ffi::SWS_BICUBIC,
        ResizeQuality::Lanczos => ffi::SWS_LANCZOS,
        ResizeQuality::Spline => ffi::SWS_SPLINE,
    };
    kernel | ffi::SWS_ACCURATE_RND
}

#[cfg(not(windows))]
fn sws_flags_for_resize_quality(quality: ResizeQuality) -> u32 {
    let kernel = match quality {
        ResizeQuality::FastBilinear => ffi::SWS_FAST_BILINEAR,
        ResizeQuality::Bilinear => ffi::SWS_BILINEAR,
        ResizeQuality::Bicubic => ffi::SWS_BICUBIC,
        ResizeQuality::Lanczos => ffi::SWS_LANCZOS,
        ResizeQuality::Spline => ffi::SWS_SPLINE,
    };
    kernel | ffi::SWS_ACCURATE_RND
}

fn ensure_software_frame(frame: AVFrame) -> Result<AVFrame> {
    if frame.format == ffi::AV_PIX_FMT_CUDA {
        let transfer_format = unsafe {
            let hw_frames_ctx = (*frame.as_ptr()).hw_frames_ctx;
            if hw_frames_ctx.is_null() {
                warn!("CUDA frame is missing hw_frames_ctx; falling back to NV12 transfer format.");
                ffi::AV_PIX_FMT_NV12
            } else {
                let frames_ctx_ptr = (*hw_frames_ctx).data as *const ffi::AVHWFramesContext;
                if frames_ctx_ptr.is_null() {
                    warn!("CUDA frame hw_frames_ctx has null data; falling back to NV12.");
                    ffi::AV_PIX_FMT_NV12
                } else {
                    let sw_format = (*frames_ctx_ptr).sw_format;
                    if sw_format == ffi::AV_PIX_FMT_NONE {
                        warn!(
                            "CUDA frame reports AV_PIX_FMT_NONE sw_format; falling back to NV12."
                        );
                        ffi::AV_PIX_FMT_NV12
                    } else {
                        sw_format
                    }
                }
            }
        };

        let mut sw_frame = AVFrame::new();
        sw_frame.set_format(transfer_format);
        sw_frame.set_width(frame.width);
        sw_frame.set_height(frame.height);
        sw_frame.set_pts(frame.pts);
        sw_frame.set_time_base(frame.time_base);
        unsafe {
            let ret = ffi::av_hwframe_transfer_data(sw_frame.as_mut_ptr(), frame.as_ptr(), 0);
            if ret < 0 {
                bail!(
                    "Failed to transfer CUDA frame to system memory: {}",
                    av_error_to_string(ret)
                );
            }
            (*sw_frame.as_mut_ptr()).best_effort_timestamp =
                (*frame.as_ptr()).best_effort_timestamp;
        }
        debug!(
            "Transferred CUDA frame {}x{} from {} to software format {}",
            frame.width,
            frame.height,
            pix_fmt_name(frame.format as ffi::AVPixelFormat),
            pix_fmt_name(transfer_format)
        );
        Ok(sw_frame)
    } else {
        Ok(frame)
    }
}

pub(crate) fn process_video_stream(
    stream_processing_context: &mut StreamProcessingContext,
    input_stream: &AVStreamRef,
    output_format_context: &mut AVFormatContextOutput,
    packet: &mut AVPacket,
    mut progress: Option<&mut ProgressTracker>,
) -> Result<()> {
    packet.rescale_ts(
        input_stream.time_base,
        stream_processing_context.decode_context.time_base,
    );

    match stream_processing_context
        .decode_context
        .send_packet(Some(packet))
    {
        Ok(_) | Err(RsmpegError::DecoderFlushedError) => {}
        Err(e) if is_eagain_error(&e) => return Ok(()),
        Err(e) => {
            error!(
                "Video decoder send_packet failure on stream {} (decoder='{}', pts={}, dts={}, duration={}, tb={}/{}): {}",
                stream_processing_context.input_stream_index,
                stream_processing_context.decoder_name,
                packet.pts,
                packet.dts,
                packet.duration,
                stream_processing_context.decode_context.time_base.num,
                stream_processing_context.decode_context.time_base.den,
                e
            );
            return Err(anyhow!(DecoderError::new(
                stream_processing_context.decoder_name.clone(),
                stream_processing_context.input_stream_index.as_i32(),
                e.to_string(),
            )));
        }
    }

    let mut warned_drain = false;

    loop {
        let frame = match stream_processing_context.decode_context.receive_frame() {
            Ok(frame) => frame,
            Err(RsmpegError::DecoderDrainError) | Err(RsmpegError::DecoderFlushedError) => {
                if !std::mem::replace(&mut warned_drain, true) {
                    debug!(
                        "Video decoder drained/flushed for stream {}",
                        stream_processing_context.input_stream_index
                    );
                }
                break;
            }
            Err(e) if is_eagain_error(&e) => {
                debug!(
                    "Video decoder returned EAGAIN for stream {}",
                    stream_processing_context.input_stream_index
                );
                break;
            }
            Err(e) => {
                error!(
                    "Video decoder receive_frame failure on stream {} (decoder='{}', last-packet pts={}, dts={}, duration={}, tb={}/{}): {}",
                    stream_processing_context.input_stream_index,
                    stream_processing_context.decoder_name,
                    packet.pts,
                    packet.dts,
                    packet.duration,
                    stream_processing_context.decode_context.time_base.num,
                    stream_processing_context.decode_context.time_base.den,
                    e
                );
                return Err(anyhow!(DecoderError::new(
                    stream_processing_context.decoder_name.clone(),
                    stream_processing_context.input_stream_index.as_i32(),
                    format!("receive_frame failed: {}", e),
                )));
            }
        };

        let frame = ensure_software_frame(frame)?;

        if let Some(progress) = progress.as_deref_mut() {
            progress.report(
                frame.best_effort_timestamp,
                stream_processing_context.decode_context.time_base,
            );
        }

        let mut new_frame = AVFrame::new();
        new_frame.set_width(stream_processing_context.encode_context.width);
        new_frame.set_height(stream_processing_context.encode_context.height);
        new_frame.set_format(ffi::AV_PIX_FMT_YUV420P);
        new_frame.alloc_buffer().context("Error allocating ")?;

        let source_width = frame.width;
        let source_height = frame.height;
        let source_pix_fmt = frame.format as ffi::AVPixelFormat;
        let mut sws_context = SwsContext::get_context(
            source_width,
            source_height,
            source_pix_fmt,
            stream_processing_context.encode_context.width,
            stream_processing_context.encode_context.height,
            stream_processing_context.encode_context.pix_fmt,
            sws_flags_for_resize_quality(stream_processing_context.resize_quality),
            None,
            None,
            None,
        )
        .context("Failed to create a swscale context.")?;

        // Ensure the encoder sees timestamps in its own time base to avoid inflated durations.
        let rescaled_pts = rescale_timestamp(
            best_effort_frame_pts(&frame),
            stream_processing_context.decode_context.time_base,
            stream_processing_context.encode_context.time_base,
        );

        new_frame.set_time_base(stream_processing_context.encode_context.time_base);
        if rescaled_pts != ffi::AV_NOPTS_VALUE {
            new_frame.set_pts(rescaled_pts);
        }

        sws_context
            .scale_frame(&frame, 0, source_height, &mut new_frame)
            .context("Failed to scale frame.")?;

        encode_and_write_frame(
            &mut stream_processing_context.encode_context,
            output_format_context,
            stream_processing_context.output_stream_index.as_usize(),
            Some(new_frame),
        )?;
    }

    Ok(())
}

pub(crate) fn process_audio_stream(
    stream_processing_context: &mut StreamProcessingContext,
    input_stream: &AVStreamRef,
    output_format_context: &mut AVFormatContextOutput,
    packet: &mut AVPacket,
) -> Result<()> {
    packet.rescale_ts(
        input_stream.time_base,
        stream_processing_context.decode_context.time_base,
    );

    let Some(fifo) = stream_processing_context.frame_buffer.as_mut() else {
        bail!(
            "Missing audio FIFO buffer for stream {}",
            stream_processing_context.input_stream_index
        );
    };

    match stream_processing_context
        .decode_context
        .send_packet(Some(packet))
    {
        Ok(_) | Err(RsmpegError::DecoderFlushedError) => {}
        Err(e) if is_eagain_error(&e) => return Ok(()),
        Err(e) => {
            error!(
                "Audio decoder send_packet failure on stream {} (decoder='{}', pts={}, dts={}, duration={}, tb={}/{}): {}",
                stream_processing_context.input_stream_index,
                stream_processing_context.decoder_name,
                packet.pts,
                packet.dts,
                packet.duration,
                stream_processing_context.decode_context.time_base.num,
                stream_processing_context.decode_context.time_base.den,
                e
            );
            return Err(anyhow!(DecoderError::new(
                stream_processing_context.decoder_name.clone(),
                stream_processing_context.input_stream_index.as_i32(),
                e.to_string(),
            )));
        }
    }

    let mut warned_drain = false;

    loop {
        let frame = match stream_processing_context.decode_context.receive_frame() {
            Ok(frame) => frame,
            Err(RsmpegError::DecoderDrainError) | Err(RsmpegError::DecoderFlushedError) => {
                if !std::mem::replace(&mut warned_drain, true) {
                    debug!(
                        "Audio decoder drained/flushed for stream {}",
                        stream_processing_context.input_stream_index
                    );
                }
                break;
            }
            Err(e) if is_eagain_error(&e) => {
                debug!(
                    "Audio decoder returned EAGAIN for stream {}",
                    stream_processing_context.input_stream_index
                );
                break;
            }
            Err(e) => {
                error!(
                    "Audio decoder receive_frame failure on stream {} (decoder='{}', last-packet pts={}, dts={}, duration={}, tb={}/{}): {}",
                    stream_processing_context.input_stream_index,
                    stream_processing_context.decoder_name,
                    packet.pts,
                    packet.dts,
                    packet.duration,
                    stream_processing_context.decode_context.time_base.num,
                    stream_processing_context.decode_context.time_base.den,
                    e
                );
                return Err(anyhow!(DecoderError::new(
                    stream_processing_context.decoder_name.clone(),
                    stream_processing_context.input_stream_index.as_i32(),
                    format!("receive_frame failed: {}", e),
                )));
            }
        };

        let output_frame_size =
            preferred_audio_frame_size(&stream_processing_context.encode_context, fifo.size());

        debug!("OUTPUT FRAME SIZE: {}", output_frame_size);

        let mut output_samples = AVSamples::new(
            stream_processing_context
                .encode_context
                .ch_layout
                .nb_channels,
            frame.nb_samples,
            stream_processing_context.encode_context.sample_fmt,
            0,
        )
        .context("Create samples buffer failed.")?;

        if let Some(resampler) = &mut stream_processing_context.resample_context {
            unsafe {
                resampler
                    .convert(
                        output_samples.audio_data.as_mut_ptr(),
                        output_samples.nb_samples,
                        frame.extended_data as *const _,
                        frame.nb_samples,
                    )
                    .context("Could not convert input samples")?;
            }
        }

        add_samples_to_fifo(fifo, &output_samples, frame.nb_samples)?;

        debug!("FIFO SIZE: {}", fifo.size());
        debug!(
            "AUDIO STREAM INDEX: {}",
            stream_processing_context.output_stream_index
        );
        while fifo.size() >= output_frame_size {
            load_encode_and_write(
                fifo,
                output_format_context,
                &mut stream_processing_context.encode_context,
                stream_processing_context.output_stream_index.as_i32(),
                &mut stream_processing_context.pts,
            )?;
        }
    }

    Ok(())
}

pub(crate) fn process_subtitle_stream(
    stream_processing_context: &mut StreamProcessingContext,
    input_stream: &AVStreamRef,
    output_format_context: &mut AVFormatContextOutput,
    packet: &mut AVPacket,
) -> Result<()> {
    if stream_processing_context.skip_stream {
        trace!(
            "Subtitle stream {} already skipped; dropping packet.",
            stream_processing_context.input_stream_index
        );
        return Ok(());
    }

    packet.rescale_ts(
        input_stream.time_base,
        stream_processing_context.decode_context.time_base,
    );

    match stream_processing_context
        .decode_context
        .decode_subtitle(Some(packet))
    {
        Ok(sub) => {
            if let Some(subtitle) = sub {
                debug!(
                    "Subtitle stream {} raw packet pts={} dts={} duration={}",
                    stream_processing_context.input_stream_index,
                    packet.pts,
                    packet.dts,
                    packet.duration
                );
                let Some(encoded_subtitle) = (match encode_subtitle_to_vec(
                    &mut stream_processing_context.encode_context,
                    &subtitle,
                    stream_processing_context.input_stream_index.as_i32(),
                ) {
                    Ok(encoded) => encoded,
                    Err(err) => {
                        return handle_subtitle_stream_failure(
                            stream_processing_context,
                            format!("failed to encode ({err})"),
                        );
                    }
                }) else {
                    return Ok(());
                };

                // Create a new packet for the encoded subtitle
                let mut encoded_packet = AVPacket::new();
                copy_payload_into_packet(&mut encoded_packet, &encoded_subtitle).with_context(
                    || {
                        format!(
                            "Could not allocate subtitle packet for stream {}",
                            stream_processing_context.input_stream_index
                        )
                    },
                )?;

                let pts = subtitle_pts(
                    subtitle.pts,
                    packet.pts,
                    stream_processing_context.last_written_dts,
                );
                let dts = subtitle_dts(packet.dts, pts);

                encoded_packet
                    .set_stream_index(stream_processing_context.output_stream_index.as_i32());
                encoded_packet.set_pts(pts);
                encoded_packet.set_dts(dts);
                encoded_packet.set_duration(packet.duration);
                encoded_packet.set_flags(packet.flags);

                let output_time_base = output_format_context.streams()
                    [stream_processing_context.output_stream_index.as_usize()]
                .time_base;
                encoded_packet.rescale_ts(
                    stream_processing_context.decode_context.time_base,
                    output_time_base,
                );

                if let Some((packet_dts, adjusted)) = enforce_monotonic_dts(
                    &mut encoded_packet,
                    stream_processing_context.last_written_dts,
                ) {
                    debug!(
                        "Subtitle stream {} adjusted DTS from {} to {}",
                        stream_processing_context.input_stream_index, packet_dts, adjusted
                    );
                }

                stream_processing_context.last_written_dts = Some(encoded_packet.dts);
                debug!(
                    "Subtitle stream {} final pts={} dts={} duration={} (tb={}/{})",
                    stream_processing_context.input_stream_index,
                    encoded_packet.pts,
                    encoded_packet.dts,
                    encoded_packet.duration,
                    output_time_base.num,
                    output_time_base.den
                );

                match output_format_context.interleaved_write_frame(&mut encoded_packet) {
                    Ok(()) => {}
                    Err(rsmpeg::error::RsmpegError::AVError(code)) if code == -EINVAL => {
                        return handle_subtitle_stream_failure(
                            stream_processing_context,
                            "produced invalid timestamps".to_string(),
                        );
                    }
                    Err(e) => {
                        return Err(e).context("Could not write subtitle packet");
                    }
                }
            }
        }
        Err(rsmpeg::error::RsmpegError::DecoderDrainError) => {
            return handle_subtitle_stream_failure(
                stream_processing_context,
                "decoder is drained".to_string(),
            );
        }
        Err(rsmpeg::error::RsmpegError::DecoderFlushedError) => {
            return handle_subtitle_stream_failure(
                stream_processing_context,
                "decoder is flushed".to_string(),
            );
        }
        Err(e) => {
            return handle_subtitle_stream_failure(
                stream_processing_context,
                format!("failed to decode ({e})"),
            );
        }
    }

    Ok(())
}

/// Applies the configured subtitle failure policy.
///
/// The default keeps selected subtitle stream failures isolated because many
/// media libraries prefer a playable A/V output over aborting for one malformed
/// subtitle stream. `Fail` exists for workflows that require strict subtitle
/// preservation.
fn handle_subtitle_stream_failure(
    stream_processing_context: &mut StreamProcessingContext,
    reason: String,
) -> Result<()> {
    match stream_processing_context.subtitle_failure_policy {
        SubtitleFailurePolicy::SkipStream => {
            warn!(
                "Subtitle stream {} {}; skipping the rest of this subtitle stream.",
                stream_processing_context.input_stream_index, reason
            );
            stream_processing_context.skip_stream = true;
            Ok(())
        }
        SubtitleFailurePolicy::Fail => {
            bail!(
                "Subtitle stream {} {}; failing due to --subtitle-failure-policy=fail",
                stream_processing_context.input_stream_index,
                reason
            )
        }
    }
}

/// Encodes a decoded subtitle into a right-sized byte buffer.
///
/// rsmpeg's convenience wrapper discards FFmpeg's returned byte count, which
/// makes callers guess the encoded packet length. This helper calls FFmpeg
/// directly so subtitle packets can preserve legitimate trailing zero bytes and
/// oversized text events can retry with a larger buffer before the stream is
/// skipped by policy.
fn encode_subtitle_to_vec(
    encode_context: &mut AVCodecContext,
    subtitle: &rsmpeg::avcodec::AVSubtitle,
    input_stream_index: i32,
) -> Result<Option<Vec<u8>>> {
    const INITIAL_SUBTITLE_PACKET_SIZE: usize = 32 * 1024;
    const MAX_SUBTITLE_PACKET_SIZE: usize = 1024 * 1024;

    let mut capacity = INITIAL_SUBTITLE_PACKET_SIZE;
    loop {
        let mut buffer = vec![0u8; capacity];
        let encoded_size = unsafe {
            ffi::avcodec_encode_subtitle(
                encode_context.as_mut_ptr(),
                buffer.as_mut_ptr(),
                buffer.len() as i32,
                subtitle.as_ptr(),
            )
        };

        if encoded_size < 0 {
            bail!(
                "FFmpeg subtitle encoder returned {}",
                av_error_to_string(encoded_size)
            );
        }

        if encoded_size == 0 {
            return Ok(None);
        }

        let encoded_size = encoded_size as usize;
        if encoded_size < buffer.len() || capacity >= MAX_SUBTITLE_PACKET_SIZE {
            buffer.truncate(encoded_size.min(buffer.len()));
            return Ok(Some(buffer));
        }

        capacity = (capacity * 2).min(MAX_SUBTITLE_PACKET_SIZE);
        debug!(
            "Subtitle stream {} filled a {} byte encode buffer; retrying with {} bytes",
            input_stream_index,
            buffer.len(),
            capacity
        );
    }
}

pub(crate) fn is_image_based_subtitle(codec_id: ffi::AVCodecID) -> bool {
    matches!(
        codec_id,
        ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE
            | ffi::AV_CODEC_ID_DVD_SUBTITLE
            | ffi::AV_CODEC_ID_DVB_SUBTITLE
            | ffi::AV_CODEC_ID_XSUB
    )
}

pub(crate) fn load_encode_and_write(
    fifo: &mut AVAudioFifo,
    output_format_context: &mut AVFormatContextOutput,
    encode_context: &mut AVCodecContext,
    stream_index: i32,
    pts: &mut AtomicI64,
) -> Result<()> {
    let frame_size = preferred_audio_frame_size(encode_context, fifo.size()).min(fifo.size());
    if frame_size <= 0 {
        return Ok(());
    }

    let mut frame = init_output_audio_frame(
        frame_size,
        encode_context.ch_layout().clone().into_inner(),
        encode_context.sample_fmt,
        encode_context.sample_rate,
    )
    .context("Failed to initialize audio frame.")?;

    if unsafe {
        let read_frame_size = fifo.read(frame.data_mut().as_mut_ptr(), frame_size)?;
        debug!("Read audio frame size (bytes): {}", read_frame_size);
        read_frame_size
    } < frame_size
    {
        bail!("Could not read data from FIFO");
    }

    frame.set_pts(pts.fetch_add(frame.nb_samples as i64, Ordering::Relaxed));

    encode_and_write_frame(
        encode_context,
        output_format_context,
        stream_index as usize,
        Some(frame),
    )
    .context("Error encoding audio frame!")?;

    Ok(())
}
