//! Stream-processing routines that execute per-stream decode/encode flows for video, audio, and subtitle data.

use crate::transcoder::prelude::*;

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
                stream_processing_context.input_stream_index,
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
                    stream_processing_context.input_stream_index,
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
            ffi::SWS_FAST_BILINEAR | ffi::SWS_ACCURATE_RND,
            None,
            None,
            None,
        )
        .context("Failed to create a swscale context.")?;

        // Ensure the encoder sees timestamps in its own time base to avoid inflated durations.
        let mut rescaled_pts = if frame.best_effort_timestamp != ffi::AV_NOPTS_VALUE {
            frame.best_effort_timestamp
        } else {
            frame.pts
        };
        if rescaled_pts != ffi::AV_NOPTS_VALUE {
            rescaled_pts = unsafe {
                ffi::av_rescale_q(
                    rescaled_pts,
                    stream_processing_context.decode_context.time_base,
                    stream_processing_context.encode_context.time_base,
                )
            };
        }

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
            stream_processing_context.output_stream_index as usize,
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
                stream_processing_context.input_stream_index,
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
                    stream_processing_context.input_stream_index,
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
                stream_processing_context.output_stream_index,
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
                // Conservative fixed buffer to avoid per-packet allocations in the hot path.
                // If we see larger subtitle payloads in the future, raise this value or switch
                // to a growth strategy.
                const MAX_SUBTITLE_PACKET_SIZE: usize = 32 * 1024; // 32KB
                let mut subtitle_buffer = vec![0u8; MAX_SUBTITLE_PACKET_SIZE];
                stream_processing_context
                    .encode_context
                    .encode_subtitle(&subtitle, &mut subtitle_buffer)?;

                let encoded_size = subtitle_buffer
                    .iter()
                    .rposition(|&x| x != 0)
                    .map(|pos| pos + 1)
                    .unwrap_or(0);

                if encoded_size == 0 {
                    return Ok(());
                }

                // Create a new packet for the encoded subtitle
                let mut encoded_packet = AVPacket::new();
                unsafe {
                    ffi::av_new_packet(encoded_packet.as_mut_ptr(), encoded_size as i32);
                    std::ptr::copy_nonoverlapping(
                        subtitle_buffer.as_ptr(),
                        (*encoded_packet.as_mut_ptr()).data,
                        encoded_size,
                    );
                }

                let mut pts = if subtitle.pts != ffi::AV_NOPTS_VALUE {
                    subtitle.pts
                } else {
                    packet.pts
                };
                let mut dts = if packet.dts != ffi::AV_NOPTS_VALUE {
                    packet.dts
                } else {
                    pts
                };

                if pts == ffi::AV_NOPTS_VALUE {
                    pts = stream_processing_context
                        .last_written_dts
                        .map(|prev| prev + 1)
                        .unwrap_or(0);
                }

                if dts == ffi::AV_NOPTS_VALUE {
                    dts = pts;
                }

                encoded_packet.set_stream_index(stream_processing_context.output_stream_index);
                encoded_packet.set_pts(pts);
                encoded_packet.set_dts(dts);
                encoded_packet.set_duration(packet.duration);
                encoded_packet.set_flags(packet.flags);

                let output_time_base = output_format_context.streams()
                    [stream_processing_context.output_stream_index as usize]
                    .time_base;
                encoded_packet.rescale_ts(
                    stream_processing_context.decode_context.time_base,
                    output_time_base,
                );

                let packet_dts = encoded_packet.dts;
                if let Some(prev_dts) = stream_processing_context.last_written_dts {
                    if packet_dts <= prev_dts {
                        let adjusted = prev_dts + 1;
                        encoded_packet.set_dts(adjusted);
                        if encoded_packet.pts < adjusted {
                            encoded_packet.set_pts(adjusted);
                        }
                        debug!(
                            "Subtitle stream {} adjusted DTS from {} to {}",
                            stream_processing_context.input_stream_index, packet_dts, adjusted
                        );
                    }
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
                        warn!(
                            "Subtitle stream {} produced invalid timestamps; skipping rest of stream.",
                            stream_processing_context.input_stream_index
                        );
                        stream_processing_context.skip_stream = true;
                    }
                    Err(e) => {
                        return Err(e).context("Could not write subtitle packet");
                    }
                }
            }
        }
        Err(rsmpeg::error::RsmpegError::DecoderDrainError) => {
            error!("Error: The decoder has been fully drained, no more subtitles to decode. Continuing...");
        }
        Err(rsmpeg::error::RsmpegError::DecoderFlushedError) => {
            error!(
                "Error: The decoder has been flushed, no more subtitles to decode. Continuing..."
            );
        }
        Err(e) => {
            error!("Error decoding subtitle: {}", e);
        }
    }

    Ok(())
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
