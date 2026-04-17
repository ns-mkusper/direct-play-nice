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

fn process_video_stream(
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
                stream_processing_context.stream_index,
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
                stream_processing_context.stream_index,
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
                        stream_processing_context.stream_index
                    );
                }
                break;
            }
            Err(e) if is_eagain_error(&e) => {
                debug!(
                    "Video decoder returned EAGAIN for stream {}",
                    stream_processing_context.stream_index
                );
                break;
            }
            Err(e) => {
                error!(
                    "Video decoder receive_frame failure on stream {} (decoder='{}', last-packet pts={}, dts={}, duration={}, tb={}/{}): {}",
                    stream_processing_context.stream_index,
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
                    stream_processing_context.stream_index,
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
            packet.stream_index as usize,
            Some(new_frame),
        )?;
    }

    Ok(())
}

fn process_audio_stream(
    stream_processing_context: &mut StreamProcessingContext,
    input_stream: &AVStreamRef,
    output_format_context: &mut AVFormatContextOutput,
    packet: &mut AVPacket,
) -> Result<()> {
    // TODO: ensure audio streams have the same metadata
    // based on https://github.com/larksuite/rsmpeg/blob/master/tests/ffmpeg_examples/transcode_aac.rs
    packet.rescale_ts(
        input_stream.time_base,
        stream_processing_context.decode_context.time_base,
    );

    let Some(fifo) = stream_processing_context.frame_buffer.as_mut() else {
        panic!("Failed to get Audio FIFO buffer!");
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
                stream_processing_context.stream_index,
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
                stream_processing_context.stream_index,
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
                        stream_processing_context.stream_index
                    );
                }
                break;
            }
            Err(e) if is_eagain_error(&e) => {
                debug!(
                    "Audio decoder returned EAGAIN for stream {}",
                    stream_processing_context.stream_index
                );
                break;
            }
            Err(e) => {
                error!(
                    "Audio decoder receive_frame failure on stream {} (decoder='{}', last-packet pts={}, dts={}, duration={}, tb={}/{}): {}",
                    stream_processing_context.stream_index,
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
                    stream_processing_context.stream_index,
                    format!("receive_frame failed: {}", e),
                )));
            }
        };

        let output_frame_size = stream_processing_context.encode_context.frame_size; // TODO: Why is this 0 in some cases and how to handle that?

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
            stream_processing_context.stream_index
        );
        while fifo.size() >= output_frame_size {
            load_encode_and_write(
                fifo,
                output_format_context,
                &mut stream_processing_context.encode_context,
                stream_processing_context.stream_index,
                &mut stream_processing_context.pts,
            )?;
        }
    }

    Ok(())
}

fn process_subtitle_stream(
    stream_processing_context: &mut StreamProcessingContext,
    input_stream: &AVStreamRef,
    output_format_context: &mut AVFormatContextOutput,
    packet: &mut AVPacket,
) -> Result<()> {
    if stream_processing_context.skip_stream {
        trace!(
            "Subtitle stream {} already skipped; dropping packet.",
            stream_processing_context.stream_index
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
                    stream_processing_context.stream_index, packet.pts, packet.dts, packet.duration
                );
                // TODO: Find the max size of subtitle data in a single packet
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

                encoded_packet.set_stream_index(stream_processing_context.stream_index);
                encoded_packet.set_pts(pts);
                encoded_packet.set_dts(dts);
                encoded_packet.set_duration(packet.duration);
                encoded_packet.set_flags(packet.flags);

                let output_time_base = output_format_context.streams()
                    [stream_processing_context.stream_index as usize]
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
                            stream_processing_context.stream_index, packet_dts, adjusted
                        );
                    }
                }

                stream_processing_context.last_written_dts = Some(encoded_packet.dts);
                debug!(
                    "Subtitle stream {} final pts={} dts={} duration={} (tb={}/{})",
                    stream_processing_context.stream_index,
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
                            stream_processing_context.stream_index
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

fn is_image_based_subtitle(codec_id: ffi::AVCodecID) -> bool {
    matches!(
        codec_id,
        ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE
            | ffi::AV_CODEC_ID_DVD_SUBTITLE
            | ffi::AV_CODEC_ID_DVB_SUBTITLE
            | ffi::AV_CODEC_ID_XSUB
    )
}

fn load_encode_and_write(
    fifo: &mut AVAudioFifo,
    output_format_context: &mut AVFormatContextOutput,
    encode_context: &mut AVCodecContext,
    stream_index: i32,
    pts: &mut AtomicI64,
) -> Result<()> {
    let frame_size = fifo.size().min(encode_context.frame_size); // TODO: should be encode_context.frame_size but it reads 0

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

const MAX_REASONABLE_FPS: f64 = 300.0;

fn is_valid_framerate(r: ffi::AVRational) -> bool {
    r.num > 0 && r.den > 0 && (r.num as f64 / r.den as f64) <= MAX_REASONABLE_FPS
}

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
                implied_fps,
                rational_to_string(fallback)
            );
            encode_time_base = fallback;
        }
    }

    encode_context.set_time_base(encode_time_base);
    output_stream.set_time_base(encode_time_base);
}

#[allow(clippy::too_many_arguments)]
fn set_h264_video_codec_par(
    decode_context: &mut AVCodecContext,
    encode_context: &mut AVCodecContext,
    output_stream: &mut AVStreamMut,
    input_stream: &AVStreamRef,
    h264_profile: H264Profile, // TODO: handle cases somewhere when target video codec is NOT h264
    h264_level: H264Level,
    quality_limits: &QualityLimits,
    device_max_resolution: Resolution,
    source_bit_rate_hint: i64,
    encoder_name: &str,
    is_constant_quality_mode: bool,
) {
    encode_context.set_sample_rate(decode_context.sample_rate);
    let device_cap = resolution_to_dimensions(device_max_resolution);
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
    encode_context.set_pix_fmt(ffi::AV_PIX_FMT_YUV420P); // TODO: downgrade more intelligently?
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
    // TODO: find a safe way to do this
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

#[allow(clippy::too_many_arguments)]
fn set_hevc_video_codec_par(
    decode_context: &mut AVCodecContext,
    encode_context: &mut AVCodecContext,
    output_stream: &mut AVStreamMut,
    input_stream: &AVStreamRef,
    quality_limits: &QualityLimits,
    device_max_resolution: Resolution,
    source_bit_rate_hint: i64,
    encoder_name: &str,
    is_constant_quality_mode: bool,
) {
    encode_context.set_sample_rate(decode_context.sample_rate);
    let device_cap = resolution_to_dimensions(device_max_resolution);
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

fn set_audio_codec_par(
    decode_context: &mut AVCodecContext,
    encode_context: &mut AVCodecContext,
    _output_stream: &mut AVStreamMut,
    quality_limits: &QualityLimits,
    source_bit_rate_hint: i64,
) {
    // TODO: Read input to determine output audio codec params
    let encoder = AVCodec::find_encoder(ffi::AV_CODEC_ID_AAC).expect("Could not find AAC encoder");
    let decode_channels = decode_context.ch_layout.nb_channels;
    encode_context.set_ch_layout(AVChannelLayout::from_nb_channels(decode_channels).into_inner());
    // The input file's sample rate is used to avoid a sample rate conversion.
    encode_context.set_sample_rate(decode_context.sample_rate);
    encode_context.set_sample_fmt(encoder.sample_fmts().unwrap()[0]); // TODO: Are we actually getting the sample rate we want?
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
}

fn set_subtitle_codec_par(
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

        // TODO: find safe way to do this
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

struct DirectPlayAssessment {
    compatible: bool,
    reasons: Vec<String>,
}

#[derive(Debug, Default)]
pub(crate) struct ConversionOutcome {
    h264_verification: Option<H264Verification>,
}

impl ConversionOutcome {
    fn profile_verified(&self) -> bool {
        self.h264_verification
            .as_ref()
            .map(|check| check.is_valid())
            .unwrap_or(true)
    }
}

#[allow(clippy::too_many_arguments)]
fn assess_direct_play_compatibility(
    input_file: &CStr,
    target_is_mp4: bool,
    sub_mode: SubMode,
    target_video_codec: ffi::AVCodecID,
    target_audio_codec: ffi::AVCodecID,
    h264_constraints: Option<(H264Profile, H264Level)>,
    max_fps: u32,
    device_cap: (u32, u32),
    supported_containers: &[ContainerFormat],
    quality_limits: &QualityLimits,
    primary_video_stream_index: Option<usize>,
    primary_criteria: PrimaryVideoCriteria,
) -> Result<DirectPlayAssessment> {
    let ictx = AVFormatContextInput::open(input_file)?;
    let primary_idx =
        select_primary_video_stream_index(&ictx, primary_video_stream_index, primary_criteria)?;

    let streams: Vec<_> = ictx.streams().iter().collect();
    let video_stream = streams.get(primary_idx).ok_or_else(|| {
        anyhow!(
            "Primary video stream index {} out of range while checking direct-play compatibility",
            primary_idx
        )
    })?;

    let mut reasons = Vec::new();
    let video_par = video_stream.codecpar();
    let input_path = PathBuf::from(input_file.to_string_lossy().into_owned());
    let input_ext = input_path
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext.to_ascii_lowercase())
        .unwrap_or_default();
    let input_container = ContainerFormat::from_extension(&input_ext);
    match input_container {
        Some(container) => {
            if !supported_containers.contains(&container) {
                reasons.push(format!(
                    "input container '{}' is not supported by all selected devices",
                    container.as_str()
                ));
            }
        }
        None => reasons.push(format!(
            "input container '{}' is unknown; cannot confirm compatibility",
            input_ext
        )),
    }

    for stream in &streams {
        let disposition_flags = unsafe { (*stream.as_ptr()).disposition };
        if (disposition_flags & ffi::AV_DISPOSITION_ATTACHED_PIC as i32) != 0 {
            reasons.push("input contains an attached picture stream".to_string());
            break;
        }
        if stream.codecpar().codec_type == ffi::AVMEDIA_TYPE_ATTACHMENT {
            reasons.push("input contains an attachment stream".to_string());
            break;
        }
    }

    if video_par.codec_id != target_video_codec {
        reasons.push(format!(
            "video codec {} is not compatible with required {}",
            describe_codec(video_par.codec_id),
            describe_codec(target_video_codec)
        ));
    }

    if video_par.width <= 0 || video_par.height <= 0 {
        reasons.push("video resolution unknown".to_string());
    } else if (video_par.width as u32) > device_cap.0 || (video_par.height as u32) > device_cap.1 {
        reasons.push(format!(
            "video resolution {}x{} exceeds device limit {}x{}",
            video_par.width, video_par.height, device_cap.0, device_cap.1
        ));
    }

    if let Some((quality_w, quality_h)) = quality_limits.max_video_dimensions {
        if video_par.width > 0
            && video_par.height > 0
            && ((video_par.width as u32) > quality_w || (video_par.height as u32) > quality_h)
        {
            reasons.push(format!(
                "video resolution {}x{} exceeds requested quality limit {}x{}",
                video_par.width, video_par.height, quality_w, quality_h
            ));
        }
    }

    if let Some(max_video_bitrate) = quality_limits.max_video_bitrate {
        let mut video_bit_rate = video_par.bit_rate;
        if video_bit_rate <= 0 {
            video_bit_rate = unsafe { (*(*video_stream.as_ptr()).codecpar).bit_rate };
        }
        if video_bit_rate <= 0 {
            reasons.push(
                "video bitrate unknown; cannot confirm compliance with requested quality limit"
                    .into(),
            );
        } else if video_bit_rate > max_video_bitrate {
            reasons.push(format!(
                "video bitrate {} bps exceeds requested limit {} bps",
                video_bit_rate, max_video_bitrate
            ));
        }
    }

    if max_fps > 0 {
        match estimate_stream_fps(video_stream) {
            Some(fps) => {
                if fps > max_fps as f64 + 0.5 {
                    reasons.push(format!(
                        "video frame rate {:.2} fps exceeds device limit {} fps",
                        fps, max_fps
                    ));
                }
            }
            None => reasons.push("video frame rate unknown; cannot confirm compatibility".into()),
        }
    }

    if target_video_codec == ffi::AV_CODEC_ID_H264 {
        let (min_h264_profile, min_h264_level) =
            h264_constraints.expect("missing h264 constraints");
        check_h264_profile_level_constraints(
            video_par.codec_id,
            video_par.profile,
            video_par.level,
            min_h264_profile,
            min_h264_level,
            &mut reasons,
        );
    }

    let mut audio_ok = false;
    let mut audio_quality_reason: Option<String> = None;
    for stream in &streams {
        let codecpar = stream.codecpar();
        if codecpar.codec_type != ffi::AVMEDIA_TYPE_AUDIO {
            continue;
        }
        if codecpar.codec_id != target_audio_codec {
            continue;
        }

        if let Some(max_audio_bitrate) = quality_limits.max_audio_bitrate {
            let mut audio_bit_rate = codecpar.bit_rate;
            if audio_bit_rate <= 0 {
                audio_bit_rate = unsafe { (*(*stream.as_ptr()).codecpar).bit_rate };
            }

            if audio_bit_rate <= 0 {
                if audio_quality_reason.is_none() {
                    audio_quality_reason = Some(
                        "audio bitrate unknown; cannot confirm compliance with requested quality limit"
                            .into(),
                    );
                }
                continue;
            }

            if audio_bit_rate > max_audio_bitrate {
                if audio_quality_reason.is_none() {
                    audio_quality_reason = Some(format!(
                        "audio bitrate {} bps exceeds requested limit {} bps",
                        audio_bit_rate, max_audio_bitrate
                    ));
                }
                continue;
            }
        }

        audio_ok = true;
        break;
    }

    if !audio_ok {
        if let Some(reason) = audio_quality_reason {
            reasons.push(reason);
        } else {
            reasons.push(format!(
                "no audio stream with compatible codec {} found",
                describe_codec(target_audio_codec)
            ));
        }
    }

    if target_is_mp4 && !matches!(sub_mode, SubMode::Skip) {
        for stream in &streams {
            let codecpar = stream.codecpar();
            if codecpar.codec_type == ffi::AVMEDIA_TYPE_SUBTITLE
                && is_image_based_subtitle(codecpar.codec_id)
            {
                reasons.push(format!(
                    "bitmap subtitle stream {} requires OCR conversion for MP4 direct-play",
                    stream.index
                ));
                break;
            }
        }
    }

    Ok(DirectPlayAssessment {
        compatible: reasons.is_empty(),
        reasons,
    })
}

fn estimate_stream_fps(stream: &AVStreamRef) -> Option<f64> {
    if let Some(rational) = stream.guess_framerate() {
        rational_to_f64(rational)
    } else {
        let avg = unsafe { (*stream.as_ptr()).avg_frame_rate };
        rational_to_f64(avg)
    }
}

pub(crate) fn rational_to_f64(rational: ffi::AVRational) -> Option<f64> {
    if rational.num <= 0 || rational.den <= 0 {
        None
    } else {
        Some(rational.num as f64 / rational.den as f64)
    }
}

