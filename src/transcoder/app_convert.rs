//! Core transcoding execution path that coordinates decode, transform, encode, and mux stages.

use crate::transcoder::prelude::*;

#[derive(Clone, Copy)]
/// Immutable conversion plan resolved before stream processing starts.
///
/// This bundles policy-level decisions (codecs, quality caps, stream policy) so
/// retry flows can rerun conversion with a modified hardware preference while
/// keeping all other inputs identical.
pub(crate) struct ConversionParams<'a> {
    pub(crate) sub_mode: SubMode,
    pub(crate) target_video_codec: ffi::AVCodecID,
    pub(crate) target_audio_codec: ffi::AVCodecID,
    pub(crate) h264_constraints: Option<(H264Profile, H264Level)>,
    pub(crate) min_fps: u32,
    pub(crate) device_max_resolution: Resolution,
    pub(crate) quality_limits: &'a QualityLimits,
    pub(crate) uv_policy: UnsupportedVideoPolicy,
    pub(crate) primary_video_stream_index: Option<usize>,
    pub(crate) primary_criteria: PrimaryVideoCriteria,
    pub(crate) requested_video_quality: VideoQuality,
    pub(crate) requested_audio_quality: AudioQuality,
    pub(crate) upscale_mode: UpscaleMode,
    pub(crate) scaler_quality: ScalerQuality,
    pub(crate) skip_codec_check: bool,
    pub(crate) subtitle_failure_policy: SubtitleFailurePolicy,
    pub(crate) hw_accel: HwAccel,
}

impl ConversionParams<'_> {
    pub(crate) fn with_hw_accel(self, hw_accel: HwAccel) -> Self {
        Self { hw_accel, ..self }
    }
}

pub(crate) fn convert_video_file(
    input_file: &CStr,
    output_file: &CStr,
    params: ConversionParams<'_>,
) -> Result<ConversionOutcome> {
    let ConversionParams {
        sub_mode,
        target_video_codec,
        target_audio_codec,
        h264_constraints,
        min_fps: _min_fps,
        device_max_resolution,
        quality_limits,
        uv_policy,
        primary_video_stream_index,
        primary_criteria,
        requested_video_quality,
        requested_audio_quality,
        upscale_mode,
        scaler_quality,
        skip_codec_check,
        subtitle_failure_policy,
        hw_accel,
    } = params;

    // H.264 profile/level checks are only meaningful when the output codec is H.264.
    let h264_constraints = if target_video_codec == ffi::AV_CODEC_ID_H264 {
        h264_constraints
    } else {
        None
    };
    let mut input_format_context = AVFormatContextInput::open(input_file)?;
    if log::log_enabled!(Level::Debug) {
        input_format_context.dump(0, input_file)?;
    }

    let mut output_format_context = AVFormatContextOutput::create(output_file)?;

    let output_path_buf = cstr_to_path_buf(output_file);
    let output_path = output_path_buf.as_path();

    let mut stream_contexts: Vec<StreamProcessingContext> = Vec::new();
    let mut progress_tracker = create_progress_tracker(&mut input_format_context);

    info!(
        "Target codecs resolved: video={}, audio={}",
        describe_codec(target_video_codec),
        describe_codec(target_audio_codec)
    );

    let is_constant_quality_mode = requested_video_quality == VideoQuality::MatchSource;

    let allow_cuda_hw_decode = matches!(hw_accel, HwAccel::Auto | HwAccel::Nvenc);
    let shared_hw_device = if allow_cuda_hw_decode {
        acquire_hw_device(hw_accel)
    } else {
        None
    };

    if log::log_enabled!(Level::Debug) {
        if let Ok(dav1d_name) = CString::new("libdav1d") {
            let present = AVCodec::find_decoder_by_name(dav1d_name.as_c_str()).is_some();
            debug!("libdav1d decoder available: {}", present);
        }
        if let Ok(libaom_name) = CString::new("libaom-av1") {
            let present = AVCodec::find_decoder_by_name(libaom_name.as_c_str()).is_some();
            debug!("libaom-av1 decoder available: {}", present);
        }
    }

    let mut logged_video_encoder = false;
    let mut logged_audio_encoder = false;
    let mut desired_h264_profile: Option<H264Profile> = None;
    let mut desired_h264_level: Option<H264Level> = None;
    let mut last_video_encoder_name: Option<String> = None;
    let mut hardware_encoder_used = false;
    let mut hw_decode_blacklist: HashSet<ffi::AVCodecID> = HashSet::new();

    // Exactly one video stream is treated as the primary conversion target.
    let primary_index = select_primary_video_stream_index(
        &input_format_context,
        primary_video_stream_index,
        primary_criteria,
    )?;
    let mut _video_streams_seen = 0usize;
    let mut video_streams_added = 0usize;

    for stream in input_format_context.streams() {
        let input_codec_type = stream.codecpar().codec_type;
        if should_skip_auxiliary_stream(stream, input_codec_type) {
            continue;
        }

        let input_stream_codecpar = stream.codecpar();
        let input_codec_id = input_stream_codecpar.codec_id;
        let input_codec_name = codec_name(input_codec_id);
        // Disable hardware decode per codec after the first hard failure to
        // avoid repeated slow failures on every stream with that codec.
        let prefer_hw_decode = allow_cuda_hw_decode
            && shared_hw_device.is_some()
            && input_codec_type == ffi::AVMEDIA_TYPE_VIDEO
            && !hw_decode_blacklist.contains(&input_codec_id);
        let mut decoder = match find_decoder_with_fallback(input_codec_id, prefer_hw_decode) {
            Some(dec) => dec,
            None if input_codec_type == ffi::AVMEDIA_TYPE_SUBTITLE => {
                warn!(
                    "Skipping subtitle stream {} (codec {}): decoder not available.",
                    stream.index, input_codec_name
                );
                continue;
            }
            None => {
                bail!(
                    "Decoder not found for stream {} (codec {}).",
                    stream.index,
                    input_codec_name
                );
            }
        };
        let mut decoder_name_owned = decoder.name().to_string_lossy().into_owned();
        debug!(
            "Selected decoder '{}' for stream {} (codec {})",
            decoder_name_owned, stream.index, input_codec_name
        );
        let mut decode_context = new_decode_context(&decoder, stream)?;

        let mut hw_decoder_active = false;
        let mut need_software_retry = false;
        if prefer_hw_decode
            && (decoder_name_owned.contains("cuvid") || decoder_name_owned.contains("nvdec"))
        {
            if let Some(device) = shared_hw_device {
                match configure_cuda_hw_decoder(&mut decode_context, device) {
                    Ok(()) => {
                        hw_decoder_active = true;
                        ensure_decoder_pkt_time_base(&mut decode_context, stream.time_base);
                        debug!(
                            "Configured CUDA hardware decoder '{}' for stream {}",
                            decoder_name_owned, stream.index
                        );
                    }
                    Err(err) => {
                        warn!(
                            "Failed to configure CUDA hardware decoder '{}' for stream {}: {}; falling back to software decode",
                            decoder_name_owned,
                            stream.index,
                            err
                        );
                        clear_decoder_hardware_state(&mut decode_context);
                        if hw_decode_blacklist.insert(input_codec_id) {
                            debug!(
                                "Disabling CUDA hardware decode for codec {} after init failure",
                                describe_codec(input_codec_id)
                            );
                        }
                        need_software_retry = true;
                    }
                }
            }
        }

        if need_software_retry {
            if let Some(sw_decoder) = find_decoder_with_fallback(input_codec_id, false) {
                decoder = sw_decoder;
                decoder_name_owned = decoder.name().to_string_lossy().into_owned();
                debug!(
                    "Retrying stream {} with software decoder '{}'",
                    stream.index, decoder_name_owned
                );
                decode_context = new_decode_context(&decoder, stream)?;
            } else {
                warn!(
                    "Hardware decoder '{}' unavailable and no software fallback found for stream {}",
                    decoder_name_owned, stream.index
                );
            }
        }

        let mut decoder_open_error = decode_context.open(None).err();

        if hw_decoder_active {
            if let Some(open_err) = decoder_open_error.take() {
                warn!(
                    "Hardware decoder '{}' failed to open for stream {}: {}; retrying with software decoder",
                    decoder_name_owned,
                    stream.index,
                    open_err
                );
                let mut fallback_error = Some(open_err);
                if let Some(sw_decoder) = find_decoder_with_fallback(input_codec_id, false) {
                    decoder = sw_decoder;
                    decoder_name_owned = decoder.name().to_string_lossy().into_owned();
                    debug!(
                        "Retrying stream {} with software decoder '{}'",
                        stream.index, decoder_name_owned
                    );
                    decode_context = new_decode_context(&decoder, stream)?;
                    fallback_error = decode_context.open(None).err();
                    hw_decoder_active = false;
                    if hw_decode_blacklist.insert(input_codec_id) {
                        debug!(
                            "Disabling CUDA hardware decode for codec {} after open failure",
                            describe_codec(input_codec_id)
                        );
                    }
                }
                decoder_open_error = fallback_error;
            }
        }

        if let Some(err) = decoder_open_error {
            if hw_decode_blacklist.insert(input_codec_id) {
                debug!(
                    "Disabling CUDA hardware decode for codec {} due to decoder error",
                    describe_codec(input_codec_id)
                );
            }
            if input_codec_type == ffi::AVMEDIA_TYPE_SUBTITLE {
                warn!(
                    "Skipping subtitle stream {} (codec {}): failed to open decoder ({}).",
                    stream.index, input_codec_name, err
                );
                continue;
            } else {
                return Err(anyhow!(
                    "Error opening decoder for stream {} (codec {}): {}",
                    stream.index,
                    input_codec_name,
                    err
                ));
            }
        }

        debug!(
            "Decoder '{}' ready for stream {} (hardware={})",
            decoder_name_owned, stream.index, hw_decoder_active
        );

        let mut encode_context: AVCodecContext;
        let media_type: ffi::AVMediaType;
        let mut frame_buffer: Option<AVAudioFifo> = None;
        let mut resample_context: Option<SwrContext> = None;
        let mut hw_device_ctx_ptr: Option<*mut ffi::AVBufferRef> = None;
        let mut encoder_name_for_video: Option<String> = None;
        let mut target_h264_profile: Option<H264Profile> = None;
        let mut target_h264_level: Option<H264Level> = None;

        let is_video_stream = decode_context.codec_type == ffi::AVMEDIA_TYPE_VIDEO;
        if is_video_stream && stream.index as usize != primary_index {
            // Secondary video streams are controlled by unsupported-video policy.
            // This keeps multi-video files deterministic for direct-play targets.
            match uv_policy {
                UnsupportedVideoPolicy::Ignore => {
                    warn!(
                        "Ignoring extra video stream (index {}) due to policy 'ignore'",
                        stream.index
                    );
                    continue;
                }
                UnsupportedVideoPolicy::Fail => {
                    bail!(
                        "Encountered extra video stream (index {}). Rerun with --unsupported-video-policy=convert or ignore.",
                        stream.index
                    );
                }
                UnsupportedVideoPolicy::Convert => { /* continue */ }
            }
        }

        if decode_context.codec_type == ffi::AVMEDIA_TYPE_SUBTITLE {
            if matches!(sub_mode, SubMode::Skip) {
                info!(
                    "Skipping subtitle stream {} due to --sub-mode=skip",
                    stream.index
                );
                continue;
            }
            if is_image_based_subtitle(decode_context.codec_id) {
                info!(
                    "Deferring bitmap subtitle stream {} (codec {}) to OCR side pass.",
                    stream.index,
                    codec_name(decode_context.codec_id)
                );
                continue;
            }
        }

        let mut output_stream = output_format_context.new_stream();

        let mut encoder_is_hw = false;
        let mut current_encoder_name: Option<String> = None;

        match decode_context.codec_type {
            ffi::AVMEDIA_TYPE_VIDEO => {
                // Prefer HW encoder when available and requested
                let (maybe_hw_encoder, maybe_hw_dev) =
                    find_hw_encoder(target_video_codec, hw_accel, shared_hw_device);
                let (encoder, using_hw_encoder) = match maybe_hw_encoder {
                    Some(enc) => (enc, true),
                    None => (
                        AVCodec::find_encoder(target_video_codec).ok_or_else(|| {
                            anyhow!(
                                "Could not find {} encoder",
                                describe_codec(target_video_codec)
                            )
                        })?,
                        false,
                    ),
                };
                if using_hw_encoder {
                    hardware_encoder_used = true;
                }
                encoder_is_hw = using_hw_encoder;
                encode_context = AVCodecContext::new(&encoder);
                // Attach hardware device if present (must be set before open())
                if let Some(buf) = maybe_hw_dev {
                    unsafe {
                        (*encode_context.as_mut_ptr()).hw_device_ctx = ffi::av_buffer_ref(buf);
                    }
                    hw_device_ctx_ptr = Some(buf);
                }
                media_type = ffi::AVMEDIA_TYPE_VIDEO;

                let encoder_name_owned = encoder.name().to_string_lossy().into_owned();
                let encoder_key = CString::new("encoder")
                    .map_err(|_| anyhow!("Failed to build encoder metadata key"))?;
                let encoder_value = CString::new(encoder_name_owned.clone())
                    .or_else(|_| CString::new("unknown"))
                    .map_err(|_| anyhow!("Failed to build encoder metadata value"))?;
                let video_metadata = stream
                    .metadata()
                    .as_deref()
                    .cloned()
                    .map(|dict| dict.set(&encoder_key, &encoder_value, 0))
                    .unwrap_or_else(|| AVDictionary::new(&encoder_key, &encoder_value, 0));
                output_stream.set_metadata(Some(video_metadata));
                current_encoder_name = Some(encoder_name_owned.clone());
                encoder_name_for_video = Some(encoder_name_owned.clone());
                if let Some((min_h264_profile, min_h264_level)) = h264_constraints {
                    target_h264_profile = Some(min_h264_profile);
                    target_h264_level = Some(min_h264_level);
                    desired_h264_profile = Some(min_h264_profile);
                    desired_h264_level = Some(min_h264_level);
                }
                last_video_encoder_name = Some(encoder_name_owned.clone());
                let encoder_name_lower = encoder_name_owned.to_ascii_lowercase();
                if encoder_name_lower.contains("nvenc")
                    || encoder_name_lower.contains("qsv")
                    || encoder_name_lower.contains("amf")
                    || encoder_name_lower.contains("vaapi")
                    || encoder_name_lower.contains("videotoolbox")
                    || encoder_name_lower.contains("_mf")
                {
                    hardware_encoder_used = true;
                }

                if !logged_video_encoder {
                    let encoder_name = &encoder_name_owned;
                    let is_hw = using_hw_encoder
                        || encoder_name.contains("nvenc")
                        || encoder_name.contains("qsv")
                        || encoder_name.contains("amf")
                        || encoder_name.contains("vaapi")
                        || encoder_name.contains("videotoolbox")
                        || encoder_name.contains("_mf");
                    let summary = if is_hw {
                        if maybe_hw_dev.is_some() {
                            format!("hardware (preference {:?}, {})", hw_accel, encoder_name)
                        } else {
                            format!("hardware ({})", encoder_name)
                        }
                    } else if matches!(hw_accel, HwAccel::None) {
                        format!("software ({}, hardware disabled)", encoder_name)
                    } else if matches!(hw_accel, HwAccel::Auto) {
                        format!("software ({}; auto fallback)", encoder_name)
                    } else {
                        format!("software ({}; {:?} unavailable)", encoder_name, hw_accel)
                    };
                    info!("Video encoder selected: {}", summary);
                    logged_video_encoder = true;
                }

                if let Some((min_h264_profile, min_h264_level)) = h264_constraints {
                    set_h264_video_codec_par(
                        &mut decode_context,
                        &mut encode_context,
                        &mut output_stream,
                        stream,
                        H264VideoCodecParams {
                            h264_profile: min_h264_profile,
                            h264_level: min_h264_level,
                            quality_limits,
                            device_max_resolution,
                            source_bit_rate_hint: input_stream_codecpar.bit_rate,
                            encoder_name: &encoder_name_owned,
                            is_constant_quality_mode,
                            upscale_mode,
                        },
                    );
                } else {
                    set_hevc_video_codec_par(
                        &mut decode_context,
                        &mut encode_context,
                        &mut output_stream,
                        stream,
                        HevcVideoCodecParams {
                            quality_limits,
                            device_max_resolution,
                            source_bit_rate_hint: input_stream_codecpar.bit_rate,
                            encoder_name: &encoder_name_owned,
                            is_constant_quality_mode,
                            upscale_mode,
                        },
                    );
                }
                let src_par = stream.codecpar();
                info!(
                    "Video stream {}: {}x{} {} -> {}x{} {}{}",
                    output_stream.index,
                    src_par.width,
                    src_par.height,
                    codec_name(src_par.codec_id),
                    encode_context.width,
                    encode_context.height,
                    describe_codec(target_video_codec),
                    if requested_video_quality == VideoQuality::MatchSource {
                        format!(
                            " (~{} approx)",
                            nearest_video_preset(
                                encode_context.width,
                                encode_context.height,
                                encode_context.bit_rate
                            )
                        )
                    } else {
                        String::new()
                    }
                );
                info!(
                    "Prepared video stream {} -> {}x{} (target {} bps)",
                    output_stream.index,
                    encode_context.width,
                    encode_context.height,
                    encode_context.bit_rate
                );
                _video_streams_seen += 1;
                video_streams_added += 1;
            }
            ffi::AVMEDIA_TYPE_AUDIO => {
                output_stream.set_metadata(stream.metadata().as_deref().cloned());
                let encoder = AVCodec::find_encoder(target_audio_codec).ok_or_else(|| {
                    anyhow!(
                        "Could not find {} encoder",
                        describe_codec(target_audio_codec)
                    )
                })?;

                encode_context = AVCodecContext::new(&encoder);
                media_type = ffi::AVMEDIA_TYPE_AUDIO;

                if !logged_audio_encoder {
                    let encoder_name = encoder.name().to_string_lossy().into_owned();
                    info!("Audio encoder selected: software ({})", encoder_name);
                    logged_audio_encoder = true;
                }

                set_audio_codec_par(
                    &mut decode_context,
                    &mut encode_context,
                    &mut output_stream,
                    quality_limits,
                    input_stream_codecpar.bit_rate,
                )?;
                let src_audio = stream.codecpar();
                let src_audio_channels = src_audio.ch_layout().nb_channels;
                info!(
                    "Audio stream {}: {} ch @ {} Hz {} -> {} ch @ {} Hz {}{}",
                    output_stream.index,
                    src_audio_channels,
                    src_audio.sample_rate,
                    codec_name(src_audio.codec_id),
                    encode_context.ch_layout.nb_channels,
                    encode_context.sample_rate,
                    describe_codec(target_audio_codec),
                    if requested_audio_quality == AudioQuality::MatchSource {
                        format!(
                            " (~{} approx)",
                            nearest_audio_preset(encode_context.bit_rate)
                        )
                    } else {
                        String::new()
                    }
                );
                info!(
                    "Audio stream {} target bitrate: {} bps",
                    output_stream.index, encode_context.bit_rate
                );
                // Always route decoded audio through swresample. The packet
                // loop writes the converted `AVSamples` buffer into the FIFO,
                // so continuing without a resampler would feed the encoder
                // uninitialized samples rather than valid decoded audio.
                resample_context = Some(
                    init_audio_resampler(&mut decode_context, &mut encode_context).with_context(
                        || {
                            format!(
                                "Failed to initialize audio resampler for input stream {}",
                                stream.index
                            )
                        },
                    )?,
                );

                // Initialize the FIFO buffer to store audio samples to be encoded.
                frame_buffer = Some(AVAudioFifo::new(
                    encode_context.sample_fmt,
                    encode_context.ch_layout.nb_channels,
                    1,
                ));
            }
            ffi::AVMEDIA_TYPE_SUBTITLE => {
                output_stream.set_metadata(stream.metadata().as_deref().cloned());

                let encoder = AVCodec::find_encoder(ffi::AV_CODEC_ID_MOV_TEXT)
                    .ok_or_else(|| anyhow!("Could not find MOV_TEXT encoder"))?;
                encode_context = AVCodecContext::new(&encoder);
                media_type = ffi::AVMEDIA_TYPE_SUBTITLE;
                set_subtitle_codec_par(
                    &mut decode_context,
                    &mut encode_context,
                    &mut output_stream,
                );
                info!(
                    "Subtitle stream {}: {} -> {}",
                    output_stream.index,
                    codec_name(stream.codecpar().codec_id),
                    describe_codec(ffi::AV_CODEC_ID_MOV_TEXT)
                );
            }
            unsupported_type => {
                debug!(
                    "Encountered unsupported stream type ({}). Not setting up Codec.",
                    unsupported_type
                );
                continue;
            }
        }

        let media_label = match media_type {
            ffi::AVMEDIA_TYPE_VIDEO => "video",
            ffi::AVMEDIA_TYPE_AUDIO => "audio",
            ffi::AVMEDIA_TYPE_SUBTITLE => "subtitle",
            _ => "stream",
        };

        if media_type == ffi::AVMEDIA_TYPE_VIDEO {
            if let (Some(profile), Some(level), Some(encoder_name)) = (
                target_h264_profile,
                target_h264_level,
                encoder_name_for_video.as_deref(),
            ) {
                enforce_h264_constraints(&mut encode_context, profile, level, encoder_name);
            }
        }

        let open_result = encode_context
            .open(None)
            .with_context(|| format!("Error opening {} encoder", media_label));
        if let Err(err) = open_result {
            if encoder_is_hw && media_type == ffi::AVMEDIA_TYPE_VIDEO {
                return Err(anyhow!(HwEncoderInitError::new(
                    current_encoder_name.unwrap_or_else(|| "unknown".to_string()),
                    err.to_string(),
                )));
            } else {
                return Err(err);
            }
        }

        output_stream.set_codecpar(encode_context.extract_codecpar());

        let stream_process_context = StreamProcessingContext {
            decode_context,
            encode_context,
            input_stream_index: InputStreamId::new(stream.index),
            output_stream_index: OutputStreamId::new(output_stream.index),
            media_type,
            frame_buffer,
            resample_context,
            pts: AtomicI64::new(0),
            last_written_dts: None,
            skip_stream: false,
            subtitle_failure_policy,
            hw_device_ctx: hw_device_ctx_ptr,
            decoder_name: decoder.name().to_string_lossy().into_owned(),
            scaler_quality,
        };

        stream_contexts.push(stream_process_context);
    }

    write_output_header(&mut output_format_context, video_streams_added)?;
    process_packets(
        &mut input_format_context,
        &mut stream_contexts,
        &mut output_format_context,
        &mut progress_tracker,
    )?;
    flush_stream_contexts(&mut stream_contexts, &mut output_format_context)?;

    if let Some(progress) = progress_tracker.as_mut() {
        progress.finish();
    }

    log_output_stream_summaries(
        &stream_contexts,
        target_video_codec,
        target_audio_codec,
        requested_video_quality,
        requested_audio_quality,
    );

    output_format_context.write_trailer()?;

    let h264_verification = verify_h264_output(H264VerificationRequest {
        target_video_codec,
        skip_codec_check,
        desired_h264_profile,
        desired_h264_level,
        output_file,
        output_path,
        last_video_encoder_name: last_video_encoder_name.as_deref(),
        hardware_encoder_used,
    })?;

    if let Some(device) = shared_hw_device {
        unref_buffer_ref(device);
    }

    Ok(ConversionOutcome { h264_verification })
}

/// Writes the container header and improves the error when multi-video output is unsupported.
fn write_output_header(
    output_format_context: &mut AVFormatContextOutput,
    video_streams_added: usize,
) -> Result<()> {
    if let Err(e) = output_format_context.write_header(&mut None) {
        if video_streams_added > 1 {
            bail!(
                "Failed to write container header ({}). The output container may not support multiple video streams. Try --unsupported-video-policy=ignore to drop extra video streams.",
                e
            );
        }
        return Err(anyhow!(e)).context("Error writing output file header");
    }
    Ok(())
}

/// Reads, routes, and processes all demuxed packets through their stream contexts.
fn process_packets(
    input_format_context: &mut AVFormatContextInput,
    stream_contexts: &mut [StreamProcessingContext],
    output_format_context: &mut AVFormatContextOutput,
    progress_tracker: &mut Option<ProgressTracker>,
) -> Result<()> {
    loop {
        let mut packet = match input_format_context.read_packet()? {
            Some(x) => x,
            None => break,
        };

        let Some(stream_processing_context) = stream_contexts
            .iter_mut()
            .find(|context| context.input_stream_index.as_i32() == packet.stream_index)
        else {
            debug!(
                "Skipping packet for stream {} with no processing context (likely attachment).",
                packet.stream_index
            );
            continue;
        };

        let input_stream: &rsmpeg::avformat::AVStreamRef<'_> =
            &input_format_context.streams()[packet.stream_index as usize];
        match stream_processing_context.media_type {
            ffi::AVMEDIA_TYPE_VIDEO => process_video_stream(
                stream_processing_context,
                input_stream,
                output_format_context,
                &mut packet,
                progress_tracker.as_mut(),
            )?,
            ffi::AVMEDIA_TYPE_AUDIO => process_audio_stream(
                stream_processing_context,
                input_stream,
                output_format_context,
                &mut packet,
            )?,
            ffi::AVMEDIA_TYPE_SUBTITLE => process_subtitle_stream(
                stream_processing_context,
                input_stream,
                output_format_context,
                &mut packet,
            )?,
            unsupported_type => {
                debug!(
                    "Encountered unsupported stream type ({}). Not setting up Codec.",
                    unsupported_type
                );
            }
        }
    }
    Ok(())
}

/// Flushes all encoders and drains buffered audio data before container finalization.
fn flush_stream_contexts(
    stream_contexts: &mut [StreamProcessingContext],
    output_format_context: &mut AVFormatContextOutput,
) -> Result<()> {
    for context in stream_contexts {
        match context.media_type {
            ffi::AVMEDIA_TYPE_VIDEO => {
                encode_and_write_frame(
                    &mut context.encode_context,
                    output_format_context,
                    context.output_stream_index.as_usize(),
                    None,
                )
                .context("Failed to flush video encoder.")?;
                if let Some(dev) = context.hw_device_ctx {
                    unref_buffer_ref(dev);
                }
            }
            ffi::AVMEDIA_TYPE_AUDIO => {
                if let Some(fifo) = context.frame_buffer.as_mut() {
                    while fifo.size() > 0 {
                        load_encode_and_write(
                            fifo,
                            output_format_context,
                            &mut context.encode_context,
                            context.output_stream_index.as_i32(),
                            &mut context.pts,
                        )
                        .context("Failed to drain buffered audio samples.")?;
                    }
                }
                encode_and_write_frame(
                    &mut context.encode_context,
                    output_format_context,
                    context.output_stream_index.as_usize(),
                    None,
                )
                .context("Failed to flush audio encoder.")?;
            }
            ffi::AVMEDIA_TYPE_SUBTITLE => {}
            unsupported_type => {
                debug!(
                    "Encountered unsupported stream type ({}). Not flushing.",
                    unsupported_type
                );
            }
        }
    }
    Ok(())
}

/// Logs final per-stream output characteristics after all data has been encoded.
fn log_output_stream_summaries(
    stream_contexts: &[StreamProcessingContext],
    target_video_codec: ffi::AVCodecID,
    target_audio_codec: ffi::AVCodecID,
    requested_video_quality: VideoQuality,
    requested_audio_quality: AudioQuality,
) {
    for context in stream_contexts {
        match context.media_type {
            ffi::AVMEDIA_TYPE_VIDEO => {
                let preset = if requested_video_quality == VideoQuality::MatchSource {
                    format!(
                        " (~{} approx)",
                        nearest_video_preset(
                            context.encode_context.width,
                            context.encode_context.height,
                            context.encode_context.bit_rate,
                        )
                    )
                } else {
                    String::new()
                };
                if target_video_codec == ffi::AV_CODEC_ID_H264 {
                    info!(
                        "Output video stream {} summary: {}x{} {}{}, bitrate {} bps, profile {}, level {}",
                        context.output_stream_index,
                        context.encode_context.width,
                        context.encode_context.height,
                        describe_codec(target_video_codec),
                        preset,
                        context.encode_context.bit_rate,
                        describe_h264_profile(context.encode_context.profile),
                        describe_h264_level(context.encode_context.level)
                    );
                } else {
                    info!(
                        "Output video stream {} summary: {}x{} {}{}, bitrate {} bps",
                        context.output_stream_index,
                        context.encode_context.width,
                        context.encode_context.height,
                        describe_codec(target_video_codec),
                        preset,
                        context.encode_context.bit_rate
                    );
                }
            }
            ffi::AVMEDIA_TYPE_AUDIO => {
                let preset = if requested_audio_quality == AudioQuality::MatchSource {
                    format!(
                        " (~{} approx)",
                        nearest_audio_preset(context.encode_context.bit_rate)
                    )
                } else {
                    String::new()
                };
                info!(
                    "Output audio stream {} summary: {} ch @ {} Hz {}{}, bitrate {} bps",
                    context.output_stream_index,
                    context.encode_context.ch_layout.nb_channels,
                    context.encode_context.sample_rate,
                    describe_codec(target_audio_codec),
                    preset,
                    context.encode_context.bit_rate
                );
            }
            ffi::AVMEDIA_TYPE_SUBTITLE => {
                info!(
                    "Output subtitle stream {} summary: {}",
                    context.output_stream_index,
                    describe_codec(ffi::AV_CODEC_ID_MOV_TEXT)
                );
            }
            _ => {}
        }
    }
}

/// Runs post-write H.264 profile/level validation when enabled and applicable.
struct H264VerificationRequest<'a> {
    target_video_codec: ffi::AVCodecID,
    skip_codec_check: bool,
    desired_h264_profile: Option<H264Profile>,
    desired_h264_level: Option<H264Level>,
    output_file: &'a CStr,
    output_path: &'a Path,
    last_video_encoder_name: Option<&'a str>,
    hardware_encoder_used: bool,
}

fn verify_h264_output(request: H264VerificationRequest<'_>) -> Result<Option<H264Verification>> {
    if request.target_video_codec != ffi::AV_CODEC_ID_H264 {
        return Ok(None);
    }
    if request.skip_codec_check {
        info!("Skipping H.264 profile/level verification (--skip-codec-check).");
        return Ok(None);
    }
    if let (Some(expected_profile), Some(expected_level)) =
        (request.desired_h264_profile, request.desired_h264_level)
    {
        let verification = verify_output_h264_profile_level(
            request.output_file,
            request.output_path,
            expected_profile,
            expected_level,
            request.last_video_encoder_name,
            request.hardware_encoder_used,
        )?;
        return Ok(Some(verification));
    }
    Ok(None)
}
