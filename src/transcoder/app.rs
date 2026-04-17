#[allow(clippy::too_many_arguments)]
pub(crate) fn convert_video_file(
    input_file: &CStr,
    output_file: &CStr,
    sub_mode: SubMode,
    target_video_codec: ffi::AVCodecID,
    target_audio_codec: ffi::AVCodecID,
    h264_constraints: Option<(H264Profile, H264Level)>,
    _min_fps: u32,
    device_max_resolution: Resolution,
    quality_limits: &QualityLimits,
    uv_policy: UnsupportedVideoPolicy,
    primary_video_stream_index: Option<usize>,
    primary_criteria: PrimaryVideoCriteria,
    requested_video_quality: VideoQuality,
    requested_audio_quality: AudioQuality,
    skip_codec_check: bool,
    hw_accel: HwAccel,
) -> Result<ConversionOutcome, anyhow::Error> {
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

    let output_path_str = output_file
        .to_str()
        .map_err(|_| anyhow!("Output path is not valid UTF-8"))?;
    let output_path = Path::new(output_path_str);

    let mut stream_contexts: Vec<StreamProcessingContext> = Vec::new();
    let mut container_duration_us = unsafe { (*input_format_context.as_mut_ptr()).duration };
    if container_duration_us <= 0 {
        container_duration_us = 1;
    }
    let mut progress_tracker = Some(ProgressTracker::new(container_duration_us));

    info!(
        "Target codecs resolved: video={}, audio={}",
        describe_codec(target_video_codec),
        describe_codec(target_audio_codec)
    );

    // START FIX: Determine Constant Quality Mode
    let is_constant_quality_mode = requested_video_quality == VideoQuality::MatchSource;
    // END FIX: Determine Constant Quality Mode

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
    let mut h264_verification: Option<H264Verification> = None;
    let mut last_video_encoder_name: Option<String> = None;
    let mut hardware_encoder_used = false;
    let mut hw_decode_blacklist: HashSet<ffi::AVCodecID> = HashSet::new();

    let primary_index = select_primary_video_stream_index(
        &input_format_context,
        primary_video_stream_index,
        primary_criteria,
    )?;
    let mut _video_streams_seen = 0usize;
    let mut video_streams_added = 0usize;

    for stream in input_format_context.streams() {
        // TODO: ID streams either unsupported in output container type or without a supported decoder and skip them, producing a warning for each skipped.
        let input_codec_type = stream.codecpar().codec_type;
        // TODO: implement support for attachments
        if input_codec_type == ffi::AVMEDIA_TYPE_ATTACHMENT {
            warn!(
                "Skipping attachment stream {} ({}).",
                stream.index,
                unsafe {
                    CStr::from_ptr(ffi::avcodec_get_name(stream.codecpar().codec_id))
                        .to_string_lossy()
                }
            );
            continue;
        }

        if input_codec_type == ffi::AVMEDIA_TYPE_DATA {
            warn!("Skipping data stream {} ({}).", stream.index, unsafe {
                CStr::from_ptr(ffi::avcodec_get_name(stream.codecpar().codec_id)).to_string_lossy()
            });
            continue;
        }

        // Skip attached picture (cover art) streams; treat them like metadata.
        let disposition_flags = unsafe { (*stream.as_ptr()).disposition };
        if (disposition_flags & ffi::AV_DISPOSITION_ATTACHED_PIC as i32) != 0 {
            info!(
                "Skipping attached-picture stream {} ({}).",
                stream.index,
                unsafe {
                    CStr::from_ptr(ffi::avcodec_get_name(stream.codecpar().codec_id))
                        .to_string_lossy()
                }
            );
            continue;
        }

        let input_stream_codecpar = stream.codecpar();
        let input_codec_id = input_stream_codecpar.codec_id;
        let codec_name = unsafe {
            CStr::from_ptr(ffi::avcodec_get_name(input_codec_id))
                .to_string_lossy()
                .into_owned()
        };
        let prefer_hw_decode = allow_cuda_hw_decode
            && shared_hw_device.is_some()
            && input_codec_type == ffi::AVMEDIA_TYPE_VIDEO
            && !hw_decode_blacklist.contains(&input_codec_id);
        let mut decoder = match find_decoder_with_fallback(input_codec_id, prefer_hw_decode) {
            Some(dec) => dec,
            None if input_codec_type == ffi::AVMEDIA_TYPE_SUBTITLE => {
                warn!(
                    "Skipping subtitle stream {} (codec {}): decoder not available.",
                    stream.index, codec_name
                );
                continue;
            }
            None => {
                bail!(
                    "Decoder not found for stream {} (codec {}).",
                    stream.index,
                    codec_name
                );
            }
        };
        let mut decoder_name_owned = decoder.name().to_string_lossy().into_owned();
        debug!(
            "Selected decoder '{}' for stream {} (codec {})",
            decoder_name_owned, stream.index, codec_name
        );
        let mut decode_context = AVCodecContext::new(&decoder);
        decode_context.apply_codecpar(&input_stream_codecpar)?;
        enable_strict_decode_failure(&mut decode_context);
        decode_context.set_time_base(stream.time_base); // TODO: needed?
        if let Some(framerate) = stream.guess_framerate() {
            decode_context.set_framerate(framerate);
        }

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
                        unsafe {
                            let ctx_ptr = decode_context.as_mut_ptr();
                            (*ctx_ptr).hw_device_ctx = ptr::null_mut();
                            (*ctx_ptr).hw_frames_ctx = ptr::null_mut();
                            (*ctx_ptr).get_format = None;
                        }
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
                decode_context = AVCodecContext::new(&decoder);
                decode_context.apply_codecpar(&input_stream_codecpar)?;
                enable_strict_decode_failure(&mut decode_context);
                decode_context.set_time_base(stream.time_base);
                if let Some(framerate) = stream.guess_framerate() {
                    decode_context.set_framerate(framerate);
                }
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
                    decode_context = AVCodecContext::new(&decoder);
                    decode_context.apply_codecpar(&input_stream_codecpar)?;
                    enable_strict_decode_failure(&mut decode_context);
                    decode_context.set_time_base(stream.time_base);
                    if let Some(framerate) = stream.guess_framerate() {
                        decode_context.set_framerate(framerate);
                    }
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
                    stream.index, codec_name, err
                );
                continue;
            } else {
                return Err(anyhow!(
                    "Error opening decoder for stream {} (codec {}): {}",
                    stream.index,
                    codec_name,
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
                    unsafe {
                        CStr::from_ptr(ffi::avcodec_get_name(decode_context.codec_id))
                            .to_string_lossy()
                    }
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
                        AVCodec::find_encoder(target_video_codec).unwrap_or_else(|| {
                            panic!(
                                "Could not find {} encoder",
                                describe_codec(target_video_codec)
                            )
                        }),
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
                    hw_device_ctx_ptr = Some(maybe_hw_dev.unwrap());
                }
                media_type = ffi::AVMEDIA_TYPE_VIDEO;

                let encoder_name_owned = encoder.name().to_string_lossy().into_owned();
                let encoder_key = CString::new("encoder").expect("encoder key CString");
                let encoder_value =
                    CString::new(encoder_name_owned.clone()).expect("encoder value CString");
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
                        min_h264_profile,
                        min_h264_level,
                        quality_limits,
                        device_max_resolution,
                        input_stream_codecpar.bit_rate,
                        &encoder_name_owned,
                        is_constant_quality_mode,
                    );
                } else {
                    set_hevc_video_codec_par(
                        &mut decode_context,
                        &mut encode_context,
                        &mut output_stream,
                        stream,
                        quality_limits,
                        device_max_resolution,
                        input_stream_codecpar.bit_rate,
                        &encoder_name_owned,
                        is_constant_quality_mode,
                    );
                }
                let src_par = stream.codecpar();
                info!(
                    "Video stream {}: {}x{} {} -> {}x{} {}{}",
                    output_stream.index,
                    src_par.width,
                    src_par.height,
                    unsafe {
                        CStr::from_ptr(ffi::avcodec_get_name(src_par.codec_id))
                            .to_str()
                            .unwrap_or("unknown")
                    },
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
                let encoder =
                    AVCodec::find_encoder(target_audio_codec).expect("Could not find AAC encoder");

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
                );
                let src_audio = stream.codecpar();
                let src_audio_channels = src_audio.ch_layout().nb_channels;
                info!(
                    "Audio stream {}: {} ch @ {} Hz {} -> {} ch @ {} Hz {}{}",
                    output_stream.index,
                    src_audio_channels,
                    src_audio.sample_rate,
                    unsafe {
                        CStr::from_ptr(ffi::avcodec_get_name(src_audio.codec_id))
                            .to_str()
                            .unwrap_or("unknown")
                    },
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
                // Initialize the resampler to be able to convert audio sample formats.
                resample_context =
                    init_audio_resampler(&mut decode_context, &mut encode_context).ok();

                // Initialize the FIFO buffer to store audio samples to be encoded.
                frame_buffer = Some(AVAudioFifo::new(
                    encode_context.sample_fmt,
                    encode_context.ch_layout.nb_channels,
                    1,
                ));
            }
            ffi::AVMEDIA_TYPE_SUBTITLE => {
                // TODO: handle cases with no metadata?
                // TODO: always copy metadata or only in some cases?
                output_stream.set_metadata(stream.metadata().as_deref().cloned());

                let encoder = AVCodec::find_encoder(ffi::AV_CODEC_ID_MOV_TEXT)
                    .expect("Could not find MOV_TEXT encoder");
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
                    unsafe {
                        CStr::from_ptr(ffi::avcodec_get_name(stream.codecpar().codec_id))
                            .to_str()
                            .unwrap_or("unknown")
                    },
                    describe_codec(ffi::AV_CODEC_ID_MOV_TEXT)
                );
            }
            // TODO: Handle metadata streams
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
            stream_index: output_stream.index,
            media_type,
            frame_buffer,
            resample_context,
            pts: AtomicI64::new(0),
            last_written_dts: None,
            skip_stream: false,
            hw_device_ctx: hw_device_ctx_ptr,
            decoder_name: decoder.name().to_string_lossy().into_owned(),
        };

        stream_contexts.push(stream_process_context);
    }

    // Write the header of the output file container.
    if let Err(e) = output_format_context.write_header(&mut None) {
        if video_streams_added > 1 {
            bail!(
                "Failed to write container header ({}). The output container may not support multiple video streams. Try --unsupported-video-policy=ignore to drop extra video streams.",
                e
            );
        } else {
            return Err(anyhow!(e)).context("Error writing output file header");
        }
    }

    // Demux streams
    loop {
        let mut packet = match input_format_context.read_packet()? {
            Some(x) => x,
            None => break,
        };

        let Some(stream_processing_context) = stream_contexts
            .iter_mut()
            .find(|context| context.stream_index == packet.stream_index)
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
            ffi::AVMEDIA_TYPE_VIDEO => {
                process_video_stream(
                    stream_processing_context,
                    input_stream,
                    &mut output_format_context,
                    &mut packet,
                    progress_tracker.as_mut(),
                )?;
            }
            ffi::AVMEDIA_TYPE_AUDIO => {
                process_audio_stream(
                    stream_processing_context,
                    input_stream,
                    &mut output_format_context,
                    &mut packet,
                )?;
            }
            ffi::AVMEDIA_TYPE_SUBTITLE => {
                process_subtitle_stream(
                    stream_processing_context,
                    input_stream,
                    &mut output_format_context,
                    &mut packet,
                )?;
            }

            unsupported_type => {
                debug!(
                    "Encountered unsupported stream type ({}). Not setting up Codec.",
                    unsupported_type
                );
            }
        }
    }

    // After processing all packets, flush each encoder
    for context in &mut stream_contexts {
        match context.media_type {
            ffi::AVMEDIA_TYPE_VIDEO => {
                encode_and_write_frame(
                    &mut context.encode_context,
                    &mut output_format_context,
                    context.stream_index as usize,
                    None,
                )
                .context("Failed to flush video encoder.")?;
                if let Some(mut dev) = context.hw_device_ctx {
                    unsafe {
                        ffi::av_buffer_unref(&mut dev);
                    }
                }
            }
            ffi::AVMEDIA_TYPE_AUDIO => {
                encode_and_write_frame(
                    &mut context.encode_context,
                    &mut output_format_context,
                    context.stream_index as usize,
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

    if let Some(progress) = progress_tracker.as_mut() {
        progress.finish();
    }

    for context in &stream_contexts {
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
                        context.stream_index,
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
                        context.stream_index,
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
                    context.stream_index,
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
                    context.stream_index,
                    describe_codec(ffi::AV_CODEC_ID_MOV_TEXT)
                );
            }
            _ => {}
        }
    }

    output_format_context.write_trailer()?;

    if target_video_codec == ffi::AV_CODEC_ID_H264 {
        if skip_codec_check {
            info!("Skipping H.264 profile/level verification (--skip-codec-check).");
        } else if let (Some(expected_profile), Some(expected_level)) =
            (desired_h264_profile, desired_h264_level)
        {
            let verification = verify_output_h264_profile_level(
                output_file,
                output_path,
                expected_profile,
                expected_level,
                last_video_encoder_name.as_deref(),
                hardware_encoder_used,
            )?;
            h264_verification = Some(verification);
        }
    }

    if let Some(mut device) = shared_hw_device {
        unsafe {
            ffi::av_buffer_unref(&mut device);
        }
    }

    Ok(ConversionOutcome { h264_verification })
}

pub(crate) fn run() -> Result<()> {
    if env::var_os("RUST_LOG").is_none() {
        env::set_var("RUST_LOG", "info");
    }
    let _ = env_logger::Builder::from_default_env()
        .format_timestamp(None)
        .target(env_logger::Target::Stderr)
        .try_init();

    configure_ffmpeg_logging();

    let mut matches = Args::command().get_matches();
    let matches_snapshot = matches.clone();
    let mut args = Args::from_arg_matches_mut(&mut matches).expect("Failed to parse CLI arguments");

    let loaded_config = config::load(args.config_file.as_deref())?;
    if loaded_config.is_none() {
        warn!(
            "No direct-play-nice configuration found. Falling back to CLI defaults; set {} or place config.toml under ~/.config/direct-play-nice/ to override.",
            config::CONFIG_ENV_VAR
        );
    }
    if let Some((_, source)) = &loaded_config {
        match source {
            config::ConfigSource::Cli(path) => {
                info!("Loaded configuration from '{}'.", path.display());
            }
            config::ConfigSource::Env(path) => {
                info!(
                    "Loaded configuration from '{}' (via {}).",
                    path.display(),
                    config::CONFIG_ENV_VAR
                );
            }
            config::ConfigSource::Default(path) => {
                info!("Loaded configuration from '{}'.", path.display());
            }
        }
    }
    if let Some((cfg, _)) = &loaded_config {
        apply_config_overrides(&mut args, cfg, &matches_snapshot);
    }
    let config_plex = loaded_config
        .as_ref()
        .and_then(|(cfg, _)| cfg.plex.as_ref());

    let plex_refresher = plex::PlexRefresher::from_sources(
        config_plex,
        args.plex_refresh,
        args.plex_url.as_deref(),
        args.plex_token.as_deref(),
    )?;

    let servarr_view = ServeArrArgsView {
        has_input: args.input_file.is_some(),
        has_output: args.output_file.is_some(),
        desired_extension: &args.servarr_output_extension,
        desired_suffix: &args.servarr_output_suffix,
    };

    let servarr_preparation = servarr::prepare_from_env(servarr_view)?;
    if let IntegrationPreparation::Skip { reason } = &servarr_preparation {
        info!("{}", reason);
        return Ok(());
    }

    match &servarr_preparation {
        IntegrationPreparation::Replace(plan) => {
            log_relevant_env(plan.kind);
        }
        IntegrationPreparation::Batch(plans) => {
            if let Some(first) = plans.first() {
                log_relevant_env(first.kind);
            }
        }
        _ => {}
    }

    // Stream probing early exit
    if args.probe_streams {
        match args.output {
            OutputFormat::Json => {
                let input = args
                    .input_file
                    .as_ref()
                    .expect("<INPUT_FILE> required for --probe-streams");
                let j = gather_streams_info_json(input.as_c_str(), args.streams_filter)?;
                println!("{}", serde_json::to_string_pretty(&j).unwrap());
            }
            OutputFormat::Text => {
                let input = args
                    .input_file
                    .as_ref()
                    .expect("<INPUT_FILE> required for --probe-streams");
                print_streams_info(input.as_c_str(), args.streams_filter)?;
            }
        }
        return Ok(());
    }

    // Additional probe early exits (supports combined --probe-hw --probe-codecs)
    if args.probe_hw || args.probe_codecs {
        let want_json = args.probe_json || matches!(args.output, OutputFormat::Json);
        if want_json {
            let summary = gather_probe_json(
                args.only_video,
                args.only_hw,
                args.probe_hw,
                args.probe_codecs,
            );
            println!("{}", serde_json::to_string_pretty(&summary).unwrap());
        } else {
            if args.probe_hw {
                print_probe();
            }
            if args.probe_codecs {
                print_probe_codecs(args.only_video, args.only_hw);
            }
        }
        return Ok(());
    }
    let base_args = args.clone();
    let run_queue: Vec<Option<ReplacePlan>> = match servarr_preparation {
        IntegrationPreparation::None => vec![None],
        IntegrationPreparation::Replace(plan) => vec![Some(plan)],
        IntegrationPreparation::Batch(plans) => plans.into_iter().map(Some).collect(),
        IntegrationPreparation::Skip { .. } => unreachable!(),
    };

    for plan in run_queue {
        run_conversion(&base_args, plan, &plex_refresher)?;
    }

    Ok(())
}

fn run_conversion(
    base_args: &Args,
    plan: Option<ReplacePlan>,
    plex_refresher: &Option<plex::PlexRefresher>,
) -> Result<()> {
    let mut args = base_args.clone();

    if let Some(ref plan_ref) = plan {
        plan_ref.assign_to_args(&mut args.input_file, &mut args.output_file);
    }

    if args.input_file.is_none() || args.output_file.is_none() {
        bail!(
            "<INPUT_FILE> and <OUTPUT_FILE> are required unless you use --probe-* flags or run inside a Sonarr/Radarr Download event."
        );
    }

    let mut quality_limits = QualityLimits::default();
    quality_limits.apply_video_quality(args.video_quality);
    quality_limits.apply_audio_quality(args.audio_quality);
    if let Some(video_cap) = args.max_video_bitrate {
        quality_limits.max_video_bitrate = Some(video_cap);
    }
    if let Some(audio_cap) = args.max_audio_bitrate {
        quality_limits.max_audio_bitrate = Some(audio_cap);
    }

    debug!(
        "Video quality {}, audio quality {}, caps: resolution={:?}, video={:?} bps, audio={:?} bps",
        args.video_quality,
        args.audio_quality,
        quality_limits.max_video_dimensions,
        quality_limits.max_video_bitrate,
        quality_limits.max_audio_bitrate
    );

    let input_display = args
        .input_file
        .as_ref()
        .map(|c| c.to_string_lossy().into_owned())
        .unwrap_or_else(|| "<unset>".to_string());
    let output_display = args
        .output_file
        .as_ref()
        .map(|c| c.to_string_lossy().into_owned())
        .unwrap_or_else(|| "<unset>".to_string());

    let selections = args
        .streaming_devices
        .take()
        .unwrap_or_else(|| vec![StreamingDeviceSelection::All]);

    let mut streaming_devices: Vec<&StreamingDevice> = if selections
        .iter()
        .any(|selection| matches!(selection, StreamingDeviceSelection::All))
    {
        devices::STREAMING_DEVICES.iter().collect()
    } else {
        selections
            .into_iter()
            .flat_map(|selection| match selection {
                StreamingDeviceSelection::Model(device) => vec![device],
                StreamingDeviceSelection::Family(family) => devices::devices_for_family(family),
                StreamingDeviceSelection::All => Vec::new(),
            })
            .collect()
    };

    streaming_devices.sort_by_key(|device| device.model);
    streaming_devices.dedup_by_key(|device| device.model);

    if streaming_devices.is_empty() {
        bail!("No streaming devices resolved from CLI arguments.");
    }

    let resolved_profile = devices::resolve_target_profile(&streaming_devices)?;
    let common_containers = StreamingDevice::get_common_containers(&streaming_devices)?;

    let target_video_codec = match args.video_codec {
        VideoCodecPreference::Auto => resolved_profile.video_codec,
        VideoCodecPreference::H264 => {
            if devices_support_codec(&streaming_devices, ffi::AV_CODEC_ID_H264) {
                ffi::AV_CODEC_ID_H264
            } else {
                bail!(
                    "Requested video codec H.264 is not supported by all selected streaming devices"
                );
            }
        }
        VideoCodecPreference::Hevc => {
            if devices_support_codec(&streaming_devices, ffi::AV_CODEC_ID_HEVC) {
                ffi::AV_CODEC_ID_HEVC
            } else {
                bail!(
                    "Requested video codec HEVC is not supported by all selected streaming devices"
                );
            }
        }
    };
    let common_audio_codec = resolved_profile.audio_codec;
    let h264_constraints = if target_video_codec == ffi::AV_CODEC_ID_H264 {
        resolved_profile.h264_constraints
    } else {
        None
    };
    let min_fps = resolved_profile.max_fps;
    let min_resolution = resolved_profile.max_resolution;
    let device_cap = resolution_to_dimensions(min_resolution);

    if let Some(device_video_limit) = resolved_profile.max_video_bitrate {
        quality_limits.max_video_bitrate = Some(
            quality_limits
                .max_video_bitrate
                .map(|value| value.min(device_video_limit))
                .unwrap_or(device_video_limit),
        );
    }
    if let Some(device_audio_limit) = resolved_profile.max_audio_bitrate {
        quality_limits.max_audio_bitrate = Some(
            quality_limits
                .max_audio_bitrate
                .map(|value| value.min(device_audio_limit))
                .unwrap_or(device_audio_limit),
        );
    }

    let device_names = streaming_devices
        .iter()
        .map(|device| device.name)
        .collect::<Vec<_>>();

    info!("Converting '{}' -> '{}'", input_display, output_display);
    info!(
        "Target streaming devices ({}): {}",
        device_names.len(),
        device_names.join(", ")
    );
    info!("Hardware acceleration preference: {:?}", args.hw_accel);
    info!(
        "Video quality preset: {} ({}; bitrate {})",
        args.video_quality,
        describe_resolution(quality_limits.max_video_dimensions),
        describe_bitrate(quality_limits.max_video_bitrate)
    );
    info!(
        "Audio quality preset: {} (bitrate {})",
        args.audio_quality,
        describe_bitrate(quality_limits.max_audio_bitrate)
    );
    info!(
        "Common output containers: {}",
        common_containers
            .iter()
            .map(|c| c.as_str())
            .collect::<Vec<_>>()
            .join(", ")
    );
    if let Some((profile, level)) = h264_constraints {
        info!(
            "Device capability ceiling: {}x{}, H.264 profile {:?}, level {:?}",
            device_cap.0, device_cap.1, profile, level
        );
    } else {
        info!(
            "Device capability ceiling: {}x{}",
            device_cap.0, device_cap.1
        );
    }

    let input_file = args
        .input_file
        .as_deref()
        .expect("INPUT_FILE is required unless using --probe-* flags");
    let output_file = args
        .output_file
        .as_deref()
        .expect("OUTPUT_FILE is required unless using --probe-* flags");
    let output_path = PathBuf::from(output_file.to_string_lossy().into_owned());
    let output_extension = output_path
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext.to_ascii_lowercase())
        .unwrap_or_default();
    let requested_container = match output_extension.as_str() {
        "mka" | "mks" => Some(ContainerFormat::Mkv),
        other => ContainerFormat::from_extension(other),
    };
    let requested_container = requested_container.ok_or_else(|| {
        anyhow!(
            "Unsupported output extension '{}'. Supported by selected devices: {}",
            output_extension,
            common_containers
                .iter()
                .map(|container| container.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        )
    })?;
    if !common_containers.contains(&requested_container) {
        bail!(
            "Output container '{}' is not compatible with all selected devices. Supported common containers: {}",
            requested_container.as_str(),
            common_containers
                .iter()
                .map(|container| container.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        );
    }
    let target_is_mp4 = matches!(output_extension.as_str(), "mp4" | "m4v");
    let output_is_mkv = matches!(output_extension.as_str(), "mkv" | "mka" | "mks");
    let should_ocr = target_is_mp4 || matches!(args.ocr_format, OcrFormat::Ass);

    let mut needs_conversion = true;
    match assess_direct_play_compatibility(
        input_file,
        target_is_mp4,
        args.sub_mode,
        target_video_codec,
        common_audio_codec,
        h264_constraints,
        min_fps,
        device_cap,
        &common_containers,
        &quality_limits,
        args.primary_video_stream_index,
        args.primary_video_criteria,
    ) {
        Ok(assessment) => {
            if assessment.compatible {
                if !should_ocr {
                    info!(
                        "Input is already direct-play compatible for the requested devices; skipping conversion."
                    );
                    return Ok(());
                }
                info!(
                    "Input is direct-play compatible for the requested devices; skipping video/audio transcode but OCR is requested."
                );
                needs_conversion = false;
            } else {
                info!("Transcoding required to satisfy requested device constraints.");
                for reason in assessment.reasons {
                    info!("  - {}", reason);
                }
            }
        }
        Err(err) => {
            warn!(
                "Unable to determine direct-play compatibility automatically; proceeding with conversion: {}",
                err
            );
        }
    }

    let temp_output_cstring = if needs_conversion && output_is_mkv {
        let stem = output_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("output");
        let tmp_path = output_path.with_file_name(format!("{stem}.conv.mp4"));
        Some(CString::new(tmp_path.to_string_lossy().to_string())?)
    } else {
        None
    };
    let conversion_output_file = temp_output_cstring.as_deref().unwrap_or(output_file);
    let mut conversion_result = if needs_conversion {
        let _conversion_slot = acquire_slot()?;
        convert_video_file(
            input_file,
            conversion_output_file,
            args.sub_mode,
            target_video_codec,
            common_audio_codec,
            h264_constraints,
            min_fps,
            min_resolution,
            &quality_limits,
            args.unsupported_video_policy,
            args.primary_video_stream_index,
            args.primary_video_criteria,
            args.video_quality,
            args.audio_quality,
            args.skip_codec_check,
            args.hw_accel,
        )
    } else {
        Ok(ConversionOutcome::default())
    };

    if needs_conversion {
        if let Err(err0) = conversion_result {
            if target_video_codec == ffi::AV_CODEC_ID_H264 {
                conversion_result = match err0.downcast::<HwProfileLevelMismatch>() {
                    Ok(mismatch) => handle_hw_profile_mismatch(
                        mismatch,
                        &args,
                        input_file,
                        conversion_output_file,
                        args.sub_mode,
                        target_video_codec,
                        common_audio_codec,
                        h264_constraints,
                        min_fps,
                        min_resolution,
                        &quality_limits,
                    ),
                    Err(err1) => match err1.downcast::<HwEncoderInitError>() {
                        Ok(init_err) => handle_hw_encoder_init_error(
                            init_err,
                            &args,
                            input_file,
                            conversion_output_file,
                            args.sub_mode,
                            target_video_codec,
                            common_audio_codec,
                            h264_constraints,
                            min_fps,
                            min_resolution,
                            &quality_limits,
                        ),
                        Err(err2) => match err2.downcast::<DecoderError>() {
                            Ok(dec_err) => Err(anyhow!(dec_err)),
                            Err(err3) => {
                                warn!(
                                    "NVENC initialization failed ({}); retrying with software encoder",
                                    err3
                                );
                                cleanup_partial_output(conversion_output_file);
                                retry_with_software_encoder(
                                    input_file,
                                    conversion_output_file,
                                    args.sub_mode,
                                    target_video_codec,
                                    common_audio_codec,
                                    h264_constraints,
                                    min_fps,
                                    min_resolution,
                                    &quality_limits,
                                    args.unsupported_video_policy,
                                    args.primary_video_stream_index,
                                    args.primary_video_criteria,
                                    args.video_quality,
                                    args.audio_quality,
                                    args.skip_codec_check,
                                )
                            }
                        },
                    },
                };
            } else {
                conversion_result = Err(err0);
            }
        }
    }

    if conversion_result.is_ok() && should_ocr {
        let mux_source_file = if needs_conversion {
            conversion_output_file
        } else {
            input_file
        };
        conversion_result = conversion_result.and_then(|outcome| {
            post_process_ocr_subtitles(
                input_file,
                mux_source_file,
                output_file,
                args.sub_mode,
                args.ocr_default_language.as_deref(),
                args.ocr_engine,
                args.ocr_format,
                args.ocr_external_command.as_deref(),
                args.ocr_write_srt_sidecar,
            )?;
            Ok(outcome)
        });
    } else if conversion_result.is_ok() && needs_conversion && output_is_mkv {
        subtitle_ocr::remux_copy_streams(conversion_output_file, output_file)?;
    }

    if let Some(tmp_cstr) = temp_output_cstring.as_ref() {
        let tmp_path = PathBuf::from(tmp_cstr.to_string_lossy().into_owned());
        if tmp_path != output_path {
            let _ = fs::remove_file(&tmp_path);
        }
    }

    match (plan, conversion_result) {
        (Some(plan), Ok(outcome)) => {
            debug_assert!(outcome.profile_verified());
            let final_path = plan.finalize_success()?;
            if let Some(ref refresher) = plex_refresher {
                if let Err(err) = refresher.refresh_path(&final_path) {
                    warn!(
                        "Plex refresh failed for '{}': {}",
                        final_path.display(),
                        err
                    );
                }
            }
            Ok(())
        }
        (Some(plan), Err(err)) => {
            if let Err(cleanup_err) = plan.abort_on_failure() {
                warn!(
                    "Failed to clean up after {:?} integration error: {}",
                    plan.kind, cleanup_err
                );
            }
            Err(err)
        }
        (None, Ok(outcome)) => {
            if args.delete_source.unwrap_or(false) {
                if !outcome.profile_verified() {
                    warn!(
                        "Skipping --delete-source because profile/level verification did not confirm expected constraints"
                    );
                } else if let (Some(input_cstr), Some(output_cstr)) =
                    (args.input_file.as_ref(), args.output_file.as_ref())
                {
                    let input_path = PathBuf::from(input_cstr.to_string_lossy().into_owned());
                    let output_path = PathBuf::from(output_cstr.to_string_lossy().into_owned());
                    if input_path != output_path {
                        match fs::remove_file(&input_path) {
                            Ok(_) => info!(
                                "Deleted source file '{}' after successful conversion",
                                input_path.display()
                            ),
                            Err(err) => warn!(
                                "Failed to delete source file '{}': {}",
                                input_path.display(),
                                err
                            ),
                        }
                    } else {
                        warn!(
                            "Skipping --delete-source because input and output paths are identical"
                        );
                    }
                }
            }
            if let Some(ref refresher) = plex_refresher {
                if let Some(output_cstr) = args.output_file.as_ref() {
                    let output_path = PathBuf::from(output_cstr.to_string_lossy().into_owned());
                    if let Err(err) = refresher.refresh_path(&output_path) {
                        warn!(
                            "Plex refresh failed for '{}': {}",
                            output_path.display(),
                            err
                        );
                    }
                }
            }
            Ok(())
        }
        (None, Err(err)) => Err(err),
    }
}
