use crate::transcoder::prelude::*;

pub(crate) fn run(mut args: Args, matches_snapshot: ArgMatches) -> Result<()> {
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
                    .context("<INPUT_FILE> required for --probe-streams")?;
                let j = gather_streams_info_json(input.as_c_str(), args.streams_filter)?;
                println!("{}", serde_json::to_string_pretty(&j)?);
            }
            OutputFormat::Text => {
                let input = args
                    .input_file
                    .as_ref()
                    .context("<INPUT_FILE> required for --probe-streams")?;
                print_streams_info(input.as_c_str(), args.streams_filter)?;
            }
        }
        return Ok(());
    }

    if let Some(fixture_dir) = args.probe_ocr_fixtures.as_deref() {
        let report =
            crate::subtitle_ocr::evaluate_ocr_fixture_accuracy(fixture_dir, args.ocr_engine)?;
        match args.output {
            OutputFormat::Json => {
                println!("{}", serde_json::to_string_pretty(&report)?);
            }
            OutputFormat::Text => {
                println!(
                    "{}",
                    crate::subtitle_ocr::render_ocr_fixture_report_markdown(&report)
                );
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
            println!("{}", serde_json::to_string_pretty(&summary)?);
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
        .context("INPUT_FILE is required unless using --probe-* flags")?;
    let output_file = args
        .output_file
        .as_deref()
        .context("OUTPUT_FILE is required unless using --probe-* flags")?;
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
        let disable_cuda_pin = should_ocr && ocr_multi_gpu_requested();
        let prev_cuda_pin = if disable_cuda_pin {
            let prev = std::env::var("DPN_DISABLE_CUDA_VISIBLE_DEVICES_PIN").ok();
            std::env::set_var("DPN_DISABLE_CUDA_VISIBLE_DEVICES_PIN", "1");
            Some(prev)
        } else {
            None
        };
        let _conversion_slot = acquire_slot()?;
        let result = convert_video_file(
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
        );
        if let Some(prev) = prev_cuda_pin {
            if let Some(value) = prev {
                std::env::set_var("DPN_DISABLE_CUDA_VISIBLE_DEVICES_PIN", value);
            } else {
                std::env::remove_var("DPN_DISABLE_CUDA_VISIBLE_DEVICES_PIN");
            }
        }
        result
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

fn ocr_multi_gpu_requested() -> bool {
    let Ok(raw) = std::env::var("DPN_OCR_CUDA_DEVICES") else {
        return false;
    };
    let ids = raw
        .split(',')
        .filter_map(|part| part.trim().parse::<i32>().ok())
        .collect::<Vec<_>>();
    ids.len() > 1
}
