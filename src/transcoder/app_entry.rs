//! High-level conversion orchestration from parsed arguments through planning, probing, and execution.

use std::fs;

use anyhow::{anyhow, bail, Context, Result};
use clap::ArgMatches;
use log::{debug, info, warn};
use rsmpeg::ffi;

use crate::config;
use crate::config_merge::apply_config_overrides;
use crate::devices::{self, ContainerFormat, StreamingDevice};
use crate::ffmpeg_utils::{cstr_to_path_buf, path_to_cstring, Args, StreamingDeviceSelection};
use crate::gpu::{gather_probe_json, print_probe, print_probe_codecs};
use crate::logging::log_relevant_env;
use crate::main_probe::{gather_streams_info_json, print_streams_info};
use crate::main_retry::{
    cleanup_partial_output, handle_hw_encoder_init_error, handle_hw_profile_mismatch,
    retry_with_software_encoder,
};
use crate::main_sidecar::{post_process_ocr_subtitles, OcrSidecarRequest};
use crate::plex;
use crate::servarr;
use crate::servarr::{ArgsView as ServeArrArgsView, IntegrationPreparation, ReplacePlan};
use crate::subtitle_ocr;
use crate::throttle::acquire_slot;
use crate::transcoder::app::app_convert::ConversionParams;
use crate::transcoder::convert_video_file;
use crate::transcoder::h264::{DecoderError, HwEncoderInitError, HwProfileLevelMismatch};
use crate::transcoder::helpers::{describe_bitrate, describe_resolution, devices_support_codec};
use crate::transcoder::pipeline::{assess_direct_play_compatibility, DirectPlayConstraints};
use crate::transcoder::quality::{QualityLimits, VideoCodecPreference};
use crate::transcoder::verification::validate_output_file;
use crate::transcoder::ConversionOutcome;
use crate::types::{OcrFormat, OutputFormat};

pub(crate) fn run(mut args: Args, matches_snapshot: ArgMatches) -> Result<()> {
    let plex_refresher = prepare_runtime_configuration(&mut args, &matches_snapshot)?;
    if maybe_handle_probe_modes(&args)? {
        return Ok(());
    }
    if args.servarr_language_audit {
        return run_servarr_language_audit(&args);
    }

    let servarr_preparation = prepare_servarr(&args)?;
    if let IntegrationPreparation::Skip { reason } = &servarr_preparation {
        info!("{}", reason);
        return Ok(());
    }
    log_servarr_context(&servarr_preparation);

    let base_args = args.clone();
    run_batch(base_args, servarr_preparation, plex_refresher)
}

/// Loads optional file config, applies precedence merge, and creates Plex side-effect policy.
fn prepare_runtime_configuration(
    args: &mut Args,
    matches_snapshot: &ArgMatches,
) -> Result<Option<plex::PlexRefresher>> {
    let loaded_config = config::load(args.config_file.as_deref())?;
    log_config_source(&loaded_config);
    if let Some((cfg, _)) = &loaded_config {
        // Apply config only when the user did not set the same option explicitly.
        apply_config_overrides(args, cfg, matches_snapshot);
    }
    let config_plex = loaded_config
        .as_ref()
        .and_then(|(cfg, _)| cfg.plex.as_ref());
    plex::PlexRefresher::from_sources(
        config_plex,
        args.plex_refresh,
        args.plex_url.as_deref(),
        args.plex_token.as_deref(),
    )
}

/// Emits user-visible information about where runtime configuration was loaded from.
fn log_config_source(loaded_config: &Option<(config::Config, config::ConfigSource)>) {
    if loaded_config.is_none() {
        warn!(
            "No direct-play-nice configuration found. Falling back to CLI defaults; set {} or place config.toml under ~/.config/direct-play-nice/ to override.",
            config::CONFIG_ENV_VAR
        );
        return;
    }
    if let Some((_, source)) = loaded_config {
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
}

/// Handles all probe-only command branches. Returns true when execution should stop early.
fn maybe_handle_probe_modes(args: &Args) -> Result<bool> {
    if args.probe_streams {
        handle_probe_streams(args)?;
        return Ok(true);
    }
    if args.probe_ocr_fixtures.is_some() {
        handle_probe_ocr_fixtures(args)?;
        return Ok(true);
    }
    if args.probe_hw || args.probe_codecs {
        handle_probe_hw_codecs(args)?;
        return Ok(true);
    }
    Ok(false)
}

/// Prints stream-level introspection data in user-selected output format.
fn handle_probe_streams(args: &Args) -> Result<()> {
    let input = args
        .input_file
        .as_ref()
        .context("<INPUT_FILE> required for --probe-streams")?;
    match args.output {
        OutputFormat::Json => {
            let j = gather_streams_info_json(input.as_c_str(), args.streams_filter)?;
            println!("{}", serde_json::to_string_pretty(&j)?);
        }
        OutputFormat::Text => {
            print_streams_info(input.as_c_str(), args.streams_filter)?;
        }
    }
    Ok(())
}

/// Runs OCR fixture evaluation and prints a report in text/JSON form.
fn handle_probe_ocr_fixtures(args: &Args) -> Result<()> {
    let fixture_dir = args
        .probe_ocr_fixtures
        .as_deref()
        .context("--probe-ocr-fixtures requires a directory path")?;
    let report = crate::subtitle_ocr::evaluate_ocr_fixture_accuracy(fixture_dir, args.ocr_engine)?;
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
    Ok(())
}

/// Prints hardware and codec inventory probes, optionally in JSON form.
fn handle_probe_hw_codecs(args: &Args) -> Result<()> {
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
    Ok(())
}

fn servarr_untagged_retag_options(args: &Args) -> servarr::UntaggedRetagOptions {
    servarr::UntaggedRetagOptions {
        audio_language: servarr::parse_language_list(
            args.servarr_untagged_audio_language.as_deref(),
        )
        .into_iter()
        .next(),
        subtitle_language: servarr::parse_language_list(
            args.servarr_untagged_subtitle_language.as_deref(),
        )
        .into_iter()
        .next(),
        dry_run: args.servarr_language_dry_run,
    }
}

fn parse_optional_i64_list(raw: Option<&str>) -> Result<Vec<i64>> {
    let Some(raw) = raw else {
        return Ok(Vec::new());
    };
    raw.split([',', ';', '|', ' '])
        .map(str::trim)
        .filter(|part| !part.is_empty())
        .map(|part| {
            part.parse::<i64>()
                .with_context(|| format!("parsing Servarr audit episode id '{part}'"))
        })
        .collect()
}

fn run_servarr_language_audit(args: &Args) -> Result<()> {
    let requirements = servarr::LanguageRequirements {
        enabled: true,
        audio: servarr::parse_language_list(args.required_audio_languages.as_deref()),
        subtitles: servarr::parse_language_list(args.required_subtitle_languages.as_deref()),
    };
    if !requirements.is_effective() {
        bail!("--servarr-language-audit requires --required-audio-languages and/or --required-subtitle-languages");
    }
    let audit_kind = if args.servarr_api_url.as_deref().is_some_and(|url| {
        url.to_ascii_lowercase().contains("7878") || url.to_ascii_lowercase().contains("radarr")
    }) {
        servarr::IntegrationKind::Radarr
    } else {
        servarr::IntegrationKind::Sonarr
    };
    let summary = servarr::run_language_audit(
        audit_kind,
        &servarr::ApiSettings {
            url: args.servarr_api_url.clone(),
            api_key: args.servarr_api_key.clone(),
        },
        &requirements,
        servarr::RedownloadOptions {
            dry_run: args.servarr_language_dry_run,
            candidate_policy: args.servarr_language_candidate_policy,
        },
        servarr::AuditOptions {
            scope: args.servarr_language_audit_scope,
            lookback_days: args.servarr_language_audit_lookback_days,
            max_searches: args.servarr_language_audit_max_searches,
            episode_ids: parse_optional_i64_list(
                args.servarr_language_audit_episode_ids.as_deref(),
            )?,
            untagged_retag: servarr_untagged_retag_options(args),
        },
    )?;
    info!("Servarr language audit summary: {:?}", summary);
    Ok(())
}

/// Computes Servarr integration strategy based on CLI and process environment.
fn prepare_servarr(args: &Args) -> Result<IntegrationPreparation> {
    let servarr_view = ServeArrArgsView {
        has_input: args.input_file.is_some(),
        has_output: args.output_file.is_some(),
        desired_extension: &args.servarr_output_extension,
        desired_suffix: &args.servarr_output_suffix,
        desired_video_quality: args.video_quality,
        language_requirements: servarr::LanguageRequirements {
            enabled: args.servarr_language_check,
            audio: servarr::parse_language_list(args.required_audio_languages.as_deref()),
            subtitles: servarr::parse_language_list(args.required_subtitle_languages.as_deref()),
        },
        untagged_retag: servarr_untagged_retag_options(args),
        api_settings: servarr::ApiSettings {
            url: args.servarr_api_url.clone(),
            api_key: args.servarr_api_key.clone(),
        },
        redownload_options: servarr::RedownloadOptions {
            dry_run: args.servarr_language_dry_run,
            candidate_policy: args.servarr_language_candidate_policy,
        },
    };
    servarr::prepare_from_env(servarr_view)
}

/// Logs Servarr-related environment context for replacement/batch runs.
fn log_servarr_context(servarr_preparation: &IntegrationPreparation) {
    match servarr_preparation {
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
}

/// Converts a Servarr preparation strategy into per-file conversion jobs.
fn prepare_run_queue(servarr_preparation: IntegrationPreparation) -> Vec<Option<ReplacePlan>> {
    match servarr_preparation {
        IntegrationPreparation::None => vec![None],
        IntegrationPreparation::Replace(plan) => vec![Some(plan)],
        IntegrationPreparation::Batch(plans) => plans.into_iter().map(Some).collect(),
        IntegrationPreparation::Skip { .. } => unreachable!(),
    }
}

/// Runs conversion for every queued file replacement plan.
fn run_batch(
    base_args: Args,
    servarr_preparation: IntegrationPreparation,
    plex_refresher: Option<plex::PlexRefresher>,
) -> Result<()> {
    let run_queue = prepare_run_queue(servarr_preparation);
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
    let device_cap = min_resolution.to_dimensions();

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
    info!("Resize quality: {}", args.resize_quality);
    info!("Resize backend: {}", args.resize_backend);
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
    let output_path = cstr_to_path_buf(output_file);
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
        DirectPlayConstraints {
            target_is_mp4,
            sub_mode: args.sub_mode,
            target_video_codec,
            target_audio_codec: common_audio_codec,
            h264_constraints,
            max_fps: min_fps,
            device_cap,
            supported_containers: &common_containers,
            quality_limits: &quality_limits,
            primary_video_stream_index: args.primary_video_stream_index,
            primary_criteria: args.primary_video_criteria,
        },
    ) {
        Ok(assessment) => {
            if assessment.compatible {
                if !should_ocr {
                    info!(
                        "Input is already direct-play compatible for the requested devices; skipping conversion."
                    );
                    return Ok(());
                } else {
                    info!(
                        "Input is direct-play compatible for the requested devices; skipping video/audio transcode but OCR is requested."
                    );
                    needs_conversion = false;
                }
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
        Some(path_to_cstring(&tmp_path)?)
    } else {
        None
    };
    let conversion_output_file = temp_output_cstring.as_deref().unwrap_or(output_file);
    let conversion_params = ConversionParams {
        sub_mode: args.sub_mode,
        target_video_codec,
        target_audio_codec: common_audio_codec,
        h264_constraints,
        min_fps,
        device_max_resolution: min_resolution,
        quality_limits: &quality_limits,
        uv_policy: args.unsupported_video_policy,
        primary_video_stream_index: args.primary_video_stream_index,
        primary_criteria: args.primary_video_criteria,
        requested_video_quality: args.video_quality,
        requested_audio_quality: args.audio_quality,
        resize_quality: args.resize_quality,
        resize_backend: args.resize_backend,
        skip_codec_check: args.skip_codec_check,
        subtitle_failure_policy: args.subtitle_failure_policy,
        hw_accel: args.hw_accel,
    };

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
        let result = convert_video_file(input_file, conversion_output_file, conversion_params);
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
                        conversion_params,
                    ),
                    Err(err1) => match err1.downcast::<HwEncoderInitError>() {
                        Ok(init_err) => handle_hw_encoder_init_error(
                            init_err,
                            &args,
                            input_file,
                            conversion_output_file,
                            conversion_params,
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
                                    conversion_params,
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

    let mut output_ready = conversion_result.is_ok() && needs_conversion && !output_is_mkv;

    if conversion_result.is_ok() && should_ocr {
        let mux_source_file = if needs_conversion {
            conversion_output_file
        } else {
            input_file
        };
        conversion_result = conversion_result.and_then(|outcome| {
            let ocr_output_ready = post_process_ocr_subtitles(OcrSidecarRequest {
                input_file,
                mux_source_file,
                output_file,
                sub_mode: args.sub_mode,
                default_ocr_language: args.ocr_default_language.as_deref(),
                ocr_engine: args.ocr_engine,
                ocr_format: args.ocr_format,
                ocr_external_command: args.ocr_external_command.as_deref(),
                ocr_write_srt_sidecar: args.ocr_write_srt_sidecar,
            })?;
            output_ready = output_ready || ocr_output_ready;
            Ok(outcome)
        });
        if conversion_result.is_ok() && !output_ready && needs_conversion && output_is_mkv {
            subtitle_ocr::remux_copy_streams(conversion_output_file, output_file)?;
            output_ready = true;
        }
    } else if conversion_result.is_ok() && needs_conversion && output_is_mkv {
        subtitle_ocr::remux_copy_streams(conversion_output_file, output_file)?;
        output_ready = true;
    }

    if conversion_result.is_ok() && output_ready && args.validate_output {
        if let Err(err) = validate_output_file(
            input_file,
            output_file,
            target_video_codec,
            common_audio_codec,
        ) {
            conversion_result = Err(err);
        }
    }

    if let Some(tmp_cstr) = temp_output_cstring.as_ref() {
        let tmp_path = cstr_to_path_buf(tmp_cstr);
        if tmp_path != output_path {
            let _ = fs::remove_file(&tmp_path);
        }
    }

    match (plan, conversion_result) {
        (Some(plan), Ok(outcome)) => {
            debug_assert!(outcome.profile_verified());
            if !output_ready {
                info!(
                    "No output was produced for {:?} integration because the input was already compatible; leaving source file '{}' untouched.",
                    plan.kind,
                    plan.input_path.display()
                );
                return Ok(());
            }
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
            if args.delete_source.unwrap_or(false) && output_ready {
                if !outcome.profile_verified() {
                    warn!(
                        "Skipping --delete-source because profile/level verification did not confirm expected constraints"
                    );
                } else if let (Some(input_cstr), Some(output_cstr)) =
                    (args.input_file.as_ref(), args.output_file.as_ref())
                {
                    let input_path = cstr_to_path_buf(input_cstr);
                    let output_path = cstr_to_path_buf(output_cstr);
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
            if output_ready {
                if let Some(ref refresher) = plex_refresher {
                    if let Some(output_cstr) = args.output_file.as_ref() {
                        let output_path = cstr_to_path_buf(output_cstr);
                        if let Err(err) = refresher.refresh_path(&output_path) {
                            warn!(
                                "Plex refresh failed for '{}': {}",
                                output_path.display(),
                                err
                            );
                        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::servarr::IntegrationKind;
    use std::ffi::CString;
    use std::path::PathBuf;

    fn make_plan(tag: &str) -> ReplacePlan {
        let input_path = PathBuf::from(format!("/tmp/{tag}.mkv"));
        let temp_output_path = PathBuf::from(format!("/tmp/{tag}.tmp.mp4"));
        let final_output_path = PathBuf::from(format!("/tmp/{tag}.mp4"));
        let backup_path = PathBuf::from(format!("/tmp/{tag}.bak"));
        ReplacePlan {
            kind: IntegrationKind::Sonarr,
            event_type: "Download".to_string(),
            display_name: Some(tag.to_string()),
            is_upgrade: Some(false),
            input_cstring: CString::new(input_path.to_string_lossy().as_ref()).unwrap(),
            temp_output_cstring: CString::new(temp_output_path.to_string_lossy().as_ref()).unwrap(),
            input_path,
            final_output_path,
            temp_output_path,
            backup_path,
        }
    }

    #[test]
    fn run_queue_none_yields_single_direct_job() {
        let queue = prepare_run_queue(IntegrationPreparation::None);
        assert_eq!(queue.len(), 1);
        assert!(queue[0].is_none());
    }

    #[test]
    fn run_queue_replace_keeps_single_plan() {
        let queue = prepare_run_queue(IntegrationPreparation::Replace(make_plan("single")));
        assert_eq!(queue.len(), 1);
        let Some(plan) = queue[0].as_ref() else {
            panic!("expected replace plan");
        };
        assert_eq!(plan.display_name.as_deref(), Some("single"));
    }

    #[test]
    fn run_queue_batch_preserves_plan_order() {
        let queue = prepare_run_queue(IntegrationPreparation::Batch(vec![
            make_plan("first"),
            make_plan("second"),
        ]));
        assert_eq!(queue.len(), 2);
        let names = queue
            .iter()
            .map(|entry| entry.as_ref().and_then(|plan| plan.display_name.as_deref()))
            .collect::<Vec<_>>();
        assert_eq!(names, vec![Some("first"), Some("second")]);
    }
}
