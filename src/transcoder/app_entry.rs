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
use crate::transcoder::dry_run::{DryRunAction, DryRunReport, SourceHandling};
use crate::transcoder::h264::{DecoderError, HwEncoderInitError, HwProfileLevelMismatch};
use crate::transcoder::helpers::{describe_bitrate, describe_resolution, devices_support_codec};
use crate::transcoder::pipeline::{
    assess_direct_play_compatibility, AudioDecodeErrorLimits, DirectPlayConstraints,
};
use crate::transcoder::quality::{QualityLimits, VideoCodecPreference};
use crate::transcoder::verification::{validate_output_file, OutputValidationOptions};
use crate::transcoder::ConversionOutcome;
use crate::types::{OcrFormat, OutputFormat};

pub(crate) fn run(mut args: Args, matches_snapshot: ArgMatches) -> Result<()> {
    let plex_refresher = prepare_runtime_configuration(&mut args, &matches_snapshot)?;
    if args.servarr_language_proof_title.is_some() || args.servarr_language_proof_file.is_some() {
        if !args.servarr_language_audit {
            bail!("language release proof options require --servarr-language-audit");
        }
        if args.probe_streams
            || args.probe_ocr_fixtures.is_some()
            || args.probe_hw
            || args.probe_codecs
        {
            bail!("language release proof options cannot be combined with probe modes");
        }
    }
    if args.dry_run {
        enable_dry_run(&mut args);
    }
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

/// Turns every write path off for this process: Servarr language actions run in
/// their own dry-run mode, DPN's language cache is read-only, and conversion
/// stops after planning (see `run_conversion`).
fn enable_dry_run(args: &mut Args) {
    args.servarr_language_dry_run = true;
    servarr::disable_cache_writes();
    info!("Dry run enabled: no file will be written, renamed, or deleted.");
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
        args.plex_remove_replaced_versions,
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
    if !requirements.is_effective() && !args.servarr_preserve_original_audio {
        bail!("--servarr-language-audit requires --required-audio-languages and/or --required-subtitle-languages, or --servarr-preserve-original-audio");
    }
    let audit_kind = if args.servarr_api_url.as_deref().is_some_and(|url| {
        url.to_ascii_lowercase().contains("7878") || url.to_ascii_lowercase().contains("radarr")
    }) {
        servarr::IntegrationKind::Radarr
    } else {
        servarr::IntegrationKind::Sonarr
    };
    servarr::set_no_candidate_max_cooldown_days(
        args.servarr_language_audit_no_candidate_max_cooldown_days,
    );
    let summary = servarr::run_language_audit(
        audit_kind,
        &servarr::ApiSettings {
            url: args.servarr_api_url.clone(),
            api_key: args.servarr_api_key.clone(),
            recovery_root: args.servarr_language_recovery_root.clone(),
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
            no_candidate_cooldown_days: args.servarr_language_audit_no_candidate_cooldown_days,
            latest_missing_no_candidate_cooldown_days: args
                .servarr_language_audit_latest_missing_no_candidate_cooldown_days,
            stale_queue_days: args.servarr_language_audit_stale_queue_days,
            stale_queue_max_removals: args.servarr_language_audit_stale_queue_max_removals,
            episode_ids: parse_optional_i64_list(
                args.servarr_language_audit_episode_ids.as_deref(),
            )?,
            untagged_retag: servarr_untagged_retag_options(args),
            proof_title: args.servarr_language_proof_title.clone(),
            proof_file: args.servarr_language_proof_file.clone(),
            preserve_original_audio: args.servarr_preserve_original_audio,
            keep_present_original_audio: args.servarr_keep_present_original_audio,
        },
    )?;
    info!("Servarr language audit summary: {:?}", summary);
    if args.servarr_language_proof_title.is_some() || args.servarr_language_proof_file.is_some() {
        let completed = if args.servarr_language_dry_run {
            summary.dry_run
        } else {
            summary.grabbed
        };
        require_one_shot_outcome(summary.errors, completed)?;
    }
    Ok(())
}

fn require_one_shot_outcome(errors: usize, completed: usize) -> Result<()> {
    if errors != 0 || completed != 1 {
        bail!(
            "proof-backed audit did not complete its requested action (errors={errors}, completed={completed}); inspect the audit log and intent state"
        );
    }
    Ok(())
}

/// Computes Servarr integration strategy based on CLI and process environment.
fn prepare_servarr(args: &Args) -> Result<IntegrationPreparation> {
    let streaming_devices = resolve_streaming_devices(args.streaming_devices.as_deref())?;
    let servarr_view = ServeArrArgsView {
        has_input: args.input_file.is_some(),
        has_output: args.output_file.is_some(),
        output_extension_override: args.servarr_output_extension.as_deref(),
        streaming_devices: &streaming_devices,
        desired_suffix: &args.servarr_output_suffix,
        desired_video_quality: servarr_filename_video_quality(args),
        language_requirements: servarr::LanguageRequirements {
            enabled: args.servarr_language_check,
            audio: servarr::parse_language_list(args.required_audio_languages.as_deref()),
            subtitles: servarr::parse_language_list(args.required_subtitle_languages.as_deref()),
        },
        preserve_original_audio: args.servarr_preserve_original_audio,
        keep_present_original_audio: args.servarr_keep_present_original_audio,
        untagged_retag: servarr_untagged_retag_options(args),
        api_settings: servarr::ApiSettings {
            url: args.servarr_api_url.clone(),
            api_key: args.servarr_api_key.clone(),
            recovery_root: args.servarr_language_recovery_root.clone(),
        },
        redownload_options: servarr::RedownloadOptions {
            dry_run: args.servarr_language_dry_run,
            candidate_policy: args.servarr_language_candidate_policy,
        },
    };
    servarr::prepare_from_env(servarr_view)
}

/// Computes the quality token Servarr replacement paths should use.
///
/// Explicit `--video-quality` values win; otherwise, mirror the effective
/// device-cap quality so downscaled replacements do not keep a misleading
/// source-resolution filename token.
fn servarr_filename_video_quality(args: &Args) -> crate::transcoder::VideoQuality {
    use crate::devices::Resolution;
    use crate::transcoder::VideoQuality;

    if args.video_quality != VideoQuality::MatchSource {
        return args.video_quality;
    }

    let selections = args
        .streaming_devices
        .clone()
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

    let Ok(resolved_profile) = devices::resolve_target_profile(&streaming_devices) else {
        return VideoQuality::MatchSource;
    };

    match resolved_profile.max_resolution {
        Resolution::Resolution480p => VideoQuality::P480,
        Resolution::Resolution720p => VideoQuality::P720,
        Resolution::Resolution1080p => VideoQuality::P1080,
        Resolution::Resolution1440p => VideoQuality::P1440,
        Resolution::Resolution2160p => VideoQuality::P2160,
    }
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

fn resolve_streaming_devices(
    selections: Option<&[StreamingDeviceSelection]>,
) -> Result<Vec<&'static StreamingDevice>> {
    let selections = selections.unwrap_or(&[]);
    let mut resolved: Vec<&StreamingDevice> = if selections.is_empty()
        || selections
            .iter()
            .any(|selection| matches!(selection, StreamingDeviceSelection::All))
    {
        devices::STREAMING_DEVICES.iter().collect()
    } else {
        selections
            .iter()
            .flat_map(|selection| match selection {
                StreamingDeviceSelection::Model(device) => vec![*device],
                StreamingDeviceSelection::Family(family) => devices::devices_for_family(*family),
                StreamingDeviceSelection::All => Vec::new(),
            })
            .collect()
    };

    resolved.sort_by_key(|device| device.model);
    resolved.dedup_by_key(|device| device.model);
    if resolved.is_empty() {
        bail!("No streaming devices resolved from CLI arguments.");
    }
    Ok(resolved)
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
    mut plan: Option<ReplacePlan>,
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

    if args.dry_run {
        if let Some(plan) = plan.as_mut() {
            plan.temp_output_path = planned_staging_path(&plan.final_output_path, "output");
            plan.temp_output_cstring = path_to_cstring(&plan.temp_output_path)?;
            plan.backup_path = plan
                .temp_output_path
                .parent()
                .unwrap()
                .join("source-backup");
            args.output_file = Some(plan.temp_output_cstring.clone());
        }
    }
    if !args.dry_run {
        if let Some(plan) = plan.as_ref() {
            if args.input_file.as_deref() != Some(plan.input_cstring.as_c_str())
                || args.output_file.as_deref() != Some(plan.temp_output_cstring.as_c_str())
            {
                bail!("Servarr input or output path does not match its replacement plan");
            }
        }
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

    let streaming_devices = resolve_streaming_devices(args.streaming_devices.as_deref())?;

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
    let ocr_stream_filter =
        subtitle_ocr::OcrStreamFilter::new(args.skip_redundant_ocr, args.ocr_languages.as_deref());

    // Keep a planned private path for dry-run reporting. Real staging is
    // created only after compatibility assessment determines work is needed.
    let direct_staging = if plan.is_none() {
        Some(DirectStaging {
            final_path: output_path.clone(),
            staged_cstring: path_to_cstring(&planned_staging_path(&output_path, "output"))?,
        })
    } else {
        None
    };
    let (output_file, output_path) = match direct_staging.as_ref() {
        Some(staging) => (
            staging.staged_cstring.as_c_str(),
            cstr_to_path_buf(&staging.staged_cstring),
        ),
        None => (output_file, output_path),
    };

    let mut needs_conversion = true;
    let mut dry_run_action = DryRunAction::Transcode;
    let mut dry_run_reasons = Vec::new();
    match assess_direct_play_compatibility(
        input_file,
        DirectPlayConstraints {
            target_is_mp4,
            sub_mode: args.sub_mode,
            ocr_stream_filter: &ocr_stream_filter,
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
                    if !args.dry_run {
                        return Ok(());
                    }
                    dry_run_action = DryRunAction::Skip;
                } else {
                    info!(
                        "Input is direct-play compatible for the requested devices; skipping video/audio transcode but OCR is requested."
                    );
                    needs_conversion = false;
                    dry_run_action = DryRunAction::RemuxSubtitles;
                }
            } else {
                info!("Transcoding required to satisfy requested device constraints.");
                for reason in &assessment.reasons {
                    info!("  - {}", reason);
                }
                dry_run_reasons = assessment.reasons;
            }
        }
        Err(err) => {
            warn!(
                "Unable to determine direct-play compatibility automatically; proceeding with conversion: {}",
                err
            );
            dry_run_reasons.push(format!(
                "compatibility could not be determined ({err}); conversion would be attempted"
            ));
        }
    }

    // A compatible file with no selected bitmap subtitles needs no output.
    // In particular, do not require the destination directory to be writable
    // just to skip a conversion.
    if !args.dry_run && !needs_conversion && should_ocr {
        let selected_bitmap = if matches!(args.sub_mode, crate::types::SubMode::Skip) {
            false
        } else {
            let input = rsmpeg::avformat::AVFormatContextInput::open(input_file)?;
            let facts = subtitle_ocr::subtitle_stream_facts(&input);
            let skipped = ocr_stream_filter.skipped(&facts);
            facts
                .iter()
                .any(|fact| fact.bitmap && !skipped.contains_key(&fact.index))
        };
        if !selected_bitmap {
            info!(
                "No selected bitmap subtitles require OCR; leaving the compatible input unchanged."
            );
            return Ok(());
        }
    }

    let mut owned_staging = if args.dry_run {
        None
    } else {
        let destination = plan
            .as_ref()
            .map(|p| p.final_output_path.as_path())
            .or_else(|| direct_staging.as_ref().map(|s| s.final_path.as_path()))
            .unwrap_or(output_path.as_path());
        reject_unsafe_destination(&cstr_to_path_buf(input_file), destination, plan.is_some())?;
        Some(OwnedStagingDir::create(destination)?)
    };
    let staged_output = owned_staging
        .as_ref()
        .map(|staging| path_to_cstring(&staging.output_path("output")))
        .transpose()?;
    if let (Some(plan), Some(staging), Some(output)) = (
        plan.as_mut(),
        owned_staging.as_ref(),
        staged_output.as_ref(),
    ) {
        plan.temp_output_path = staging.output_path("output");
        plan.temp_output_cstring = output.clone();
        plan.backup_path = staging.directory.join("source-backup");
    }
    let output_file = staged_output.as_deref().unwrap_or(output_file);
    let output_path = cstr_to_path_buf(output_file);

    let temp_output_cstring = if needs_conversion && output_is_mkv && should_ocr {
        let base_output = plan
            .as_ref()
            .map(|plan| plan.final_output_path.as_path())
            .or_else(|| {
                direct_staging
                    .as_ref()
                    .map(|staging| staging.final_path.as_path())
            })
            .unwrap_or(&output_path);
        let tmp_path = owned_staging
            .as_ref()
            .map(|staging| staging.output_path("conv"))
            .unwrap_or_else(|| planned_staging_path(base_output, "conv"));
        Some(path_to_cstring(&tmp_path)?)
    } else {
        None
    };
    let conversion_output_file = temp_output_cstring.as_deref().unwrap_or(output_file);

    if args.dry_run {
        let report = build_dry_run_report(DryRunInputs {
            args: &args,
            plan: plan.as_ref(),
            action: dry_run_action,
            reasons: dry_run_reasons,
            input_file,
            output_path: &output_path,
            conversion_temp: temp_output_cstring.as_deref().map(cstr_to_path_buf),
            direct_staging: direct_staging.as_ref(),
            device_names: &device_names,
            target_video_codec,
            common_audio_codec,
            container: requested_container,
            should_ocr,
        });
        println!("{}", report.render(args.output));
        return Ok(());
    }

    // All conversion, OCR and verification opens must refer to the same inode,
    // even if Arr replaces the import path while the conversion is running.
    let pinned_input = if let (Some(plan), Some(staging)) = (plan.as_ref(), owned_staging.as_mut())
    {
        Some(path_to_cstring(&staging.pin_source(&plan.input_path)?)?)
    } else {
        None
    };
    let input_file = pinned_input.as_deref().unwrap_or(input_file);

    let conversion_params = ConversionParams {
        sub_mode: args.sub_mode,
        output_is_matroska: output_is_mkv,
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
        audio_decode_error_limits: AudioDecodeErrorLimits {
            policy: args.audio_decode_error_policy,
            max_packets: args.audio_decode_error_max_packets,
            max_ratio: args.audio_decode_error_max_ratio,
        },
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

    let mut output_ready =
        conversion_result.is_ok() && needs_conversion && temp_output_cstring.is_none();

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
                ocr_stream_filter: &ocr_stream_filter,
                subtitle_failure_policy: args.subtitle_failure_policy,
            })?;
            output_ready = output_ready || ocr_output_ready;
            Ok(outcome)
        });
        if conversion_result.is_ok() && !output_ready && needs_conversion && output_is_mkv {
            conversion_result = conversion_result.and_then(|outcome| {
                // Avoid an unnecessary remux when the intermediate contains
                // subtitles or attachments that should remain byte-for-byte intact.
                let intermediate =
                    rsmpeg::avformat::AVFormatContextInput::open(conversion_output_file)?;
                let has_preserved_streams = intermediate.streams().iter().any(|stream| {
                    matches!(
                        stream.codecpar().codec_type,
                        ffi::AVMEDIA_TYPE_SUBTITLE | ffi::AVMEDIA_TYPE_ATTACHMENT
                    )
                });
                drop(intermediate);
                if has_preserved_streams {
                    servarr::promote_staged_output(
                        &cstr_to_path_buf(conversion_output_file),
                        &output_path,
                    )?;
                } else {
                    subtitle_ocr::remux_copy_streams(conversion_output_file, output_file)?;
                }
                output_ready = true;
                Ok(outcome)
            });
        }
    }

    if conversion_result.is_ok() && output_ready && output_is_mkv {
        if let Err(err) = crate::transcoder::verification::validate_matroska_copied_streams(
            input_file,
            output_file,
            !matches!(args.sub_mode, crate::types::SubMode::Skip),
        ) {
            conversion_result = Err(err);
        }
    }

    if conversion_result.is_ok() && output_ready && args.validate_output {
        if let Err(err) = validate_output_file(
            input_file,
            output_file,
            target_video_codec,
            common_audio_codec,
            OutputValidationOptions {
                visual_validate: args.visual_validate_output,
                visual_quality_report: args.visual_quality_report,
                visual_scan_frames: args.visual_scan_frames,
                visual_sample_interval: args.visual_sample_interval,
                visual_failure_ratio: args.visual_failure_ratio,
            },
        ) {
            conversion_result = Err(err);
        }
    }

    if conversion_result.is_ok() && output_ready {
        if let (Some(plan), Some(snapshot)) = (
            plan.as_ref(),
            owned_staging
                .as_ref()
                .and_then(|staging| staging.source_snapshot.as_deref()),
        ) {
            if !same_file_identity(&plan.input_path, snapshot) {
                conversion_result = Err(anyhow!(
                    "Servarr source '{}' changed during conversion; refusing to publish",
                    plan.input_path.display()
                ));
            }
        }
    }

    let mut sidecars = SidecarPublishGuard::default();
    if conversion_result.is_ok() && output_ready && args.ocr_write_srt_sidecar {
        let final_path = plan
            .as_ref()
            .map(|p| p.final_output_path.as_path())
            .or_else(|| direct_staging.as_ref().map(|s| s.final_path.as_path()))
            .unwrap_or(output_path.as_path());
        if let Some(staging) = owned_staging.as_ref() {
            if let Err(err) = sidecars.publish(staging, final_path) {
                conversion_result = Err(err);
            }
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
            let replaced_path = plan.input_path.clone();
            let snapshot = owned_staging
                .as_ref()
                .and_then(|staging| staging.source_snapshot.as_deref())
                .context("Servarr source snapshot is missing")?;
            let final_path = finalize_servarr_no_clobber(plan, snapshot)?;
            sidecars.commit();
            if let Some(ref refresher) = plex_refresher {
                if let Err(err) = refresher.refresh_replaced(&final_path, Some(&replaced_path)) {
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
            let final_output_path = match direct_staging.as_ref() {
                Some(staging) => {
                    if output_ready {
                        publish_no_clobber(&output_path, &staging.final_path)?;
                    } else {
                        let _ = fs::remove_file(&output_path);
                    }
                    staging.final_path.clone()
                }
                None => output_path.clone(),
            };
            if output_ready {
                sidecars.commit();
            }
            let mut deleted_source = None;
            if args.delete_source.unwrap_or(false) && output_ready {
                if !outcome.profile_verified() {
                    warn!(
                        "Skipping --delete-source because profile/level verification did not confirm expected constraints"
                    );
                } else if let Some(input_cstr) = args.input_file.as_ref() {
                    let input_path = cstr_to_path_buf(input_cstr);
                    let output_path = final_output_path.clone();
                    if input_path != output_path {
                        match fs::remove_file(&input_path) {
                            Ok(_) => {
                                info!(
                                    "Deleted source file '{}' after successful conversion",
                                    input_path.display()
                                );
                                deleted_source = Some(input_path);
                            }
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
                    if let Err(err) =
                        refresher.refresh_replaced(&final_output_path, deleted_source.as_deref())
                    {
                        warn!(
                            "Plex refresh failed for '{}': {}",
                            final_output_path.display(),
                            err
                        );
                    }
                }
            }
            Ok(())
        }
        (None, Err(err)) => {
            if direct_staging.is_some() {
                cleanup_partial_output(output_file);
            }
            Err(err)
        }
    }
}

struct OwnedStagingDir {
    directory: std::path::PathBuf,
    extension: std::ffi::OsString,
    source_snapshot: Option<std::path::PathBuf>,
}

impl OwnedStagingDir {
    fn create(final_path: &std::path::Path) -> Result<Self> {
        use std::sync::atomic::{AtomicU64, Ordering};
        static NONCE: AtomicU64 = AtomicU64::new(0);
        let parent = final_path
            .parent()
            .filter(|p| !p.as_os_str().is_empty())
            .unwrap_or_else(|| std::path::Path::new("."));
        let extension = final_path
            .extension()
            .context("Output needs a container extension")?
            .to_os_string();
        for _ in 0..128 {
            let now = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)?
                .as_nanos();
            let name = format!(
                ".direct-play-nice-{}-{now}-{}",
                std::process::id(),
                NONCE.fetch_add(1, Ordering::Relaxed)
            );
            let directory = parent.join(name);
            let mut builder = fs::DirBuilder::new();
            #[cfg(unix)]
            {
                use std::os::unix::fs::DirBuilderExt;
                builder.mode(0o700);
            }
            match builder.create(&directory) {
                Ok(()) => {
                    return Ok(Self {
                        directory,
                        extension,
                        source_snapshot: None,
                    })
                }
                Err(err) if err.kind() == std::io::ErrorKind::AlreadyExists => continue,
                Err(err) => {
                    return Err(err).with_context(|| {
                        format!(
                            "creating private staging directory beside '{}'",
                            final_path.display()
                        )
                    })
                }
            }
        }
        bail!(
            "could not reserve a unique staging directory beside '{}'",
            final_path.display()
        )
    }

    fn output_path(&self, name: &str) -> std::path::PathBuf {
        self.directory.join(name).with_extension(&self.extension)
    }

    fn pin_source(&mut self, source: &std::path::Path) -> Result<std::path::PathBuf> {
        #[cfg(not(any(unix, windows)))]
        bail!("Cannot safely identify a Servarr source on this platform");
        #[cfg(any(unix, windows))]
        {
            let mut snapshot = self.directory.join("source-snapshot");
            if let Some(extension) = source.extension() {
                snapshot.set_extension(extension);
            }
            fs::hard_link(source, &snapshot)
                .with_context(|| format!("pinning Servarr source '{}'", source.display()))?;
            self.source_snapshot = Some(snapshot.clone());
            let metadata = fs::symlink_metadata(&snapshot)?;
            if !metadata.file_type().is_file() || !is_safe_source_file(&metadata) {
                bail!(
                    "Servarr source '{}' is not a safe regular file",
                    source.display()
                );
            }
            #[cfg(unix)]
            let matches = same_file_identity(source, &snapshot);
            #[cfg(windows)]
            let matches = windows_same_file(source, &snapshot, false).with_context(|| {
                format!(
                    "Cannot verify Windows file identity for Servarr source '{}'",
                    source.display()
                )
            })?;
            if !matches {
                bail!(
                    "Servarr source '{}' changed while being pinned",
                    source.display()
                );
            }
            Ok(snapshot)
        }
    }
}

impl Drop for OwnedStagingDir {
    fn drop(&mut self) {
        if let Some(snapshot) = &self.source_snapshot {
            if let Err(err) = fs::remove_file(snapshot) {
                if err.kind() != std::io::ErrorKind::NotFound {
                    warn!(
                        "Unable to remove source snapshot '{}': {}",
                        snapshot.display(),
                        err
                    );
                }
            }
        }
        for name in ["output", "conv", "published"] {
            let path = self.output_path(name);
            for file in [path.clone(), crate::staging::path_for(&path, "ocr")] {
                if let Err(err) = fs::remove_file(&file) {
                    if err.kind() != std::io::ErrorKind::NotFound {
                        warn!(
                            "Unable to remove staging file '{}': {}",
                            file.display(),
                            err
                        );
                    }
                }
            }
        }
        if let Ok(entries) = fs::read_dir(&self.directory) {
            for entry in entries.flatten() {
                let name = entry.file_name();
                if name
                    .to_str()
                    .is_some_and(|s| s.starts_with("output.") && s.ends_with(".srt"))
                    && entry.file_type().is_ok_and(|t| t.is_file())
                {
                    let _ = fs::remove_file(entry.path());
                }
            }
        }
        if let Err(err) = fs::remove_dir(&self.directory) {
            if err.kind() != std::io::ErrorKind::NotFound {
                warn!(
                    "Preserving non-empty staging directory '{}': {}",
                    self.directory.display(),
                    err
                );
            }
        }
    }
}

#[derive(Default)]
struct SidecarPublishGuard {
    linked: Vec<(std::path::PathBuf, std::path::PathBuf)>,
    committed: bool,
}

impl SidecarPublishGuard {
    fn publish(&mut self, staging: &OwnedStagingDir, output: &std::path::Path) -> Result<()> {
        #[cfg(unix)]
        let parent = output
            .parent()
            .filter(|p| !p.as_os_str().is_empty())
            .unwrap_or_else(|| std::path::Path::new("."));
        #[cfg(unix)]
        let stem = output
            .file_stem()
            .context("Output has no filename for OCR sidecars")?;
        #[cfg(not(unix))]
        let _ = output;
        for entry in fs::read_dir(&staging.directory)? {
            let entry = entry?;
            let name = entry.file_name();
            let Some(suffix) = name.to_str().and_then(|name| name.strip_prefix("output.")) else {
                continue;
            };
            if !suffix.ends_with(".srt") {
                continue;
            }
            if !entry.file_type()?.is_file() {
                bail!("OCR sidecar staging path is not a regular file");
            }
            #[cfg(not(unix))]
            bail!("OCR sidecars cannot be safely rolled back on this platform");
            #[cfg(unix)]
            {
                let mut target_name = stem.to_os_string();
                target_name.push(".");
                target_name.push(suffix);
                let target = parent.join(target_name);
                fs::hard_link(entry.path(), &target).with_context(|| {
                    format!(
                        "publishing OCR sidecar '{}' without overwriting",
                        target.display()
                    )
                })?;
                self.linked.push((entry.path(), target));
            }
        }
        Ok(())
    }

    fn commit(&mut self) {
        self.committed = true;
        for (_, destination) in &self.linked {
            info!("Published OCR sidecar '{}'", destination.display());
        }
    }
}

impl Drop for SidecarPublishGuard {
    fn drop(&mut self) {
        if !self.committed {
            for (source, destination) in &self.linked {
                if same_file_identity(source, destination) {
                    if let Err(err) = fs::remove_file(destination) {
                        warn!(
                            "Unable to roll back OCR sidecar '{}': {}",
                            destination.display(),
                            err
                        );
                    }
                }
            }
        }
    }
}

fn planned_staging_path(final_path: &std::path::Path, name: &str) -> std::path::PathBuf {
    let parent = final_path
        .parent()
        .filter(|p| !p.as_os_str().is_empty())
        .unwrap_or_else(|| std::path::Path::new("."));
    let mut path = parent.join(".direct-play-nice-{unique}").join(name);
    if let Some(extension) = final_path.extension() {
        path.set_extension(extension);
    }
    path
}

fn reject_unsafe_destination(
    input: &std::path::Path,
    output: &std::path::Path,
    servarr: bool,
) -> Result<()> {
    match fs::symlink_metadata(output) {
        Ok(_) if servarr && input == output => Ok(()),
        Ok(metadata) if servarr && metadata.file_type().is_dir() => Ok(()),
        Ok(_) if servarr && !same_target_identity(input, output) => Ok(()),
        Ok(_) => bail!(
            "Refusing to overwrite existing output or input alias '{}'",
            output.display()
        ),
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(err) => Err(err).with_context(|| format!("checking output '{}'", output.display())),
    }
}

fn publish_no_clobber(staged: &std::path::Path, output: &std::path::Path) -> Result<()> {
    fs::hard_link(staged, output).with_context(|| {
        format!(
            "could not promote '{}' without overwriting an existing file",
            output.display()
        )
    })?;
    if let Err(err) = fs::remove_file(staged) {
        warn!(
            "Published '{}' but could not remove staging link '{}': {}",
            output.display(),
            staged.display(),
            err
        );
    }
    Ok(())
}

fn finalize_servarr_no_clobber(
    plan: ReplacePlan,
    snapshot: &std::path::Path,
) -> Result<std::path::PathBuf> {
    let input = &plan.input_path;
    if !same_file_identity(input, snapshot) {
        bail!(
            "Servarr source '{}' changed during conversion; refusing to publish",
            input.display()
        );
    }

    // The identity check and rename cannot be atomic against independent writers.
    // Check the moved file as well, and never overwrite a racing replacement when
    // restoring it. A failed restoration leaves the moved file in staging.
    fs::rename(input, &plan.backup_path)
        .with_context(|| format!("moving Servarr source '{}' to backup", input.display()))?;
    let result = (|| -> Result<std::path::PathBuf> {
        if !same_file_identity(&plan.backup_path, snapshot) {
            bail!(
                "Servarr source '{}' changed during finalization; refusing to publish",
                input.display()
            );
        }
        match fs::symlink_metadata(input) {
            Ok(_) => bail!(
                "Servarr source '{}' was replaced during finalization; refusing to publish",
                input.display()
            ),
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => {}
            Err(err) => {
                return Err(err).with_context(|| {
                    format!(
                        "checking Servarr source '{}' before publication",
                        input.display()
                    )
                })
            }
        }
        publish_no_clobber(&plan.temp_output_path, &plan.final_output_path)?;
        Ok(plan.final_output_path.clone())
    })();

    if result.is_ok() {
        if let Err(err) = fs::remove_file(&plan.backup_path) {
            warn!(
                "Retained source backup '{}': {}",
                plan.backup_path.display(),
                err
            );
        }
        return result;
    }

    if fs::hard_link(&plan.backup_path, input).is_ok()
        || same_file_identity(&plan.backup_path, input)
    {
        if let Err(err) = fs::remove_file(&plan.backup_path) {
            warn!(
                "Restored source but retained backup '{}': {}",
                plan.backup_path.display(),
                err
            );
        }
        result
    } else {
        result.with_context(|| {
            format!(
                "moved source remains safely at '{}'",
                plan.backup_path.display()
            )
        })
    }
}

#[cfg(unix)]
fn is_safe_source_file(_metadata: &fs::Metadata) -> bool {
    true
}

#[cfg(windows)]
fn is_safe_source_file(metadata: &fs::Metadata) -> bool {
    use std::os::windows::fs::MetadataExt;
    const FILE_ATTRIBUTE_REPARSE_POINT: u32 = 0x400;
    metadata.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT == 0
}

#[cfg(unix)]
fn same_target_identity(a: &std::path::Path, b: &std::path::Path) -> bool {
    use std::os::unix::fs::MetadataExt;
    match (fs::metadata(a), fs::metadata(b)) {
        (Ok(a), Ok(b)) => a.dev() == b.dev() && a.ino() == b.ino(),
        // If identity cannot be established, reject rather than risk an alias.
        _ => true,
    }
}

#[cfg(windows)]
fn same_target_identity(a: &std::path::Path, b: &std::path::Path) -> bool {
    windows_same_file(a, b, true).unwrap_or(true)
}

#[cfg(not(any(unix, windows)))]
fn same_target_identity(a: &std::path::Path, b: &std::path::Path) -> bool {
    // Conservative on platforms without an available filesystem identity.
    let _ = (a, b);
    true
}

#[cfg(unix)]
fn same_file_identity(a: &std::path::Path, b: &std::path::Path) -> bool {
    use std::os::unix::fs::MetadataExt;
    match (fs::symlink_metadata(a), fs::symlink_metadata(b)) {
        (Ok(a), Ok(b)) => a.dev() == b.dev() && a.ino() == b.ino(),
        _ => false,
    }
}

#[cfg(windows)]
fn same_file_identity(a: &std::path::Path, b: &std::path::Path) -> bool {
    windows_same_file(a, b, false).unwrap_or(false)
}

#[cfg(not(any(unix, windows)))]
fn same_file_identity(_a: &std::path::Path, _b: &std::path::Path) -> bool {
    false
}

#[cfg(windows)]
fn windows_same_file(
    a: &std::path::Path,
    b: &std::path::Path,
    follow: bool,
) -> std::io::Result<bool> {
    use std::os::windows::fs::OpenOptionsExt;
    use std::os::windows::io::AsRawHandle;

    const FILE_ID_INFO_CLASS: i32 = 18;
    const FILE_FLAG_OPEN_REPARSE_POINT: u32 = 0x0020_0000;
    const FILE_FLAG_BACKUP_SEMANTICS: u32 = 0x0200_0000;
    const SHARE_READ_WRITE_DELETE: u32 = 0x7;

    #[repr(C)]
    #[derive(PartialEq, Eq)]
    struct FileIdInfo {
        volume_serial_number: u64,
        file_id: [u8; 16],
    }

    #[link(name = "kernel32")]
    extern "system" {
        fn GetFileInformationByHandleEx(
            file: std::os::windows::io::RawHandle,
            info_class: i32,
            info: *mut std::ffi::c_void,
            size: u32,
        ) -> i32;
    }

    fn open_id(path: &std::path::Path, follow: bool) -> std::io::Result<(fs::File, FileIdInfo)> {
        let flags = FILE_FLAG_BACKUP_SEMANTICS
            | if follow {
                0
            } else {
                FILE_FLAG_OPEN_REPARSE_POINT
            };
        let file = fs::OpenOptions::new()
            .access_mode(0)
            .share_mode(SHARE_READ_WRITE_DELETE)
            .custom_flags(flags)
            .open(path)?;
        let mut info = std::mem::MaybeUninit::<FileIdInfo>::zeroed();
        // The handle remains open during both identity queries and comparison.
        let ok = unsafe {
            GetFileInformationByHandleEx(
                file.as_raw_handle(),
                FILE_ID_INFO_CLASS,
                info.as_mut_ptr().cast(),
                std::mem::size_of::<FileIdInfo>() as u32,
            )
        };
        if ok == 0 {
            return Err(std::io::Error::last_os_error());
        }
        let info = unsafe { info.assume_init() };
        if info.volume_serial_number == 0 || info.file_id == [0; 16] {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "Windows file identity is unavailable",
            ));
        }
        Ok((file, info))
    }

    let (_a_handle, a_id) = open_id(a, follow)?;
    let (_b_handle, b_id) = open_id(b, follow)?;
    Ok(a_id == b_id)
}

/// Staging bookkeeping for direct conversion: the user-facing output path and
/// the temporary path the pipeline actually writes to.
struct DirectStaging {
    final_path: std::path::PathBuf,
    staged_cstring: std::ffi::CString,
}

struct DryRunInputs<'a> {
    args: &'a Args,
    plan: Option<&'a ReplacePlan>,
    action: DryRunAction,
    reasons: Vec<String>,
    input_file: &'a std::ffi::CStr,
    output_path: &'a std::path::Path,
    conversion_temp: Option<std::path::PathBuf>,
    direct_staging: Option<&'a DirectStaging>,
    device_names: &'a [&'a str],
    target_video_codec: ffi::AVCodecID,
    common_audio_codec: ffi::AVCodecID,
    container: ContainerFormat,
    should_ocr: bool,
}

/// Assembles the paths and decisions a real run would act on, without acting on them.
fn build_dry_run_report(inputs: DryRunInputs<'_>) -> DryRunReport {
    use crate::transcoder::helpers::describe_codec;

    let DryRunInputs {
        args,
        plan,
        action,
        reasons,
        input_file,
        output_path,
        conversion_temp,
        direct_staging,
        device_names,
        target_video_codec,
        common_audio_codec,
        container,
        should_ocr,
    } = inputs;

    let mut temp_outputs = Vec::new();
    let (output, backup) = match (plan, direct_staging) {
        (Some(plan), _) => {
            temp_outputs.push(plan.temp_output_path.clone());
            (
                plan.final_output_path.clone(),
                Some(plan.backup_path.clone()),
            )
        }
        (None, Some(staging)) => {
            temp_outputs.push(output_path.to_path_buf());
            (staging.final_path.clone(), None)
        }
        (None, None) => (output_path.to_path_buf(), None),
    };
    if let Some(temp) = conversion_temp {
        temp_outputs.push(temp);
    }

    let source_handling = match (action, plan) {
        (DryRunAction::Skip, _) => SourceHandling::Keep,
        (_, Some(_)) => SourceHandling::ReplaceViaBackup,
        (_, None) if args.delete_source.unwrap_or(false) => SourceHandling::DeleteAfterVerify,
        (_, None) => SourceHandling::Keep,
    };

    DryRunReport {
        mode: DryRunReport::mode_for(plan),
        action,
        input: cstr_to_path_buf(input_file),
        output,
        temp_outputs,
        backup,
        source_handling,
        reasons,
        target_devices: device_names.iter().map(|name| name.to_string()).collect(),
        video_codec: describe_codec(target_video_codec).to_string(),
        audio_codec: describe_codec(common_audio_codec).to_string(),
        container: container.as_str().to_string(),
        subtitle_ocr_pass: should_ocr,
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
    use crate::transcoder::VideoQuality;
    use clap::Parser;
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

    #[cfg(unix)]
    #[test]
    fn servarr_destination_check_rejects_aliases_but_allows_distinct_paths() {
        use std::os::unix::fs::symlink;
        let tmp = tempfile::tempdir().unwrap();
        let source = tmp.path().join("source.mkv");
        let hardlink = tmp.path().join("hardlink.mkv");
        let symbolic = tmp.path().join("symbolic.mkv");
        let unrelated = tmp.path().join("unrelated.mkv");
        fs::write(&source, b"source").unwrap();
        fs::hard_link(&source, &hardlink).unwrap();
        symlink(&source, &symbolic).unwrap();
        fs::write(&unrelated, b"unrelated").unwrap();
        assert!(reject_unsafe_destination(&source, &source, true).is_ok());
        assert!(reject_unsafe_destination(&source, &source, false).is_err());
        assert!(reject_unsafe_destination(&source, &hardlink, true).is_err());
        assert!(reject_unsafe_destination(&source, &symbolic, true).is_err());
        assert!(reject_unsafe_destination(&source, &unrelated, true).is_ok());
        assert!(reject_unsafe_destination(&source, &unrelated, false).is_err());
    }

    #[cfg(unix)]
    #[test]
    fn staged_sidecar_is_rolled_back_if_media_publication_fails() {
        let tmp = tempfile::tempdir().unwrap();
        let output = tmp.path().join("output.mp4");
        let staging = OwnedStagingDir::create(&output).unwrap();
        let staged_sidecar = staging.directory.join("output.eng.srt");
        let sidecar = tmp.path().join("output.eng.srt");
        fs::write(&staged_sidecar, b"subtitle").unwrap();
        let mut guard = SidecarPublishGuard::default();
        guard.publish(&staging, &output).unwrap();
        assert_eq!(fs::read(&sidecar).unwrap(), b"subtitle");
        drop(guard);
        assert!(!sidecar.exists());
        let directory = staging.directory.clone();
        drop(staging);
        assert!(!directory.exists());
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn promotion_never_overwrites_racing_destination_and_restores_source() {
        let tmp = tempfile::tempdir().unwrap();
        let input = tmp.path().join("source.mkv");
        let output = tmp.path().join("result.mkv");
        fs::write(&input, b"source bytes").unwrap();
        let mut staging = OwnedStagingDir::create(&output).unwrap();
        let staged = staging.output_path("output");
        fs::write(&staged, b"converted bytes").unwrap();
        fs::write(&output, b"racing destination").unwrap();
        assert!(publish_no_clobber(&staged, &output).is_err());
        assert_eq!(fs::read(&staged).unwrap(), b"converted bytes");
        assert_eq!(fs::read(&output).unwrap(), b"racing destination");
        let mut plan = make_plan("no-clobber-test");
        plan.input_path = input.clone();
        plan.final_output_path = output.clone();
        plan.temp_output_path = staged;
        plan.backup_path = staging.directory.join("source-backup");
        let snapshot = staging.pin_source(&input).unwrap();
        assert!(finalize_servarr_no_clobber(plan, &snapshot).is_err());
        assert_eq!(fs::read(&input).unwrap(), b"source bytes");
        assert_eq!(fs::read(&output).unwrap(), b"racing destination");
        let directory = staging.directory.clone();
        drop(staging);
        assert!(!directory.exists());
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn servarr_finalization_rejects_source_replaced_after_validation() {
        let tmp = tempfile::tempdir().unwrap();
        let input = tmp.path().join("source.mkv");
        let output = tmp.path().join("result.mkv");
        fs::write(&input, b"original").unwrap();
        let mut staging = OwnedStagingDir::create(&output).unwrap();
        let snapshot = staging.pin_source(&input).unwrap();
        let staged = staging.output_path("output");
        fs::write(&staged, b"converted").unwrap();
        let replacement = tmp.path().join("replacement.mkv");
        fs::write(&replacement, b"replacement").unwrap();
        fs::rename(&replacement, &input).unwrap();
        let mut plan = make_plan("replaced-test");
        plan.input_path = input.clone();
        plan.final_output_path = output.clone();
        plan.temp_output_path = staged;
        plan.backup_path = staging.directory.join("source-backup");
        assert!(finalize_servarr_no_clobber(plan, &snapshot).is_err());
        assert_eq!(fs::read(&input).unwrap(), b"replacement");
        assert!(!output.exists());
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn servarr_publication_failure_restores_original_source() {
        let tmp = tempfile::tempdir().unwrap();
        let input = tmp.path().join("source.mkv");
        let output = tmp.path().join("result.mkv");
        fs::write(&input, b"original").unwrap();
        let mut staging = OwnedStagingDir::create(&output).unwrap();
        let snapshot = staging.pin_source(&input).unwrap();
        let staged = staging.output_path("output");
        fs::write(&staged, b"converted").unwrap();
        let mut plan = make_plan("restore-test");
        plan.input_path = input.clone();
        plan.final_output_path = tmp.path().join("missing").join("result.mkv");
        plan.temp_output_path = staged.clone();
        plan.backup_path = staging.directory.join("source-backup");
        assert!(finalize_servarr_no_clobber(plan, &snapshot).is_err());
        assert_eq!(fs::read(&input).unwrap(), b"original");
        assert_eq!(fs::read(&staged).unwrap(), b"converted");
        assert!(!staging.directory.join("source-backup").exists());
    }

    #[cfg(windows)]
    #[test]
    fn windows_servarr_source_identity_and_successful_finalization() {
        for in_place in [true, false] {
            let tmp = tempfile::tempdir().unwrap();
            let input = tmp.path().join("source.mkv");
            let output = if in_place {
                input.clone()
            } else {
                tmp.path().join("result.mkv")
            };
            let unrelated = tmp.path().join("unrelated.mkv");
            let alias = tmp.path().join("alias.mkv");
            fs::write(&input, b"original").unwrap();
            fs::write(&unrelated, b"original").unwrap();
            fs::hard_link(&input, &alias).unwrap();
            assert!(same_file_identity(&input, &alias));
            assert!(!same_file_identity(&input, &unrelated));
            assert!(!same_file_identity(&input, &tmp.path().join("missing")));
            assert!(reject_unsafe_destination(&input, &alias, true).is_err());
            assert!(reject_unsafe_destination(&input, &unrelated, true).is_ok());

            let mut staging = OwnedStagingDir::create(&output).unwrap();
            let snapshot = staging.pin_source(&input).unwrap();
            assert!(same_file_identity(&input, &snapshot));
            let staged = staging.output_path("output");
            fs::write(&staged, b"converted").unwrap();
            let mut plan = make_plan("windows-success");
            plan.input_path = input.clone();
            plan.final_output_path = output.clone();
            plan.temp_output_path = staged;
            plan.backup_path = staging.directory.join("source-backup");
            assert_eq!(
                finalize_servarr_no_clobber(plan, &snapshot).unwrap(),
                output
            );
            assert_eq!(fs::read(&output).unwrap(), b"converted");
            assert_eq!(fs::read(&alias).unwrap(), b"original");
            if !in_place {
                assert!(!input.exists());
            }
            let directory = staging.directory.clone();
            drop(staging);
            assert!(!directory.exists());
        }
    }

    #[test]
    fn proof_audit_requires_exactly_one_success_and_no_errors() {
        assert!(require_one_shot_outcome(0, 1).is_ok());
        assert!(require_one_shot_outcome(0, 0).is_err());
        assert!(require_one_shot_outcome(1, 1).is_err());
        assert!(require_one_shot_outcome(0, 2).is_err());
    }

    #[test]
    fn servarr_filename_video_quality_uses_all_device_cap_for_match_source() {
        let mut args = Args::parse_from(["direct_play_nice"]);
        args.video_quality = VideoQuality::MatchSource;
        args.streaming_devices = Some(vec![StreamingDeviceSelection::All]);

        assert_eq!(servarr_filename_video_quality(&args), VideoQuality::P720);
    }

    #[test]
    fn servarr_filename_video_quality_prefers_explicit_video_quality() {
        let mut args = Args::parse_from(["direct_play_nice"]);
        args.video_quality = VideoQuality::P720;
        args.streaming_devices = Some(vec![StreamingDeviceSelection::All]);

        assert_eq!(servarr_filename_video_quality(&args), VideoQuality::P720);
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
