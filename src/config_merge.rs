//! Configuration merge policy between parsed CLI arguments and file-based defaults.
//!
//! This module exists to keep `Args` parsing focused on syntax/validation while
//! preserving all precedence rules in one place.

use clap::parser::ValueSource;
use clap::ArgMatches;
use log::warn;

use crate::{config, Args};

fn cli_value_provided(matches: &ArgMatches, id: &str) -> bool {
    matches
        .value_source(id)
        .is_some_and(|source| source != ValueSource::DefaultValue)
}

/// Applies config values to `args` only when the same field was not set by CLI.
///
/// The merge rule is strict: explicit user CLI input always wins.
pub(crate) fn apply_config_overrides(args: &mut Args, cfg: &config::Config, matches: &ArgMatches) {
    if args.streaming_devices.is_none() {
        if let Some(devices) = cfg.streaming_devices.as_ref() {
            let raw_values: Vec<String> = match devices {
                config::StreamingDevicesSetting::Single(value) => value
                    .split(',')
                    .map(str::trim)
                    .filter(|entry| !entry.is_empty())
                    .map(|s| s.to_string())
                    .collect(),
                config::StreamingDevicesSetting::List(values) => values
                    .iter()
                    .flat_map(|value| value.split(','))
                    .map(str::trim)
                    .filter(|entry| !entry.is_empty())
                    .map(|s| s.to_string())
                    .collect(),
            };

            let selections: std::result::Result<Vec<_>, _> = raw_values
                .iter()
                .map(|entry| Args::parse_device_selection(entry))
                .collect();
            match selections {
                Ok(list) if !list.is_empty() => args.streaming_devices = Some(list),
                Ok(_) => {}
                Err(err) => warn!("Failed to parse config streaming_devices: {}", err),
            }
        }
    }

    if args.max_video_bitrate.is_none() && !cli_value_provided(matches, "max_video_bitrate") {
        if let Some(bitrate) = cfg.max_video_bitrate.as_deref() {
            match Args::parse_bitrate(bitrate) {
                Ok(bps) => args.max_video_bitrate = Some(bps),
                Err(err) => warn!(
                    "Failed to parse config max_video_bitrate='{}': {}",
                    bitrate, err
                ),
            }
        }
    }

    if args.max_audio_bitrate.is_none() && !cli_value_provided(matches, "max_audio_bitrate") {
        if let Some(bitrate) = cfg.max_audio_bitrate.as_deref() {
            match Args::parse_bitrate(bitrate) {
                Ok(bps) => args.max_audio_bitrate = Some(bps),
                Err(err) => warn!(
                    "Failed to parse config max_audio_bitrate='{}': {}",
                    bitrate, err
                ),
            }
        }
    }

    if !cli_value_provided(matches, "video_quality") {
        if let Some(video_quality) = cfg.video_quality {
            args.video_quality = video_quality;
        }
    }

    if !cli_value_provided(matches, "video_codec") {
        if let Some(video_codec) = cfg.video_codec {
            args.video_codec = video_codec;
        }
    }

    if !cli_value_provided(matches, "audio_quality") {
        if let Some(audio_quality) = cfg.audio_quality {
            args.audio_quality = audio_quality;
        }
    }

    if !cli_value_provided(matches, "resize_quality") {
        if let Some(resize_quality) = cfg.resize_quality {
            args.resize_quality = resize_quality;
        }
    }

    if !cli_value_provided(matches, "resize_backend") {
        if let Some(resize_backend) = cfg.resize_backend {
            args.resize_backend = resize_backend;
        }
    }

    if !cli_value_provided(matches, "hw_accel") {
        if let Some(hw_accel) = cfg.hw_accel {
            args.hw_accel = hw_accel;
        }
    }

    if !cli_value_provided(matches, "unsupported_video_policy") {
        if let Some(policy) = cfg.unsupported_video_policy {
            args.unsupported_video_policy = policy;
        }
    }

    if !cli_value_provided(matches, "primary_video_stream_index")
        && cfg.primary_video_stream_index.is_some()
    {
        args.primary_video_stream_index = cfg.primary_video_stream_index;
    }

    if !cli_value_provided(matches, "primary_video_criteria") {
        if let Some(criteria) = cfg.primary_video_criteria {
            args.primary_video_criteria = criteria;
        }
    }

    if !cli_value_provided(matches, "servarr_output_extension") {
        if let Some(ext) = cfg.servarr_output_extension.as_ref() {
            args.servarr_output_extension = ext.clone();
        }
    }

    if !cli_value_provided(matches, "servarr_output_suffix") {
        if let Some(suffix) = cfg.servarr_output_suffix.as_ref() {
            args.servarr_output_suffix = suffix.clone();
        }
    }

    if !cli_value_provided(matches, "servarr_language_audit") {
        if let Some(enabled) = cfg.servarr_language_audit {
            args.servarr_language_audit = enabled;
        }
    }

    if !cli_value_provided(matches, "servarr_language_audit_scope") {
        if let Some(scope) = cfg.servarr_language_audit_scope {
            args.servarr_language_audit_scope = scope;
        }
    }

    if !cli_value_provided(matches, "servarr_language_audit_lookback_days") {
        if let Some(days) = cfg.servarr_language_audit_lookback_days {
            args.servarr_language_audit_lookback_days = days;
        }
    }

    if !cli_value_provided(matches, "servarr_language_audit_max_searches") {
        if let Some(max_searches) = cfg.servarr_language_audit_max_searches {
            args.servarr_language_audit_max_searches = max_searches;
        }
    }

    if !cli_value_provided(matches, "servarr_language_audit_episode_ids") {
        if let Some(ids) = cfg.servarr_language_audit_episode_ids.as_ref() {
            args.servarr_language_audit_episode_ids = Some(ids.clone());
        }
    }

    if !cli_value_provided(matches, "servarr_language_check") {
        if let Some(enabled) = cfg.servarr_language_check {
            args.servarr_language_check = enabled;
        }
    }

    if !cli_value_provided(matches, "required_audio_languages") {
        if let Some(languages) = cfg.required_audio_languages.as_ref() {
            args.required_audio_languages = Some(languages.clone());
        }
    }

    if !cli_value_provided(matches, "required_subtitle_languages") {
        if let Some(languages) = cfg.required_subtitle_languages.as_ref() {
            args.required_subtitle_languages = Some(languages.clone());
        }
    }

    if !cli_value_provided(matches, "servarr_api_url") {
        if let Some(url) = cfg.servarr_api_url.as_ref() {
            args.servarr_api_url = Some(url.clone());
        }
    }

    if !cli_value_provided(matches, "servarr_api_key") {
        if let Some(api_key) = cfg.servarr_api_key.as_ref() {
            args.servarr_api_key = Some(api_key.clone());
        }
    }

    if !cli_value_provided(matches, "servarr_language_dry_run") {
        if let Some(dry_run) = cfg.servarr_language_dry_run {
            args.servarr_language_dry_run = dry_run;
        }
    }

    if !cli_value_provided(matches, "servarr_untagged_audio_language") {
        if let Some(language) = cfg.servarr_untagged_audio_language.as_ref() {
            args.servarr_untagged_audio_language = Some(language.clone());
        }
    }

    if !cli_value_provided(matches, "servarr_untagged_subtitle_language") {
        if let Some(language) = cfg.servarr_untagged_subtitle_language.as_ref() {
            args.servarr_untagged_subtitle_language = Some(language.clone());
        }
    }

    if !cli_value_provided(matches, "servarr_language_candidate_policy") {
        if let Some(policy) = cfg.servarr_language_candidate_policy {
            args.servarr_language_candidate_policy = policy;
        }
    }

    if !cli_value_provided(matches, "sub_mode") {
        if let Some(sub_mode) = cfg.sub_mode {
            args.sub_mode = sub_mode;
        }
    }

    if !cli_value_provided(matches, "subtitle_failure_policy") {
        if let Some(policy) = cfg.subtitle_failure_policy {
            args.subtitle_failure_policy = policy;
        }
    }

    if !cli_value_provided(matches, "ocr_default_language") {
        if let Some(default_language) = cfg.ocr_default_language.as_ref() {
            args.ocr_default_language = Some(default_language.clone());
        }
    }

    if !cli_value_provided(matches, "ocr_engine") {
        if let Some(ocr_engine) = cfg.ocr_engine {
            args.ocr_engine = ocr_engine;
        }
    }

    if !cli_value_provided(matches, "ocr_format") {
        if let Some(ocr_format) = cfg.ocr_format {
            args.ocr_format = ocr_format;
        }
    }

    if !cli_value_provided(matches, "ocr_preprocess") {
        if let Some(ocr_preprocess) = cfg.ocr_preprocess {
            args.ocr_preprocess = ocr_preprocess;
        }
    }

    if !cli_value_provided(matches, "ocr_external_command") {
        if let Some(ocr_external_command) = cfg.ocr_external_command.as_ref() {
            args.ocr_external_command = Some(ocr_external_command.clone());
        }
    }

    if !cli_value_provided(matches, "ocr_write_srt_sidecar") {
        if let Some(ocr_write_srt_sidecar) = cfg.ocr_write_srt_sidecar {
            args.ocr_write_srt_sidecar = ocr_write_srt_sidecar;
        }
    }

    if !cli_value_provided(matches, "skip_codec_check") {
        if let Some(skip_codec_check) = cfg.skip_codec_check {
            args.skip_codec_check = skip_codec_check;
        }
    }

    if args.delete_source.is_none() {
        if let Some(delete_source) = cfg.delete_source {
            args.delete_source = Some(delete_source);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::transcoder::quality::VideoQuality;
    use crate::{OcrPreprocess, ResizeBackend, ResizeQuality, SubtitleFailurePolicy};
    use clap::{CommandFactory, FromArgMatches};

    fn parse_args(argv: &[&str]) -> (Args, ArgMatches) {
        let mut matches = Args::command().get_matches_from(argv);
        let snapshot = matches.clone();
        let args = Args::from_arg_matches_mut(&mut matches).unwrap();
        (args, snapshot)
    }

    #[test]
    fn applies_video_quality_from_config_when_not_set_in_cli() {
        let (mut args, matches) = parse_args(&["direct_play_nice"]);
        let cfg = config::Config {
            video_quality: Some(VideoQuality::P720),
            ..Default::default()
        };
        apply_config_overrides(&mut args, &cfg, &matches);
        assert_eq!(args.video_quality, VideoQuality::P720);
    }

    #[test]
    fn cli_video_quality_takes_precedence_over_config() {
        let (mut args, matches) = parse_args(&["direct_play_nice", "--video-quality", "1080p"]);
        let cfg = config::Config {
            video_quality: Some(VideoQuality::P720),
            ..Default::default()
        };
        apply_config_overrides(&mut args, &cfg, &matches);
        assert_eq!(args.video_quality, VideoQuality::P1080);
    }

    #[test]
    fn applies_ocr_preprocess_from_config_when_not_set_in_cli() {
        let (mut args, matches) = parse_args(&["direct_play_nice"]);
        let cfg = config::Config {
            ocr_preprocess: Some(OcrPreprocess::OpenCvBasic),
            ..Default::default()
        };
        apply_config_overrides(&mut args, &cfg, &matches);
        assert_eq!(args.ocr_preprocess, OcrPreprocess::OpenCvBasic);
    }

    #[test]
    fn cli_ocr_preprocess_takes_precedence_over_config() {
        let (mut args, matches) =
            parse_args(&["direct_play_nice", "--ocr-preprocess", "open-cv-subtitle"]);
        let cfg = config::Config {
            ocr_preprocess: Some(OcrPreprocess::OpenCvBasic),
            ..Default::default()
        };
        apply_config_overrides(&mut args, &cfg, &matches);
        assert_eq!(args.ocr_preprocess, OcrPreprocess::OpenCvSubtitle);
    }

    #[test]
    fn applies_servarr_language_settings_from_config_when_not_set_in_cli() {
        let (mut args, matches) = parse_args(&["direct_play_nice"]);
        let cfg = config::Config {
            servarr_language_audit: Some(true),
            servarr_language_audit_scope: Some(crate::ServarrLanguageAuditScope::Inventory),
            servarr_language_audit_lookback_days: Some(30),
            servarr_language_audit_max_searches: Some(20),
            servarr_language_audit_episode_ids: Some("77,88".to_string()),
            servarr_language_check: Some(true),
            required_audio_languages: Some("eng,jpn".to_string()),
            required_subtitle_languages: Some("eng".to_string()),
            servarr_api_url: Some("http://localhost:8989".to_string()),
            servarr_api_key: Some("secret".to_string()),
            servarr_language_dry_run: Some(true),
            servarr_untagged_audio_language: Some("eng".to_string()),
            servarr_untagged_subtitle_language: Some("eng".to_string()),
            servarr_language_candidate_policy: Some(
                crate::ServarrLanguageCandidatePolicy::CustomFormatOrTitle,
            ),
            ..Default::default()
        };
        apply_config_overrides(&mut args, &cfg, &matches);
        assert!(args.servarr_language_audit);
        assert_eq!(
            args.servarr_language_audit_scope,
            crate::ServarrLanguageAuditScope::Inventory
        );
        assert_eq!(args.servarr_language_audit_lookback_days, 30);
        assert_eq!(args.servarr_language_audit_max_searches, 20);
        assert_eq!(
            args.servarr_language_audit_episode_ids.as_deref(),
            Some("77,88")
        );
        assert!(args.servarr_language_check);
        assert_eq!(args.required_audio_languages.as_deref(), Some("eng,jpn"));
        assert_eq!(args.required_subtitle_languages.as_deref(), Some("eng"));
        assert_eq!(
            args.servarr_api_url.as_deref(),
            Some("http://localhost:8989")
        );
        assert_eq!(args.servarr_api_key.as_deref(), Some("secret"));
        assert!(args.servarr_language_dry_run);
        assert_eq!(args.servarr_untagged_audio_language.as_deref(), Some("eng"));
        assert_eq!(
            args.servarr_untagged_subtitle_language.as_deref(),
            Some("eng")
        );
        assert_eq!(
            args.servarr_language_candidate_policy,
            crate::ServarrLanguageCandidatePolicy::CustomFormatOrTitle
        );
    }

    #[test]
    fn cli_servarr_language_settings_take_precedence_over_config() {
        let (mut args, matches) = parse_args(&[
            "direct_play_nice",
            "--servarr-language-check",
            "--required-audio-languages",
            "eng",
        ]);
        let cfg = config::Config {
            servarr_language_check: Some(false),
            required_audio_languages: Some("jpn".to_string()),
            ..Default::default()
        };
        apply_config_overrides(&mut args, &cfg, &matches);
        assert!(args.servarr_language_check);
        assert_eq!(args.required_audio_languages.as_deref(), Some("eng"));
    }

    #[test]
    fn applies_subtitle_failure_policy_from_config_when_not_set_in_cli() {
        let (mut args, matches) = parse_args(&["direct_play_nice"]);
        let cfg = config::Config {
            subtitle_failure_policy: Some(SubtitleFailurePolicy::Fail),
            ..Default::default()
        };
        apply_config_overrides(&mut args, &cfg, &matches);
        assert_eq!(args.subtitle_failure_policy, SubtitleFailurePolicy::Fail);
    }

    #[test]
    fn cli_subtitle_failure_policy_takes_precedence_over_config() {
        let (mut args, matches) = parse_args(&[
            "direct_play_nice",
            "--subtitle-failure-policy",
            "skip-stream",
        ]);
        let cfg = config::Config {
            subtitle_failure_policy: Some(SubtitleFailurePolicy::Fail),
            ..Default::default()
        };
        apply_config_overrides(&mut args, &cfg, &matches);
        assert_eq!(
            args.subtitle_failure_policy,
            SubtitleFailurePolicy::SkipStream
        );
    }

    #[test]
    fn applies_resize_quality_from_config_when_not_set_in_cli() {
        let (mut args, matches) = parse_args(&["direct_play_nice"]);
        let cfg = config::Config {
            resize_quality: Some(ResizeQuality::Bicubic),
            ..Default::default()
        };
        apply_config_overrides(&mut args, &cfg, &matches);
        assert_eq!(args.resize_quality, ResizeQuality::Bicubic);
    }

    #[test]
    fn cli_resize_quality_takes_precedence_over_config() {
        let (mut args, matches) = parse_args(&["direct_play_nice", "--resize-quality", "spline"]);
        let cfg = config::Config {
            resize_quality: Some(ResizeQuality::Lanczos),
            ..Default::default()
        };
        apply_config_overrides(&mut args, &cfg, &matches);
        assert_eq!(args.resize_quality, ResizeQuality::Spline);
    }

    #[test]
    fn applies_resize_backend_from_config_when_not_set_in_cli() {
        let (mut args, matches) = parse_args(&["direct_play_nice"]);
        let cfg = config::Config {
            resize_backend: Some(ResizeBackend::Cuda),
            ..Default::default()
        };
        apply_config_overrides(&mut args, &cfg, &matches);
        assert_eq!(args.resize_backend, ResizeBackend::Cuda);
    }

    #[test]
    fn cli_resize_backend_takes_precedence_over_config() {
        let (mut args, matches) = parse_args(&["direct_play_nice", "--resize-backend", "software"]);
        let cfg = config::Config {
            resize_backend: Some(ResizeBackend::Cuda),
            ..Default::default()
        };
        apply_config_overrides(&mut args, &cfg, &matches);
        assert_eq!(args.resize_backend, ResizeBackend::Software);
    }
}
