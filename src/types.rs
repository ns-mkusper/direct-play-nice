use super::*;

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum, Deserialize)]
#[serde(rename_all = "lowercase")]
pub(crate) enum UnsupportedVideoPolicy {
    Convert,
    Ignore,
    Fail,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum, Deserialize)]
#[serde(rename_all = "lowercase")]
pub(crate) enum SubMode {
    Auto,
    Force,
    Skip,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum, Deserialize)]
#[serde(rename_all = "lowercase")]
pub(crate) enum OcrEngine {
    Auto,
    Tesseract,
    PpOcrV3,
    PpOcrV4,
    External,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum, Deserialize)]
#[serde(rename_all = "lowercase")]
pub(crate) enum OcrFormat {
    Srt,
    Ass,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum, Deserialize)]
#[serde(rename_all = "lowercase")]
pub(crate) enum PrimaryVideoCriteria {
    Resolution,
    Bitrate,
    Fps,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum)]
pub(crate) enum OutputFormat {
    Text,
    Json,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum)]
pub(crate) enum StreamsFilter {
    All,
    Video,
    Audio,
    Subtitle,
}

pub(crate) fn derive_target_bitrate(source: i64, limit: Option<i64>) -> Option<i64> {
    match (limit, source) {
        (Some(limit), source_value) if limit > 0 => {
            if source_value > 0 {
                Some(std::cmp::min(source_value, limit))
            } else {
                Some(limit)
            }
        }
        (None, source_value) if source_value > 0 => Some(source_value),
        _ => None,
    }
}

pub(crate) fn resolution_to_dimensions(resolution: Resolution) -> (u32, u32) {
    match resolution {
        Resolution::Resolution480p => (640, 480),
        Resolution::Resolution720p => (1280, 720),
        Resolution::Resolution1080p => (1920, 1080),
        Resolution::Resolution1440p => (2560, 1440),
        Resolution::Resolution2160p => (3840, 2160),
    }
}

pub(crate) fn clamp_dimensions(
    source_width: i32,
    source_height: i32,
    device_cap: (u32, u32),
    quality_cap: Option<(u32, u32)>,
) -> (i32, i32) {
    if source_width <= 0 || source_height <= 0 {
        return (source_width.max(1), source_height.max(1));
    }

    let mut max_width = device_cap.0.max(2);
    let mut max_height = device_cap.1.max(2);

    if let Some((quality_width, quality_height)) = quality_cap {
        max_width = max_width.min(quality_width.max(2));
        max_height = max_height.min(quality_height.max(2));
    }

    if (source_width as u32) <= max_width && (source_height as u32) <= max_height {
        return (source_width, source_height);
    }

    let width_ratio = max_width as f64 / source_width as f64;
    let height_ratio = max_height as f64 / source_height as f64;
    let scale = width_ratio.min(height_ratio);

    let mut target_width = (source_width as f64 * scale).round() as i32;
    let mut target_height = (source_height as f64 * scale).round() as i32;

    if target_width < 2 {
        target_width = 2;
    }
    if target_height < 2 {
        target_height = 2;
    }

    if source_width >= 2 {
        target_width = target_width.min(source_width);
    } else {
        target_width = source_width;
    }

    if source_height >= 2 {
        target_height = target_height.min(source_height);
    } else {
        target_height = source_height;
    }

    if target_width % 2 != 0 {
        target_width = (target_width - 1).max(2);
    }
    if target_height % 2 != 0 {
        target_height = (target_height - 1).max(2);
    }

    const WIDTH_ALIGNMENT: i32 = 16;
    if target_width >= WIDTH_ALIGNMENT && target_width % WIDTH_ALIGNMENT != 0 {
        let aspect_ratio = source_width as f64 / source_height as f64;
        let max_width_i32 = max_width as i32;
        let max_height_i32 = max_height as i32;

        let mut aligned_width = ((target_width / WIDTH_ALIGNMENT).max(1)) * WIDTH_ALIGNMENT;
        if aligned_width > max_width_i32 {
            aligned_width = (max_width_i32 / WIDTH_ALIGNMENT).max(1) * WIDTH_ALIGNMENT;
        }

        while aligned_width >= WIDTH_ALIGNMENT {
            let mut aligned_height = ((aligned_width as f64 / aspect_ratio).round() as i32).max(2);
            if aligned_height % 2 != 0 {
                aligned_height += 1;
            }
            if aligned_height <= max_height_i32 {
                target_width = aligned_width;
                target_height = aligned_height.max(2);
                break;
            }
            aligned_width -= WIDTH_ALIGNMENT;
        }
    }

    (target_width, target_height)
}

pub(crate) fn default_video_bitrate(width: i32, height: i32) -> i64 {
    let max_dim = width.max(height).max(1) as u32;
    match max_dim {
        d if d <= 640 => 1_200_000,
        d if d <= 854 => 2_500_000,
        d if d <= 1280 => 5_000_000,
        d if d <= 1920 => 8_000_000,
        d if d <= 2560 => 16_000_000,
        _ => 35_000_000,
    }
}

pub(crate) fn nearest_video_preset(width: i32, height: i32, bitrate: i64) -> &'static str {
    let max_dim = width.max(height).max(1);
    let bitrate = if bitrate > 0 {
        bitrate
    } else {
        default_video_bitrate(width, height)
    };
    match max_dim {
        d if d <= 360 => "360p",
        d if d <= 480 => "480p",
        d if d <= 720 => "720p",
        d if d <= 1080 => "1080p",
        d if d <= 1440 => "1440p",
        _ => {
            if bitrate >= 30_000_000 {
                "2160p"
            } else {
                "1440p"
            }
        }
    }
}

pub(crate) fn nearest_audio_preset(bitrate: i64) -> &'static str {
    let bitrate = if bitrate > 0 { bitrate } else { 192_000 };
    const PRESETS: &[(i64, &str)] = &[
        (96_000, "96k"),
        (128_000, "128k"),
        (160_000, "160k"),
        (192_000, "192k"),
        (224_000, "224k"),
        (256_000, "256k"),
        (320_000, "320k"),
    ];
    let mut best = PRESETS[0];
    let mut best_diff = (bitrate - PRESETS[0].0).abs();
    for candidate in PRESETS.iter().copied() {
        let diff = (bitrate - candidate.0).abs();
        if diff < best_diff {
            best = candidate;
            best_diff = diff;
        }
    }
    best.1
}

fn cli_value_provided(matches: &ArgMatches, id: &str) -> bool {
    matches
        .value_source(id)
        .is_some_and(|source| source != ValueSource::DefaultValue)
}

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

    if !cli_value_provided(matches, "sub_mode") {
        if let Some(sub_mode) = cfg.sub_mode {
            args.sub_mode = sub_mode;
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
