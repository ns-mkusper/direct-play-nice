//! Shared policy enums and merge helpers used to reconcile CLI arguments with configuration defaults.

use clap::ValueEnum;
use serde::Deserialize;

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum, Deserialize)]
#[serde(rename_all = "lowercase")]
/// Policy for handling non-primary or unsupported video streams.
pub(crate) enum UnsupportedVideoPolicy {
    Convert,
    Ignore,
    Fail,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum, Deserialize)]
#[serde(rename_all = "lowercase")]
/// Subtitle processing strategy applied during conversion.
pub(crate) enum SubMode {
    Auto,
    Force,
    Skip,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum, Deserialize)]
#[serde(rename_all = "kebab-case")]
/// Policy for subtitle decode/encode failures after a subtitle stream was selected.
pub(crate) enum SubtitleFailurePolicy {
    /// Warn, disable only the failing subtitle stream, and keep A/V conversion running.
    SkipStream,
    /// Abort conversion when any selected subtitle stream fails.
    Fail,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum, Deserialize)]
#[serde(rename_all = "lowercase")]
/// OCR backend selector for bitmap-subtitle conversion.
pub(crate) enum OcrEngine {
    Auto,
    Tesseract,
    PpOcrV3,
    PpOcrV4,
    External,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum, Deserialize)]
#[serde(rename_all = "lowercase")]
/// Output subtitle format produced by OCR.
pub(crate) enum OcrFormat {
    Srt,
    Ass,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum, Deserialize)]
#[serde(rename_all = "lowercase")]
/// Ranking key used when auto-selecting the primary video stream.
pub(crate) enum PrimaryVideoCriteria {
    Resolution,
    Bitrate,
    Fps,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum, Deserialize)]
#[serde(rename_all = "kebab-case")]
/// Resampling kernel used when video frames are scaled.
pub(crate) enum ResizeQuality {
    FastBilinear,
    Bilinear,
    Bicubic,
    Lanczos,
    Spline,
}

impl std::fmt::Display for ResizeQuality {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let label = match self {
            ResizeQuality::FastBilinear => "fast-bilinear",
            ResizeQuality::Bilinear => "bilinear",
            ResizeQuality::Bicubic => "bicubic",
            ResizeQuality::Lanczos => "lanczos",
            ResizeQuality::Spline => "spline",
        };
        write!(f, "{}", label)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum, Deserialize)]
#[serde(rename_all = "kebab-case")]
/// Backend used to resize video frames when dimensions change.
pub(crate) enum ResizeBackend {
    /// Prefer a GPU backend when the active decode/encode path can keep frames on device; otherwise use software.
    Auto,
    /// Always use libswscale software resizing.
    Software,
    /// Require CUDA `scale_cuda` resizing and fail if it cannot be used.
    Cuda,
}

impl std::fmt::Display for ResizeBackend {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let label = match self {
            ResizeBackend::Auto => "auto",
            ResizeBackend::Software => "software",
            ResizeBackend::Cuda => "cuda",
        };
        write!(f, "{}", label)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum, Deserialize, Default)]
#[serde(rename_all = "kebab-case")]
/// Scope of Servarr items inspected during periodic language audits.
pub(crate) enum ServarrLanguageAuditScope {
    /// Inspect recent import history only. This is efficient for delayed dub/sub follow-up.
    #[default]
    History,
    /// Inspect the current library inventory instead of only recently imported items.
    Inventory,
    /// Inspect inventory, then search the newest released missing-language items first.
    LatestMissing,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum, Deserialize, Default)]
#[serde(rename_all = "kebab-case")]
/// Confidence policy for choosing Servarr replacement releases when language requirements are missing.
pub(crate) enum ServarrLanguageCandidatePolicy {
    /// Only trust explicit language/subtitle fields in Arr release metadata.
    #[default]
    Strict,
    /// Trust matching Arr custom format names in addition to explicit metadata.
    CustomFormat,
    /// Trust custom format names and strong release-title tokens such as dual-audio or multi-subs.
    CustomFormatOrTitle,
    /// Use looser release-title inference. Intended for dry-run/manual review.
    TitleGuess,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum)]
/// Output rendering format for probe/report commands.
pub(crate) enum OutputFormat {
    Text,
    Json,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum)]
/// Stream subset used by `--probe-streams`.
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

pub(crate) fn clamp_dimensions(
    source_width: i32,
    source_height: i32,
    device_cap: (u32, u32),
    quality_cap: Option<(u32, u32)>,
) -> (i32, i32) {
    // Keep dimensions valid for encoder/pixel-format requirements, then fit
    // source aspect ratio inside the strictest active cap.
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
        return encoder_safe_dimensions(source_width, source_height);
    }

    scaled_dimensions(source_width, source_height, max_width, max_height)
}

fn encoder_safe_dimensions(width: i32, height: i32) -> (i32, i32) {
    let mut safe_width = width;
    let mut safe_height = height;

    if safe_width > 2 && safe_width % 2 != 0 {
        safe_width -= 1;
    }
    if safe_height > 2 && safe_height % 2 != 0 {
        safe_height -= 1;
    }

    (safe_width.max(1), safe_height.max(1))
}

fn scaled_dimensions(
    source_width: i32,
    source_height: i32,
    max_width: u32,
    max_height: u32,
) -> (i32, i32) {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn clamp_dimensions_preserves_source_when_within_caps() {
        assert_eq!(
            clamp_dimensions(640, 360, (1920, 1080), Some((1920, 1080))),
            (640, 360)
        );
    }

    #[test]
    fn clamp_dimensions_never_enlarges_without_quality_cap() {
        assert_eq!(clamp_dimensions(640, 360, (1920, 1080), None), (640, 360));
    }

    #[test]
    fn clamp_dimensions_normalizes_odd_dimensions_down_without_enlarging() {
        assert_eq!(clamp_dimensions(641, 361, (1920, 1080), None), (640, 360));
        assert_eq!(clamp_dimensions(3, 3, (1920, 1080), None), (2, 2));
    }

    #[test]
    fn clamp_dimensions_downscales_to_strictest_cap_and_alignment() {
        assert_eq!(
            clamp_dimensions(1920, 1080, (1280, 720), Some((3840, 2160))),
            (1280, 720)
        );
        assert_eq!(
            clamp_dimensions(640, 480, (1280, 720), Some((960, 720))),
            (640, 480)
        );
    }
}
