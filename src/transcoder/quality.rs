use crate::transcoder::prelude::*;

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum, Deserialize)]
#[serde(rename_all = "kebab-case")]
/// Enumerates options for VideoCodecPreference.
pub(crate) enum VideoCodecPreference {
    Auto,
    H264,
    Hevc,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum, Deserialize)]
/// Enumerates options for VideoQuality.
pub(crate) enum VideoQuality {
    /// Leave video resolution/bitrate untouched.
    #[value(
        name = "match-source",
        alias = "source",
        alias = "auto",
        alias = "original",
        alias = "input"
    )]
    #[serde(
        rename = "match-source",
        alias = "source",
        alias = "auto",
        alias = "original",
        alias = "input"
    )]
    MatchSource,
    /// 360p (SD) profile – ~1.2 Mbps target bitrate.
    #[value(name = "360p", alias = "sd", alias = "sd360", alias = "low")]
    #[serde(rename = "360p", alias = "sd", alias = "sd360", alias = "low")]
    P360,
    /// 480p (SD+) profile – ~2.5 Mbps target bitrate.
    #[value(name = "480p", alias = "sd480", alias = "dvd", alias = "standard")]
    #[serde(rename = "480p", alias = "sd480", alias = "dvd", alias = "standard")]
    P480,
    /// 720p (HD) profile – ~5 Mbps target bitrate.
    #[value(name = "720p", alias = "hd", alias = "hd-ready", alias = "1280x720")]
    #[serde(rename = "720p", alias = "hd", alias = "hd-ready", alias = "1280x720")]
    P720,
    /// 1080p (Full HD) profile – ~8 Mbps target bitrate.
    #[value(name = "1080p", alias = "full-hd", alias = "fhd", alias = "1920x1080")]
    #[serde(
        rename = "1080p",
        alias = "full-hd",
        alias = "fhd",
        alias = "1920x1080"
    )]
    P1080,
    /// 1440p (Quad HD) profile – ~16 Mbps target bitrate.
    #[value(name = "1440p", alias = "qhd", alias = "2k", alias = "2560x1440")]
    #[serde(rename = "1440p", alias = "qhd", alias = "2k", alias = "2560x1440")]
    P1440,
    /// 2160p (Ultra HD / 4K) profile – ~35 Mbps target bitrate.
    #[value(name = "2160p", alias = "uhd", alias = "4k", alias = "3840x2160")]
    #[serde(rename = "2160p", alias = "uhd", alias = "4k", alias = "3840x2160")]
    P2160,
}

/// Implements behavior for `VideoQuality`.
impl VideoQuality {
    /// Executes the targets routine.
    fn targets(self) -> (Option<(u32, u32)>, Option<i64>) {
        match self {
            VideoQuality::MatchSource => (None, None),
            VideoQuality::P360 => (Some((640, 360)), Some(1_200_000)),
            VideoQuality::P480 => (Some((854, 480)), Some(2_500_000)),
            VideoQuality::P720 => (Some((1280, 720)), Some(5_000_000)),
            VideoQuality::P1080 => (Some((1920, 1080)), Some(8_000_000)),
            VideoQuality::P1440 => (Some((2560, 1440)), Some(16_000_000)),
            VideoQuality::P2160 => (Some((3840, 2160)), Some(35_000_000)),
        }
    }
}

/// Implements behavior for `VideoQuality`.
impl std::fmt::Display for VideoQuality {
    /// Executes the fmt routine.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let label = match self {
            VideoQuality::MatchSource => "match-source",
            VideoQuality::P360 => "360p",
            VideoQuality::P480 => "480p",
            VideoQuality::P720 => "720p",
            VideoQuality::P1080 => "1080p",
            VideoQuality::P1440 => "1440p",
            VideoQuality::P2160 => "2160p",
        };
        write!(f, "{}", label)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum, Deserialize)]
/// Enumerates options for AudioQuality.
pub(crate) enum AudioQuality {
    /// Leave audio bitrate untouched.
    #[value(
        name = "match-source",
        alias = "source",
        alias = "auto",
        alias = "original"
    )]
    #[serde(
        rename = "match-source",
        alias = "source",
        alias = "auto",
        alias = "original"
    )]
    MatchSource,
    /// 320 kbps (very high) AAC target bitrate.
    #[value(name = "320k", alias = "very-high", alias = "studio")]
    #[serde(rename = "320k", alias = "very-high", alias = "studio")]
    K320,
    /// 256 kbps (high) AAC target bitrate.
    #[value(name = "256k", alias = "high", alias = "itunes")]
    #[serde(rename = "256k", alias = "high", alias = "itunes")]
    K256,
    /// 224 kbps AAC target bitrate.
    #[value(name = "224k", alias = "broadcast")]
    #[serde(rename = "224k", alias = "broadcast")]
    K224,
    /// 192 kbps (standard) AAC target bitrate.
    #[value(name = "192k", alias = "standard", alias = "cd")]
    #[serde(rename = "192k", alias = "standard", alias = "cd")]
    K192,
    /// 160 kbps AAC target bitrate.
    #[value(name = "160k", alias = "medium-high")]
    #[serde(rename = "160k", alias = "medium-high")]
    K160,
    /// 128 kbps (medium) AAC target bitrate.
    #[value(name = "128k", alias = "medium", alias = "default")]
    #[serde(rename = "128k", alias = "medium", alias = "default")]
    K128,
    /// 96 kbps (low) AAC target bitrate.
    #[value(name = "96k", alias = "low", alias = "speech")]
    #[serde(rename = "96k", alias = "low", alias = "speech")]
    K96,
}

/// Implements behavior for `AudioQuality`.
impl AudioQuality {
    /// Executes the bitrate routine.
    fn bitrate(self) -> Option<i64> {
        match self {
            AudioQuality::MatchSource => None,
            AudioQuality::K320 => Some(320_000),
            AudioQuality::K256 => Some(256_000),
            AudioQuality::K224 => Some(224_000),
            AudioQuality::K192 => Some(192_000),
            AudioQuality::K160 => Some(160_000),
            AudioQuality::K128 => Some(128_000),
            AudioQuality::K96 => Some(96_000),
        }
    }
}

/// Implements behavior for `AudioQuality`.
impl std::fmt::Display for AudioQuality {
    /// Executes the fmt routine.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let label = match self {
            AudioQuality::MatchSource => "match-source",
            AudioQuality::K320 => "320k",
            AudioQuality::K256 => "256k",
            AudioQuality::K224 => "224k",
            AudioQuality::K192 => "192k",
            AudioQuality::K160 => "160k",
            AudioQuality::K128 => "128k",
            AudioQuality::K96 => "96k",
        };
        write!(f, "{}", label)
    }
}

#[derive(Copy, Clone, Debug, Default)]
/// Stores data for QualityLimits.
pub(crate) struct QualityLimits {
    pub(crate) max_video_dimensions: Option<(u32, u32)>,
    pub(crate) max_video_bitrate: Option<i64>,
    pub(crate) max_audio_bitrate: Option<i64>,
}

/// Implements behavior for `QualityLimits`.
impl QualityLimits {
    /// Executes the apply video quality routine.
    pub(crate) fn apply_video_quality(&mut self, video_quality: VideoQuality) {
        let (dimensions, bitrate) = video_quality.targets();
        self.max_video_dimensions = dimensions;
        self.max_video_bitrate = bitrate;
    }

    /// Executes the apply audio quality routine.
    pub(crate) fn apply_audio_quality(&mut self, audio_quality: AudioQuality) {
        self.max_audio_bitrate = audio_quality.bitrate();
    }
}
