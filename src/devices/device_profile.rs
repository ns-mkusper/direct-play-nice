//! Shared device profile types and constraint aggregation helpers.

use std::{cmp::Ordering, convert::TryFrom};

use anyhow::{anyhow, bail, Result};
use rusty_ffmpeg::ffi;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
/// Named output resolutions used for device constraints.
pub enum Resolution {
    /// 640x480.
    Resolution480p,
    /// 1280x720.
    Resolution720p,
    /// 1920x1080.
    Resolution1080p,
    /// 2560x1440.
    Resolution1440p,
    /// 3840x2160.
    Resolution2160p,
}

/// Implements behavior for `Resolution`.
impl Resolution {
    /// Returns canonical `(width, height)` dimensions.
    pub fn to_dimensions(self) -> (u32, u32) {
        match self {
            Resolution::Resolution480p => (640, 480),
            Resolution::Resolution720p => (1280, 720),
            Resolution::Resolution1080p => (1920, 1080),
            Resolution::Resolution1440p => (2560, 1440),
            Resolution::Resolution2160p => (3840, 2160),
        }
    }

    /// Maps known dimensions to a [`Resolution`].
    ///
    /// Unknown values default to `1080p`.
    pub fn from_dimensions(x: u32, y: u32) -> Resolution {
        match (x, y) {
            (640, 480) => Resolution::Resolution480p,
            (1280, 720) => Resolution::Resolution720p,
            (1920, 1080) => Resolution::Resolution1080p,
            (2560, 1440) => Resolution::Resolution1440p,
            (3840, 2160) => Resolution::Resolution2160p,
            _ => Resolution::Resolution1080p,
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, EnumIter)]
/// H.264 level values used for compatibility checks.
pub enum H264Level {
    /// H.264 level 1.0.
    Level1 = 10,
    /// H.264 level 1.1.
    Level1_1 = 11,
    /// H.264 level 1.2.
    Level1_2 = 12,
    /// H.264 level 1.3.
    Level1_3 = 13,
    /// H.264 level 2.0.
    Level2 = 20,
    /// H.264 level 2.1.
    Level2_1 = 21,
    /// H.264 level 2.2.
    Level2_2 = 22,
    /// H.264 level 3.0.
    Level3 = 30,
    /// H.264 level 3.1.
    Level3_1 = 31,
    /// H.264 level 3.2.
    Level3_2 = 32,
    /// H.264 level 4.0.
    Level4 = 40,
    /// H.264 level 4.1.
    Level4_1 = 41,
    /// H.264 level 4.2.
    Level4_2 = 42,
    /// H.264 level 5.0.
    Level5 = 50,
    /// H.264 level 5.1.
    Level5_1 = 51,
    /// H.264 level 5.2.
    Level5_2 = 52,
}

/// Implements behavior for `H264Level`.
impl PartialOrd for H264Level {
    /// Executes the partial cmp routine.
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Implements behavior for `H264Level`.
impl Ord for H264Level {
    /// Executes the cmp routine.
    fn cmp(&self, other: &Self) -> Ordering {
        (*self as u32).cmp(&(*other as u32))
    }
}

/// Implements behavior for `H264Level`.
impl TryFrom<i32> for H264Level {
    type Error = &'static str;

    /// Executes the try from routine.
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            10 => Ok(H264Level::Level1),
            11 => Ok(H264Level::Level1_1),
            12 => Ok(H264Level::Level1_2),
            13 => Ok(H264Level::Level1_3),
            20 => Ok(H264Level::Level2),
            21 => Ok(H264Level::Level2_1),
            22 => Ok(H264Level::Level2_2),
            30 => Ok(H264Level::Level3),
            31 => Ok(H264Level::Level3_1),
            32 => Ok(H264Level::Level3_2),
            40 => Ok(H264Level::Level4),
            41 => Ok(H264Level::Level4_1),
            42 => Ok(H264Level::Level4_2),
            50 => Ok(H264Level::Level5),
            51 => Ok(H264Level::Level5_1),
            52 => Ok(H264Level::Level5_2),
            _ => Err("Invalid H.264 level value"),
        }
    }
}

/// Implements behavior for `H264Level`.
impl H264Level {
    /// Returns FFmpeg's textual representation for this level.
    pub fn ffmpeg_name(&self) -> &'static str {
        match self {
            H264Level::Level1 => "1",
            H264Level::Level1_1 => "1.1",
            H264Level::Level1_2 => "1.2",
            H264Level::Level1_3 => "1.3",
            H264Level::Level2 => "2",
            H264Level::Level2_1 => "2.1",
            H264Level::Level2_2 => "2.2",
            H264Level::Level3 => "3",
            H264Level::Level3_1 => "3.1",
            H264Level::Level3_2 => "3.2",
            H264Level::Level4 => "4",
            H264Level::Level4_1 => "4.1",
            H264Level::Level4_2 => "4.2",
            H264Level::Level5 => "5",
            H264Level::Level5_1 => "5.1",
            H264Level::Level5_2 => "5.2",
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
/// H.264 profile values used for compatibility checks.
pub enum H264Profile {
    /// Baseline profile.
    Baseline = ffi::AV_PROFILE_H264_BASELINE as isize,
    /// Main profile.
    Main = ffi::AV_PROFILE_H264_MAIN as isize,
    /// Extended profile.
    Extended = ffi::AV_PROFILE_H264_EXTENDED as isize,
    /// High profile.
    High = ffi::AV_PROFILE_H264_HIGH as isize,
    /// High 10 profile.
    High10 = ffi::AV_PROFILE_H264_HIGH_10 as isize,
    /// High 4:2:2 profile.
    High422 = ffi::AV_PROFILE_H264_HIGH_422 as isize,
    /// High 4:4:4 profile.
    High444 = ffi::AV_PROFILE_H264_HIGH_444 as isize,
}

/// Implements behavior for `H264Profile`.
impl TryFrom<i32> for H264Profile {
    type Error = &'static str;

    /// Executes the try from routine.
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            x if x == ffi::AV_PROFILE_H264_BASELINE as i32 => Ok(H264Profile::Baseline),
            x if x == ffi::AV_PROFILE_H264_MAIN as i32 => Ok(H264Profile::Main),
            x if x == ffi::AV_PROFILE_H264_EXTENDED as i32 => Ok(H264Profile::Extended),
            x if x == ffi::AV_PROFILE_H264_HIGH as i32 => Ok(H264Profile::High),
            x if x == ffi::AV_PROFILE_H264_HIGH_10 as i32 => Ok(H264Profile::High10),
            x if x == ffi::AV_PROFILE_H264_HIGH_422 as i32 => Ok(H264Profile::High422),
            x if x == ffi::AV_PROFILE_H264_HIGH_444 as i32 => Ok(H264Profile::High444),
            _ => Err("Invalid H.264 profile value"),
        }
    }
}

/// Implements behavior for `H264Profile`.
impl TryFrom<u32> for H264Profile {
    type Error = &'static str;

    /// Executes the try from routine.
    fn try_from(value: u32) -> Result<Self, Self::Error> {
        let value = i32::try_from(value).map_err(|_| "Invalid H.264 profile value")?;
        H264Profile::try_from(value)
    }
}

/// Implements behavior for `H264Profile`.
impl H264Profile {
    /// Returns FFmpeg's textual representation for this profile.
    pub fn ffmpeg_name(&self) -> &'static str {
        match self {
            H264Profile::Baseline => "baseline",
            H264Profile::Main => "main",
            H264Profile::Extended => "extended",
            H264Profile::High => "high",
            H264Profile::High10 => "high10",
            H264Profile::High422 => "high422",
            H264Profile::High444 => "high444",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
/// Output container format.
pub enum ContainerFormat {
    /// MPEG-4 Part 14 container.
    Mp4,
    /// iTunes-style MPEG-4 variant.
    M4v,
    /// QuickTime container.
    Mov,
    /// Matroska container.
    Mkv,
}

/// Implements behavior for `ContainerFormat`.
impl ContainerFormat {
    /// Returns the standard lowercase extension for this container.
    pub fn as_str(self) -> &'static str {
        match self {
            ContainerFormat::Mp4 => "mp4",
            ContainerFormat::M4v => "m4v",
            ContainerFormat::Mov => "mov",
            ContainerFormat::Mkv => "mkv",
        }
    }

    /// Parses a file extension into a container format.
    pub fn from_extension(ext: &str) -> Option<Self> {
        match ext.to_ascii_lowercase().as_str() {
            "mp4" => Some(ContainerFormat::Mp4),
            "m4v" => Some(ContainerFormat::M4v),
            "mov" => Some(ContainerFormat::Mov),
            "mkv" => Some(ContainerFormat::Mkv),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
/// Supported streaming-device families.
pub enum DeviceFamily {
    /// Google Chromecast and related devices.
    Chromecast,
    /// Roku devices.
    Roku,
    /// Apple TV devices.
    AppleTv,
    /// Amazon Fire TV devices.
    FireTv,
}

/// Implements behavior for `DeviceFamily`.
impl DeviceFamily {
    /// Returns this family as a canonical identifier string.
    pub fn as_str(self) -> &'static str {
        match self {
            DeviceFamily::Chromecast => "chromecast",
            DeviceFamily::Roku => "roku",
            DeviceFamily::AppleTv => "apple_tv",
            DeviceFamily::FireTv => "fire_tv",
        }
    }

    /// Parses a family identifier like `"roku"` or `"apple_tv"`.
    pub fn from_identifier(id: &str) -> Option<Self> {
        match id.trim().to_ascii_lowercase().as_str() {
            "chromecast" => Some(DeviceFamily::Chromecast),
            "roku" => Some(DeviceFamily::Roku),
            "apple_tv" | "appletv" | "apple-tv" => Some(DeviceFamily::AppleTv),
            "fire_tv" | "firetv" | "fire-tv" => Some(DeviceFamily::FireTv),
            _ => None,
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq, Eq)]
/// Capability profile for a specific streaming device model.
pub struct StreamingDevice {
    /// Human-readable device name.
    pub name: &'static str,
    /// Stable model identifier used by this crate.
    pub model: &'static str,
    /// Device manufacturer.
    pub maker: &'static str,
    /// Device family classification.
    pub family: DeviceFamily,
    /// Supported output containers.
    pub containers: &'static [ContainerFormat],
    /// Supported video codecs.
    pub video_codecs: &'static [ffi::AVCodecID],
    /// Supported audio codecs.
    pub audio_codecs: &'static [ffi::AVCodecID],
    /// Maximum supported H.264 profile.
    pub max_h264_profile: H264Profile,
    /// Maximum supported H.264 level.
    pub max_h264_level: H264Level,
    /// Maximum supported frame rate.
    pub max_fps: u32,
    /// Maximum supported output resolution.
    pub max_resolution: Resolution,
    /// Optional maximum supported video bitrate (bits/sec).
    pub max_video_bitrate: Option<i64>,
    /// Optional maximum supported audio bitrate (bits/sec).
    pub max_audio_bitrate: Option<i64>,
}

#[derive(Clone, Debug)]
/// Device-agnostic constraints resolved across selected targets.
pub struct ResolvedTargetProfile {
    #[allow(dead_code)]
    /// Selected output container.
    pub container: ContainerFormat,
    /// Selected output video codec.
    pub video_codec: ffi::AVCodecID,
    /// Selected output audio codec.
    pub audio_codec: ffi::AVCodecID,
    /// Optional H.264 profile/level constraints.
    pub h264_constraints: Option<(H264Profile, H264Level)>,
    /// Maximum output frame rate.
    pub max_fps: u32,
    /// Maximum output resolution.
    pub max_resolution: Resolution,
    /// Optional maximum output video bitrate (bits/sec).
    pub max_video_bitrate: Option<i64>,
    /// Optional maximum output audio bitrate (bits/sec).
    pub max_audio_bitrate: Option<i64>,
}

#[derive(Clone, Debug)]
#[allow(dead_code)]
/// Relevant media characteristics discovered from an input file.
pub struct InputMediaProfile {
    /// Input container format, if known.
    pub container: Option<ContainerFormat>,
    /// Input video codec.
    pub video_codec: ffi::AVCodecID,
    /// Input audio codec.
    pub audio_codec: ffi::AVCodecID,
    /// Input video bitrate (bits/sec), if known.
    pub video_bitrate: Option<i64>,
    /// Input audio bitrate (bits/sec), if known.
    pub audio_bitrate: Option<i64>,
}

#[derive(Clone, Debug)]
#[allow(dead_code)]
/// Planned output settings after device and input reconciliation.
pub struct PlannedOutputProfile {
    /// Planned output container.
    pub container: ContainerFormat,
    /// Planned output video codec.
    pub video_codec: ffi::AVCodecID,
    /// Planned output audio codec.
    pub audio_codec: ffi::AVCodecID,
    /// Optional H.264 profile/level constraints.
    pub h264_constraints: Option<(H264Profile, H264Level)>,
    /// Planned maximum output frame rate.
    pub max_fps: u32,
    /// Planned maximum output resolution.
    pub max_resolution: Resolution,
    /// Planned target video bitrate (bits/sec), if set.
    pub target_video_bitrate: Option<i64>,
    /// Planned target audio bitrate (bits/sec), if set.
    pub target_audio_bitrate: Option<i64>,
}

/// Implements behavior for `StreamingDevice`.
impl StreamingDevice {
    /// Returns one video codec shared by all provided devices.
    pub fn get_common_video_codec(devices: &[&StreamingDevice]) -> Result<ffi::AVCodecID> {
        let first = devices
            .first()
            .ok_or_else(|| anyhow!("No streaming devices provided"))?;
        let mut common_codecs = first.video_codecs.to_vec();
        for device in &devices[1..] {
            common_codecs.retain(|codec_id| device.video_codecs.contains(codec_id));
        }
        common_codecs
            .into_iter()
            .next()
            .ok_or_else(|| anyhow!("No common video codec found among the selected devices"))
    }

    /// Returns one audio codec shared by all provided devices.
    pub fn get_common_audio_codec(devices: &[&StreamingDevice]) -> Result<ffi::AVCodecID> {
        let first = devices
            .first()
            .ok_or_else(|| anyhow!("No streaming devices provided"))?;
        let mut common_codecs = first.audio_codecs.to_vec();
        for device in &devices[1..] {
            common_codecs.retain(|codec_id| device.audio_codecs.contains(codec_id));
        }
        common_codecs
            .into_iter()
            .next()
            .ok_or_else(|| anyhow!("No common audio codec found among the selected devices"))
    }

    /// Returns all container formats common to all provided devices.
    pub fn get_common_containers(devices: &[&StreamingDevice]) -> Result<Vec<ContainerFormat>> {
        let first = devices
            .first()
            .ok_or_else(|| anyhow!("No streaming devices provided"))?;
        let mut common = first.containers.to_vec();
        for device in &devices[1..] {
            common.retain(|container| device.containers.contains(container));
        }
        if common.is_empty() {
            bail!("No common output container found among the selected devices");
        }
        Ok(common)
    }

    /// Returns the first common container format across devices.
    pub fn get_common_container(devices: &[&StreamingDevice]) -> Result<ContainerFormat> {
        Self::get_common_containers(devices)?
            .into_iter()
            .next()
            .ok_or_else(|| anyhow!("No common output container found among the selected devices"))
    }

    /// Returns the strictest (lowest) maximum H.264 profile across devices.
    pub fn get_min_h264_profile(devices: &[&StreamingDevice]) -> Result<H264Profile> {
        let mut min_profile = H264Profile::High444;
        for device in devices {
            if device.max_h264_profile < min_profile {
                min_profile = device.max_h264_profile;
            }
        }
        Ok(min_profile)
    }

    /// Returns the strictest (lowest) maximum H.264 level across devices.
    pub fn get_min_h264_level(devices: &[&StreamingDevice]) -> Result<H264Level> {
        let mut min_level = H264Level::iter().max_by_key(|level| *level as i32).unwrap();
        for device in devices {
            if device.max_h264_level < min_level {
                min_level = device.max_h264_level;
            }
        }
        Ok(min_level)
    }

    /// Returns the minimum supported fps limit across devices.
    pub fn get_min_fps(devices: &[&StreamingDevice]) -> Result<u32> {
        let mut min_fps = u32::MAX;
        for device in devices {
            if device.max_fps < min_fps {
                min_fps = device.max_fps;
            }
        }
        Ok(min_fps)
    }

    /// Returns the minimum supported resolution across devices.
    pub fn get_min_resolution(devices: &[&StreamingDevice]) -> Result<Resolution> {
        let mut min_res = (u32::MAX, u32::MAX);
        for device in devices {
            let res = device.max_resolution.to_dimensions();
            if res.0 < min_res.0 || res.1 < min_res.1 {
                min_res = res;
            }
        }
        if min_res.0 < 1 || min_res.1 < 1 {
            bail!(
                "Target resolution ({}, {}) too small. Exiting...",
                min_res.0,
                min_res.1
            );
        }
        Ok(Resolution::from_dimensions(min_res.0, min_res.1))
    }

    /// Returns the smallest video bitrate limit across devices.
    pub fn get_min_video_bitrate(devices: &[&StreamingDevice]) -> Option<i64> {
        devices.iter().filter_map(|d| d.max_video_bitrate).min()
    }

    /// Returns the smallest audio bitrate limit across devices.
    pub fn get_min_audio_bitrate(devices: &[&StreamingDevice]) -> Option<i64> {
        devices.iter().filter_map(|d| d.max_audio_bitrate).min()
    }
}

/// Computes a resolved compatibility profile for selected target devices.
///
/// # Examples
///
/// ```rust
/// let devices = vec![
///     &direct_play_nice::devices::roku::ROKU_ULTRA,
///     &direct_play_nice::devices::apple_tv::APPLE_TV_4K_3RD_GEN,
/// ];
/// let resolved = direct_play_nice::devices::resolve_target_profile(&devices)
///     .expect("should find a compatible intersection");
/// assert!(resolved.max_fps > 0);
/// ```
pub fn resolve_target_profile(devices: &[&StreamingDevice]) -> Result<ResolvedTargetProfile> {
    let video_codec = StreamingDevice::get_common_video_codec(devices)?;
    let audio_codec = StreamingDevice::get_common_audio_codec(devices)?;
    let container = StreamingDevice::get_common_container(devices)?;
    let h264_constraints = if video_codec == ffi::AV_CODEC_ID_H264 {
        Some((
            StreamingDevice::get_min_h264_profile(devices)?,
            StreamingDevice::get_min_h264_level(devices)?,
        ))
    } else {
        None
    };

    Ok(ResolvedTargetProfile {
        container,
        video_codec,
        audio_codec,
        h264_constraints,
        max_fps: StreamingDevice::get_min_fps(devices)?,
        max_resolution: StreamingDevice::get_min_resolution(devices)?,
        max_video_bitrate: StreamingDevice::get_min_video_bitrate(devices),
        max_audio_bitrate: StreamingDevice::get_min_audio_bitrate(devices),
    })
}

#[allow(dead_code)]
/// Produces an output plan for selected devices and input media properties.
///
/// # Examples
///
/// ```rust
/// use rsmpeg::ffi;
///
/// let devices = vec![&direct_play_nice::devices::roku::ROKU_ULTRA];
/// let input = direct_play_nice::devices::InputMediaProfile {
///     container: Some(direct_play_nice::devices::ContainerFormat::Mkv),
///     video_codec: ffi::AV_CODEC_ID_H264,
///     audio_codec: ffi::AV_CODEC_ID_AAC,
///     video_bitrate: Some(8_000_000),
///     audio_bitrate: Some(192_000),
/// };
/// let plan = direct_play_nice::devices::plan_output_profile(&devices, &input)
///     .expect("planning should succeed");
/// assert!(plan.max_fps > 0);
/// ```
pub fn plan_output_profile(
    devices: &[&StreamingDevice],
    input: &InputMediaProfile,
) -> Result<PlannedOutputProfile> {
    let resolved = resolve_target_profile(devices)?;
    let common_containers = StreamingDevice::get_common_containers(devices)?;

    let container = input
        .container
        .filter(|container| common_containers.contains(container))
        .unwrap_or(resolved.container);

    let video_codec = if devices
        .iter()
        .all(|device| device.video_codecs.contains(&input.video_codec))
    {
        input.video_codec
    } else {
        resolved.video_codec
    };
    let audio_codec = if devices
        .iter()
        .all(|device| device.audio_codecs.contains(&input.audio_codec))
    {
        input.audio_codec
    } else {
        resolved.audio_codec
    };

    let target_video_bitrate = match (input.video_bitrate, resolved.max_video_bitrate) {
        (Some(input), Some(limit)) => Some(input.min(limit)),
        (Some(input), None) => Some(input),
        (None, Some(limit)) => Some(limit),
        (None, None) => None,
    };
    let target_audio_bitrate = match (input.audio_bitrate, resolved.max_audio_bitrate) {
        (Some(input), Some(limit)) => Some(input.min(limit)),
        (Some(input), None) => Some(input),
        (None, Some(limit)) => Some(limit),
        (None, None) => None,
    };

    let h264_constraints = if video_codec == ffi::AV_CODEC_ID_H264 {
        resolved.h264_constraints
    } else {
        None
    };

    Ok(PlannedOutputProfile {
        container,
        video_codec,
        audio_codec,
        h264_constraints,
        max_fps: resolved.max_fps,
        max_resolution: resolved.max_resolution,
        target_video_bitrate,
        target_audio_bitrate,
    })
}

/// Implements behavior for `ResolvedTargetProfile`.
impl ResolvedTargetProfile {
    #[allow(dead_code)]
    /// Returns whether this profile is playable by `device`.
    pub fn is_compatible_with_device(&self, device: &StreamingDevice) -> bool {
        let h264_ok = if let Some((profile, level)) = self.h264_constraints {
            profile <= device.max_h264_profile && level <= device.max_h264_level
        } else {
            true
        };

        let bitrate_ok = self
            .max_video_bitrate
            .map(|v| device.max_video_bitrate.map(|d| v <= d).unwrap_or(true))
            .unwrap_or(true)
            && self
                .max_audio_bitrate
                .map(|v| device.max_audio_bitrate.map(|d| v <= d).unwrap_or(true))
                .unwrap_or(true);

        device.containers.contains(&self.container)
            && device.video_codecs.contains(&self.video_codec)
            && device.audio_codecs.contains(&self.audio_codec)
            && self.max_fps <= device.max_fps
            && {
                let (w, h) = self.max_resolution.to_dimensions();
                let (dw, dh) = device.max_resolution.to_dimensions();
                w <= dw && h <= dh
            }
            && h264_ok
            && bitrate_ok
    }
}

#[cfg(test)]
mod tests {
    use crate::devices::device_profile::*;

    #[test]
    /// Executes the h264 profile try from accepts known profiles routine.
    fn h264_profile_try_from_accepts_known_profiles() {
        assert_eq!(
            H264Profile::try_from(ffi::AV_PROFILE_H264_BASELINE).expect("baseline should map"),
            H264Profile::Baseline
        );
        assert_eq!(
            H264Profile::try_from(ffi::AV_PROFILE_H264_HIGH).expect("high should map"),
            H264Profile::High
        );
    }

    #[test]
    /// Executes the h264 profile try from rejects unknown value routine.
    fn h264_profile_try_from_rejects_unknown_value() {
        assert!(H264Profile::try_from(-1).is_err());
    }
}
