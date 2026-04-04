//! Shared device profile types and constraint aggregation helpers.

use std::{cmp::Ordering, convert::TryFrom};

use anyhow::{anyhow, bail, Result};
use rusty_ffmpeg::ffi;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Resolution {
    Resolution480p,
    Resolution720p,
    Resolution1080p,
    Resolution1440p,
    Resolution2160p,
}

impl Resolution {
    pub fn to_dimensions(self) -> (u32, u32) {
        match self {
            Resolution::Resolution480p => (640, 480),
            Resolution::Resolution720p => (1280, 720),
            Resolution::Resolution1080p => (1920, 1080),
            Resolution::Resolution1440p => (2560, 1440),
            Resolution::Resolution2160p => (3840, 2160),
        }
    }

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
pub enum H264Level {
    Level1 = 10,
    Level1_1 = 11,
    Level1_2 = 12,
    Level1_3 = 13,
    Level2 = 20,
    Level2_1 = 21,
    Level2_2 = 22,
    Level3 = 30,
    Level3_1 = 31,
    Level3_2 = 32,
    Level4 = 40,
    Level4_1 = 41,
    Level4_2 = 42,
    Level5 = 50,
    Level5_1 = 51,
    Level5_2 = 52,
}

impl PartialOrd for H264Level {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for H264Level {
    fn cmp(&self, other: &Self) -> Ordering {
        (*self as u32).cmp(&(*other as u32))
    }
}

impl TryFrom<i32> for H264Level {
    type Error = &'static str;

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

impl H264Level {
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
pub enum H264Profile {
    Baseline = ffi::AV_PROFILE_H264_BASELINE as isize,
    Main = ffi::AV_PROFILE_H264_MAIN as isize,
    Extended = ffi::AV_PROFILE_H264_EXTENDED as isize,
    High = ffi::AV_PROFILE_H264_HIGH as isize,
    High10 = ffi::AV_PROFILE_H264_HIGH_10 as isize,
    High422 = ffi::AV_PROFILE_H264_HIGH_422 as isize,
    High444 = ffi::AV_PROFILE_H264_HIGH_444 as isize,
}

impl TryFrom<i32> for H264Profile {
    type Error = &'static str;

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

impl TryFrom<u32> for H264Profile {
    type Error = &'static str;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        let value = i32::try_from(value).map_err(|_| "Invalid H.264 profile value")?;
        H264Profile::try_from(value)
    }
}

impl H264Profile {
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
pub enum ContainerFormat {
    Mp4,
    M4v,
    Mov,
    Mkv,
}

impl ContainerFormat {
    pub fn as_str(self) -> &'static str {
        match self {
            ContainerFormat::Mp4 => "mp4",
            ContainerFormat::M4v => "m4v",
            ContainerFormat::Mov => "mov",
            ContainerFormat::Mkv => "mkv",
        }
    }

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
pub enum DeviceFamily {
    Chromecast,
    Roku,
    AppleTv,
    FireTv,
}

impl DeviceFamily {
    pub fn as_str(self) -> &'static str {
        match self {
            DeviceFamily::Chromecast => "chromecast",
            DeviceFamily::Roku => "roku",
            DeviceFamily::AppleTv => "apple_tv",
            DeviceFamily::FireTv => "fire_tv",
        }
    }

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
pub struct StreamingDevice {
    pub name: &'static str,
    pub model: &'static str,
    pub maker: &'static str,
    pub family: DeviceFamily,
    pub containers: &'static [ContainerFormat],
    pub video_codecs: &'static [ffi::AVCodecID],
    pub audio_codecs: &'static [ffi::AVCodecID],
    pub max_h264_profile: H264Profile,
    pub max_h264_level: H264Level,
    pub max_fps: u32,
    pub max_resolution: Resolution,
    pub max_video_bitrate: Option<i64>,
    pub max_audio_bitrate: Option<i64>,
}

#[derive(Clone, Debug)]
pub struct ResolvedTargetProfile {
    #[allow(dead_code)]
    pub container: ContainerFormat,
    pub video_codec: ffi::AVCodecID,
    pub audio_codec: ffi::AVCodecID,
    pub h264_constraints: Option<(H264Profile, H264Level)>,
    pub max_fps: u32,
    pub max_resolution: Resolution,
    pub max_video_bitrate: Option<i64>,
    pub max_audio_bitrate: Option<i64>,
}

#[derive(Clone, Debug)]
#[allow(dead_code)]
pub struct InputMediaProfile {
    pub container: Option<ContainerFormat>,
    pub video_codec: ffi::AVCodecID,
    pub audio_codec: ffi::AVCodecID,
    pub video_bitrate: Option<i64>,
    pub audio_bitrate: Option<i64>,
}

#[derive(Clone, Debug)]
#[allow(dead_code)]
pub struct PlannedOutputProfile {
    pub container: ContainerFormat,
    pub video_codec: ffi::AVCodecID,
    pub audio_codec: ffi::AVCodecID,
    pub h264_constraints: Option<(H264Profile, H264Level)>,
    pub max_fps: u32,
    pub max_resolution: Resolution,
    pub target_video_bitrate: Option<i64>,
    pub target_audio_bitrate: Option<i64>,
}

impl StreamingDevice {
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

    pub fn get_common_container(devices: &[&StreamingDevice]) -> Result<ContainerFormat> {
        Self::get_common_containers(devices)?
            .into_iter()
            .next()
            .ok_or_else(|| anyhow!("No common output container found among the selected devices"))
    }

    pub fn get_min_h264_profile(devices: &[&StreamingDevice]) -> Result<H264Profile> {
        let mut min_profile = H264Profile::High444;
        for device in devices {
            if device.max_h264_profile < min_profile {
                min_profile = device.max_h264_profile;
            }
        }
        Ok(min_profile)
    }

    pub fn get_min_h264_level(devices: &[&StreamingDevice]) -> Result<H264Level> {
        let mut min_level = H264Level::iter().max_by_key(|level| *level as i32).unwrap();
        for device in devices {
            if device.max_h264_level < min_level {
                min_level = device.max_h264_level;
            }
        }
        Ok(min_level)
    }

    pub fn get_min_fps(devices: &[&StreamingDevice]) -> Result<u32> {
        let mut min_fps = u32::MAX;
        for device in devices {
            if device.max_fps < min_fps {
                min_fps = device.max_fps;
            }
        }
        Ok(min_fps)
    }

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

    pub fn get_min_video_bitrate(devices: &[&StreamingDevice]) -> Option<i64> {
        devices.iter().filter_map(|d| d.max_video_bitrate).min()
    }

    pub fn get_min_audio_bitrate(devices: &[&StreamingDevice]) -> Option<i64> {
        devices.iter().filter_map(|d| d.max_audio_bitrate).min()
    }
}

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

impl ResolvedTargetProfile {
    #[allow(dead_code)]
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
    use super::*;

    #[test]
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
    fn h264_profile_try_from_rejects_unknown_value() {
        assert!(H264Profile::try_from(-1).is_err());
    }
}
