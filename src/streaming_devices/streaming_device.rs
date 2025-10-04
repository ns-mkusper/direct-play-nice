//! Streaming devices and their direct-play specs

use std::{clone::Clone, cmp::Ordering};

use anyhow::{anyhow, bail, Error};
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
    fn to_dimensions(&self) -> (u32, u32) {
        match self {
            Resolution::Resolution480p => (640, 480),
            Resolution::Resolution720p => (1280, 720),
            Resolution::Resolution1080p => (1920, 1080),
            Resolution::Resolution1440p => (2560, 1440),
            Resolution::Resolution2160p => (3840, 2160),
        }
    }

    fn from_resolution(x: u32, y: u32) -> Resolution {
        match (x, y) {
            (640, 480) => Resolution::Resolution480p,
            (1280, 720) => Resolution::Resolution720p,
            (1920, 1080) => Resolution::Resolution1080p,
            (2560, 1440) => Resolution::Resolution1440p,
            (3840, 2160) => Resolution::Resolution2160p,
            _ => Resolution::Resolution1080p, // TODO: What should we do if an unsupported resolution is provided?
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, Ord, EnumIter)]
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

impl PartialEq for H264Level {
    fn eq(&self, other: &Self) -> bool {
        (*self as u32) == (*other as u32)
    }
}

impl PartialOrd for H264Level {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some((*self as u32).cmp(&(*other as u32)))
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

#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StreamingDevice {
    #[allow(dead_code)]
    pub name: &'static str,
    pub max_h264_profile: H264Profile,
    pub max_h264_level: H264Level,
    pub max_fps: u32,
    pub max_resolution: Resolution,
    pub model: &'static str,
    #[allow(dead_code)]
    pub maker: &'static str,
    pub audio_codec: [Option<ffi::AVCodecID>; 5],
    pub video_codec: [Option<ffi::AVCodecID>; 5],
}

impl StreamingDevice {
    /// Finds the intersection of video codecs among all `StreamingDevice`'s
    pub fn get_common_video_codec(
        devices: &Vec<&StreamingDevice>,
    ) -> Result<ffi::AVCodecID, self::Error> {
        let mut common_codecs: Vec<ffi::AVCodecID> = devices[0]
            .video_codec
            .iter()
            .filter_map(|codec_id| *codec_id)
            .collect();

        for device in &devices[1..] {
            common_codecs.retain(|codec_id| device.video_codec.contains(&Some(*codec_id)));
        }

        common_codecs.into_iter().next().ok_or_else(|| {
            anyhow!("No common video codec found among the provided streaming devices.")
        })
    }

    /// Finds the intersection of audio codecs among all `StreamingDevice`'s
    pub fn get_common_audio_codec(
        devices: &Vec<&StreamingDevice>,
    ) -> Result<ffi::AVCodecID, self::Error> {
        let mut common_codecs: Vec<ffi::AVCodecID> = devices[0]
            .audio_codec
            .iter()
            .filter_map(|codec_id| *codec_id)
            .collect();

        for device in &devices[1..] {
            common_codecs.retain(|codec_id| device.audio_codec.contains(&Some(*codec_id)));
        }

        common_codecs.into_iter().next().ok_or_else(|| {
            anyhow!("No common audio codec found among the provided streaming devices.")
        })
    }

    /// Gets the minimum H.264 profile level among all devices
    pub fn get_min_h264_profile(
        devices: &Vec<&StreamingDevice>,
    ) -> Result<H264Profile, self::Error> {
        let mut min_profile = H264Profile::High444; // TODO: implement more intelligent max method

        for device in devices {
            if device.max_h264_profile < min_profile {
                min_profile = device.max_h264_profile;
            }
        }

        Ok(min_profile)
    }

    /// Gets the minimum H.264 level among all `StreamingDevice`'s
    pub fn get_min_h264_level(devices: &Vec<&StreamingDevice>) -> Result<H264Level, self::Error> {
        let mut min_level = H264Level::iter().max_by_key(|level| *level as i32).unwrap();

        for device in devices {
            if device.max_h264_level < min_level {
                min_level = device.max_h264_level;
            }
        }

        Ok(min_level)
    }

    /// Gets the minimum FPS among all `StreamingDevice`'s
    pub fn get_min_fps(devices: &Vec<&StreamingDevice>) -> Result<u32, self::Error> {
        let mut min_fps = u32::MAX;

        for device in devices {
            if device.max_fps < min_fps as u32 {
                min_fps = device.max_fps as u32;
            }
        }

        Ok(min_fps)
    }

    /// Gets the minimum resolution of all provided `StreamingDevice`'s
    pub fn get_min_resolution(devices: &Vec<&StreamingDevice>) -> Result<Resolution, self::Error> {
        let mut min_res = (u32::MAX, u32::MAX);

        for device in devices {
            let res = device.max_resolution.to_dimensions();
            if res.0 < min_res.0 || res.1 < min_res.1 {
                min_res = res;
            }
        }

        // TODO: Better safety feature to avoid undesired resolutions?
        if min_res.0 < 1 || min_res.1 < 1 {
            bail!(
                "Target resolution ({}, {}) too small. Exiting...",
                min_res.0,
                min_res.1
            );
        }

        Ok(Resolution::from_resolution(min_res.0, min_res.1))
    }
}
