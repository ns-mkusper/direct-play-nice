//! Streaming devices and their direct-play specs

use std::clone::Clone;

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug)]
pub struct StreamingDevice {
    pub name: &'static str,
    pub max_h264_profile: &'static str,
    pub max_h264_level: f32,
    pub max_fps: u32,
    pub max_resolution: Resolution,
    pub model: &'static str,
    pub maker: &'static str,
    pub audio_codec: u32,
    pub video_codec: u32,
}

impl StreamingDevice {
    // pub fn find_common_video_codec(devices: &[StreamingDevice]) -> Option<String> {
    //     // Find the common video codec among all devices
    //     // TODO: Implement
    // }

    // pub fn find_common_audio_codec(devices: &[StreamingDevice]) -> Option<String> {
    //     // Find the common audio codec among all devices
    //     // TODO: Implement
    // }

    // pub fn find_min_h264_profile(devices: &[StreamingDevice]) -> u8 {
    //     // Find the minimum H.264 profile level among all devices
    //     // TODO: Implement
    // }

    // pub fn find_min_h264_level(devices: &[StreamingDevice]) -> f32 {
    //     // Find the minimum H.264 level among all devices
    //     // TODO: Implement
    // }

    // pub fn find_min_fps(devices: &[StreamingDevice]) -> u32 {
    //     // Find the minimum FPS among all devices
    //     // TODO: Implement
    // }

    pub fn find_min_resolution(devices: &[StreamingDevice]) -> Resolution {
        let mut min_res = (9999, 9999); // TODO: set to more sensible value

        for device in devices {
            let res = device.max_resolution.to_dimensions();
            if res.0 < min_res.0 || res.1 < min_res.1 {
                min_res = res;
            }
        }

        Resolution::from_resolution(min_res.0, min_res.1)
    }
}
