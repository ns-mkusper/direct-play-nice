//! Apple TV model capability profiles.
//!
// Sources:
// - https://developer.apple.com/library/archive/technotes/tn2429/_index.html
// - https://developer.apple.com/documentation/tvos-release-notes/tvos-media-formats

use rsmpeg::ffi;

use super::device_profile::{
    ContainerFormat, DeviceFamily, H264Level, H264Profile, Resolution, StreamingDevice,
};

const APPLE_TV_CONTAINERS: &[ContainerFormat] = &[
    ContainerFormat::Mp4,
    ContainerFormat::M4v,
    ContainerFormat::Mov,
];
const APPLE_TV_HD_VIDEO: &[ffi::AVCodecID] = &[ffi::AV_CODEC_ID_H264];
const APPLE_TV_4K_VIDEO: &[ffi::AVCodecID] = &[ffi::AV_CODEC_ID_H264, ffi::AV_CODEC_ID_HEVC];
const APPLE_TV_AUDIO: &[ffi::AVCodecID] = &[
    ffi::AV_CODEC_ID_AAC,
    ffi::AV_CODEC_ID_AC3,
    ffi::AV_CODEC_ID_EAC3,
];

/// Capability profile for Apple TV HD.
pub const APPLE_TV_HD: StreamingDevice = StreamingDevice {
    name: "Apple TV HD",
    model: "apple_tv_hd",
    maker: "Apple",
    family: DeviceFamily::AppleTv,
    containers: APPLE_TV_CONTAINERS,
    video_codecs: APPLE_TV_HD_VIDEO,
    audio_codecs: APPLE_TV_AUDIO,
    max_h264_profile: H264Profile::High,
    max_h264_level: H264Level::Level4_2,
    max_fps: 60,
    max_resolution: Resolution::Resolution1080p,
    max_video_bitrate: Some(20_000_000),
    max_audio_bitrate: Some(640_000),
};

/// Capability profile for Apple TV 4K (1st generation).
pub const APPLE_TV_4K_1ST_GEN: StreamingDevice = StreamingDevice {
    name: "Apple TV 4K (1st gen)",
    model: "apple_tv_4k_1st_gen",
    maker: "Apple",
    family: DeviceFamily::AppleTv,
    containers: APPLE_TV_CONTAINERS,
    video_codecs: APPLE_TV_4K_VIDEO,
    audio_codecs: APPLE_TV_AUDIO,
    max_h264_profile: H264Profile::High,
    max_h264_level: H264Level::Level5_1,
    max_fps: 60,
    max_resolution: Resolution::Resolution2160p,
    max_video_bitrate: Some(35_000_000),
    max_audio_bitrate: Some(768_000),
};

/// Capability profile for Apple TV 4K (2nd generation).
pub const APPLE_TV_4K_2ND_GEN: StreamingDevice = StreamingDevice {
    name: "Apple TV 4K (2nd gen)",
    model: "apple_tv_4k_2nd_gen",
    maker: "Apple",
    family: DeviceFamily::AppleTv,
    containers: APPLE_TV_CONTAINERS,
    video_codecs: APPLE_TV_4K_VIDEO,
    audio_codecs: APPLE_TV_AUDIO,
    max_h264_profile: H264Profile::High,
    max_h264_level: H264Level::Level5_1,
    max_fps: 60,
    max_resolution: Resolution::Resolution2160p,
    max_video_bitrate: Some(40_000_000),
    max_audio_bitrate: Some(768_000),
};

/// Capability profile for Apple TV 4K (3rd generation).
pub const APPLE_TV_4K_3RD_GEN: StreamingDevice = StreamingDevice {
    name: "Apple TV 4K (3rd gen)",
    model: "apple_tv_4k_3rd_gen",
    maker: "Apple",
    family: DeviceFamily::AppleTv,
    containers: APPLE_TV_CONTAINERS,
    video_codecs: APPLE_TV_4K_VIDEO,
    audio_codecs: APPLE_TV_AUDIO,
    max_h264_profile: H264Profile::High,
    max_h264_level: H264Level::Level5_2,
    max_fps: 60,
    max_resolution: Resolution::Resolution2160p,
    max_video_bitrate: Some(40_000_000),
    max_audio_bitrate: Some(768_000),
};
