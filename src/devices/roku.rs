//! Roku model capability profiles.
//!
// Sources:
// - https://developer.roku.com/docs/specs/media/streaming-specifications.md

use rsmpeg::ffi;

use super::device_profile::{
    ContainerFormat, DeviceFamily, H264Level, H264Profile, Resolution, StreamingDevice,
};

const ROKU_CONTAINERS: &[ContainerFormat] = &[ContainerFormat::Mp4, ContainerFormat::Mkv];
const ROKU_VIDEO: &[ffi::AVCodecID] = &[ffi::AV_CODEC_ID_H264, ffi::AV_CODEC_ID_HEVC];
const ROKU_AUDIO: &[ffi::AVCodecID] = &[
    ffi::AV_CODEC_ID_AAC,
    ffi::AV_CODEC_ID_AC3,
    ffi::AV_CODEC_ID_EAC3,
    ffi::AV_CODEC_ID_DTS,
];

/// Capability profile for Roku Express.
pub const ROKU_EXPRESS: StreamingDevice = StreamingDevice {
    name: "Roku Express",
    model: "roku_express",
    maker: "Roku",
    family: DeviceFamily::Roku,
    containers: ROKU_CONTAINERS,
    video_codecs: ROKU_VIDEO,
    audio_codecs: ROKU_AUDIO,
    max_h264_profile: H264Profile::High,
    max_h264_level: H264Level::Level4_1,
    max_fps: 60,
    max_resolution: Resolution::Resolution1080p,
    max_video_bitrate: Some(20_000_000),
    max_audio_bitrate: Some(640_000),
};

/// Capability profile for Roku Streaming Stick 4K.
pub const ROKU_STREAMING_STICK_4K: StreamingDevice = StreamingDevice {
    name: "Roku Streaming Stick 4K",
    model: "roku_streaming_stick_4k",
    maker: "Roku",
    family: DeviceFamily::Roku,
    containers: ROKU_CONTAINERS,
    video_codecs: ROKU_VIDEO,
    audio_codecs: ROKU_AUDIO,
    max_h264_profile: H264Profile::High,
    max_h264_level: H264Level::Level5_1,
    max_fps: 60,
    max_resolution: Resolution::Resolution2160p,
    max_video_bitrate: Some(40_000_000),
    max_audio_bitrate: Some(1_536_000),
};

/// Capability profile for Roku Ultra.
pub const ROKU_ULTRA: StreamingDevice = StreamingDevice {
    name: "Roku Ultra",
    model: "roku_ultra",
    maker: "Roku",
    family: DeviceFamily::Roku,
    containers: ROKU_CONTAINERS,
    video_codecs: ROKU_VIDEO,
    audio_codecs: ROKU_AUDIO,
    max_h264_profile: H264Profile::High,
    max_h264_level: H264Level::Level5_1,
    max_fps: 60,
    max_resolution: Resolution::Resolution2160p,
    max_video_bitrate: Some(40_000_000),
    max_audio_bitrate: Some(1_536_000),
};
