// Sources:
// - https://developer.amazon.com/docs/device-specs/device-specifications-fire-tv-streaming-media-player.html

use rsmpeg::ffi;

use super::device_profile::{
    ContainerFormat, DeviceFamily, H264Level, H264Profile, Resolution, StreamingDevice,
};

const FIRE_TV_CONTAINERS: &[ContainerFormat] = &[ContainerFormat::Mp4, ContainerFormat::Mkv];
const FIRE_TV_VIDEO: &[ffi::AVCodecID] = &[
    ffi::AV_CODEC_ID_H264,
    ffi::AV_CODEC_ID_HEVC,
    ffi::AV_CODEC_ID_AV1,
];
const FIRE_TV_AUDIO: &[ffi::AVCodecID] = &[
    ffi::AV_CODEC_ID_AAC,
    ffi::AV_CODEC_ID_AC3,
    ffi::AV_CODEC_ID_EAC3,
    ffi::AV_CODEC_ID_FLAC,
];

pub const FIRE_TV_STICK_4K: StreamingDevice = StreamingDevice {
    name: "Fire TV Stick 4K",
    model: "fire_tv_stick_4k",
    maker: "Amazon",
    family: DeviceFamily::FireTv,
    containers: FIRE_TV_CONTAINERS,
    video_codecs: FIRE_TV_VIDEO,
    audio_codecs: FIRE_TV_AUDIO,
    max_h264_profile: H264Profile::High,
    max_h264_level: H264Level::Level5_1,
    max_fps: 60,
    max_resolution: Resolution::Resolution2160p,
    max_video_bitrate: Some(35_000_000),
    max_audio_bitrate: Some(1_024_000),
};

pub const FIRE_TV_STICK_4K_MAX: StreamingDevice = StreamingDevice {
    name: "Fire TV Stick 4K Max",
    model: "fire_tv_stick_4k_max",
    maker: "Amazon",
    family: DeviceFamily::FireTv,
    containers: FIRE_TV_CONTAINERS,
    video_codecs: FIRE_TV_VIDEO,
    audio_codecs: FIRE_TV_AUDIO,
    max_h264_profile: H264Profile::High,
    max_h264_level: H264Level::Level5_1,
    max_fps: 60,
    max_resolution: Resolution::Resolution2160p,
    max_video_bitrate: Some(40_000_000),
    max_audio_bitrate: Some(1_024_000),
};

pub const FIRE_TV_CUBE_3RD_GEN: StreamingDevice = StreamingDevice {
    name: "Fire TV Cube (3rd gen)",
    model: "fire_tv_cube_3rd_gen",
    maker: "Amazon",
    family: DeviceFamily::FireTv,
    containers: FIRE_TV_CONTAINERS,
    video_codecs: FIRE_TV_VIDEO,
    audio_codecs: FIRE_TV_AUDIO,
    max_h264_profile: H264Profile::High,
    max_h264_level: H264Level::Level5_2,
    max_fps: 60,
    max_resolution: Resolution::Resolution2160p,
    max_video_bitrate: Some(40_000_000),
    max_audio_bitrate: Some(1_024_000),
};
