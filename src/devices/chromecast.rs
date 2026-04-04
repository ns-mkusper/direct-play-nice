// Sources:
// - https://developers.google.com/cast/docs/media

use rsmpeg::ffi;

use super::device_profile::{
    ContainerFormat, DeviceFamily, H264Level, H264Profile, Resolution, StreamingDevice,
};

const CHROMECAST_CONTAINERS: &[ContainerFormat] = &[ContainerFormat::Mp4];
const CHROMECAST_AUDIO: &[ffi::AVCodecID] = &[ffi::AV_CODEC_ID_AAC];

const H264_ONLY: &[ffi::AVCodecID] = &[ffi::AV_CODEC_ID_H264];
const H264_VP8: &[ffi::AVCodecID] = &[ffi::AV_CODEC_ID_H264, ffi::AV_CODEC_ID_VP8];
const H264_HEVC_VP9: &[ffi::AVCodecID] = &[
    ffi::AV_CODEC_ID_H264,
    ffi::AV_CODEC_ID_HEVC,
    ffi::AV_CODEC_ID_VP9,
];
const H264_VP9: &[ffi::AVCodecID] = &[ffi::AV_CODEC_ID_H264, ffi::AV_CODEC_ID_VP9];

pub const CHROMECAST_1ST_GEN: StreamingDevice = StreamingDevice {
    name: "Chromecast (1st gen)",
    model: "chromecast_1st_gen",
    maker: "Google",
    family: DeviceFamily::Chromecast,
    containers: CHROMECAST_CONTAINERS,
    video_codecs: H264_ONLY,
    audio_codecs: CHROMECAST_AUDIO,
    max_h264_profile: H264Profile::High,
    max_h264_level: H264Level::Level4_1,
    max_fps: 30,
    max_resolution: Resolution::Resolution1080p,
    max_video_bitrate: Some(20_000_000),
    max_audio_bitrate: Some(320_000),
};

pub const CHROMECAST_2ND_GEN: StreamingDevice = StreamingDevice {
    name: "Chromecast (2nd gen)",
    model: "chromecast_2nd_gen",
    maker: "Google",
    family: DeviceFamily::Chromecast,
    containers: CHROMECAST_CONTAINERS,
    video_codecs: H264_ONLY,
    audio_codecs: CHROMECAST_AUDIO,
    max_h264_profile: H264Profile::High,
    max_h264_level: H264Level::Level4_1,
    max_fps: 60,
    max_resolution: Resolution::Resolution1080p,
    max_video_bitrate: Some(20_000_000),
    max_audio_bitrate: Some(320_000),
};

pub const CHROMECAST_3RD_GEN: StreamingDevice = StreamingDevice {
    name: "Chromecast (3rd gen)",
    model: "chromecast_3rd_gen",
    maker: "Google",
    family: DeviceFamily::Chromecast,
    containers: CHROMECAST_CONTAINERS,
    video_codecs: H264_VP8,
    audio_codecs: CHROMECAST_AUDIO,
    max_h264_profile: H264Profile::High,
    max_h264_level: H264Level::Level4_2,
    max_fps: 60,
    max_resolution: Resolution::Resolution1080p,
    max_video_bitrate: Some(25_000_000),
    max_audio_bitrate: Some(320_000),
};

pub const CHROMECAST_ULTRA: StreamingDevice = StreamingDevice {
    name: "Chromecast Ultra",
    model: "chromecast_ultra",
    maker: "Google",
    family: DeviceFamily::Chromecast,
    containers: CHROMECAST_CONTAINERS,
    video_codecs: H264_VP8,
    audio_codecs: CHROMECAST_AUDIO,
    max_h264_profile: H264Profile::High,
    max_h264_level: H264Level::Level4_2,
    max_fps: 60,
    max_resolution: Resolution::Resolution2160p,
    max_video_bitrate: Some(35_000_000),
    max_audio_bitrate: Some(320_000),
};

pub const CHROMECAST_GOOGLE_TV: StreamingDevice = StreamingDevice {
    name: "Chromecast with Google TV",
    model: "chromecast_google_tv",
    maker: "Google",
    family: DeviceFamily::Chromecast,
    containers: CHROMECAST_CONTAINERS,
    video_codecs: H264_HEVC_VP9,
    audio_codecs: CHROMECAST_AUDIO,
    max_h264_profile: H264Profile::High,
    max_h264_level: H264Level::Level5_1,
    max_fps: 60,
    max_resolution: Resolution::Resolution2160p,
    max_video_bitrate: Some(45_000_000),
    max_audio_bitrate: Some(320_000),
};

pub const GOOGLE_TV_STREAMER: StreamingDevice = StreamingDevice {
    name: "Google TV Streamer",
    model: "google_tv_streamer",
    maker: "Google",
    family: DeviceFamily::Chromecast,
    containers: CHROMECAST_CONTAINERS,
    video_codecs: H264_HEVC_VP9,
    audio_codecs: CHROMECAST_AUDIO,
    max_h264_profile: H264Profile::High,
    max_h264_level: H264Level::Level5_2,
    max_fps: 60,
    max_resolution: Resolution::Resolution2160p,
    max_video_bitrate: Some(45_000_000),
    max_audio_bitrate: Some(320_000),
};

pub const NEST_HUB: StreamingDevice = StreamingDevice {
    name: "Nest Hub",
    model: "nest_hub",
    maker: "Google",
    family: DeviceFamily::Chromecast,
    containers: CHROMECAST_CONTAINERS,
    video_codecs: H264_VP9,
    audio_codecs: CHROMECAST_AUDIO,
    max_h264_profile: H264Profile::High,
    max_h264_level: H264Level::Level4_1,
    max_fps: 60,
    max_resolution: Resolution::Resolution720p,
    max_video_bitrate: Some(12_000_000),
    max_audio_bitrate: Some(256_000),
};

pub const NEST_HUB_MAX: StreamingDevice = StreamingDevice {
    name: "Nest Hub Max",
    model: "nest_hub_max",
    maker: "Google",
    family: DeviceFamily::Chromecast,
    containers: CHROMECAST_CONTAINERS,
    video_codecs: H264_VP9,
    audio_codecs: CHROMECAST_AUDIO,
    max_h264_profile: H264Profile::High,
    max_h264_level: H264Level::Level4_1,
    max_fps: 30,
    max_resolution: Resolution::Resolution720p,
    max_video_bitrate: Some(12_000_000),
    max_audio_bitrate: Some(256_000),
};

pub const CHROMECAST_DEVICES: &[StreamingDevice] = &[
    CHROMECAST_1ST_GEN,
    CHROMECAST_2ND_GEN,
    CHROMECAST_3RD_GEN,
    CHROMECAST_ULTRA,
    CHROMECAST_GOOGLE_TV,
    GOOGLE_TV_STREAMER,
    NEST_HUB,
    NEST_HUB_MAX,
];
