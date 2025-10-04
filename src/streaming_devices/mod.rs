pub(crate) mod streaming_device;

use rsmpeg::ffi::{self};
pub use streaming_device::{H264Level, H264Profile, Resolution, StreamingDevice};

/// All supported `StreamingDevices`
pub const STREAMING_DEVICES: &[StreamingDevice; 8] = &[
    // Google Cast
    // see: https://developers.google.com/cast/docs/media

    // Chromecast (1st gen)
    StreamingDevice {
        name: "Chromecast (1st gen)",
        model: "chromecast_1st_gen",
        maker: "Google",
        video_codec: [Some(ffi::AV_CODEC_ID_H264), None, None, None, None],
        audio_codec: [Some(ffi::AV_CODEC_ID_AAC), None, None, None, None],
        max_h264_profile: H264Profile::High,
        max_h264_level: H264Level::Level4_1,
        max_fps: 30,
        max_resolution: Resolution::Resolution1080p,
    },
    // Chromecast (2nd gen)
    StreamingDevice {
        name: "Chromecast (2nd gen)",
        model: "chromecast_2nd_gen",
        maker: "Google",
        video_codec: [Some(ffi::AV_CODEC_ID_H264), None, None, None, None],
        audio_codec: [Some(ffi::AV_CODEC_ID_AAC), None, None, None, None],
        max_h264_profile: H264Profile::High,
        max_h264_level: H264Level::Level4_1,
        max_fps: 60,
        max_resolution: Resolution::Resolution1080p,
    },
    // Chromecast (3rd gen)
    StreamingDevice {
        name: "Chromecast (3rd gen)",
        model: "chromecast_3rd_gen",
        maker: "Google",
        video_codec: [
            Some(ffi::AV_CODEC_ID_H264),
            Some(ffi::AV_CODEC_ID_VP8),
            None,
            None,
            None,
        ],
        audio_codec: [Some(ffi::AV_CODEC_ID_AAC), None, None, None, None],
        max_h264_profile: H264Profile::High,
        max_h264_level: H264Level::Level4_2,
        max_fps: 60,
        max_resolution: Resolution::Resolution1080p,
    },
    // Chromecast Ultra
    StreamingDevice {
        name: "Chromecast Ultra",
        model: "chromecast_ultra",
        maker: "Google",
        video_codec: [
            Some(ffi::AV_CODEC_ID_H264),
            Some(ffi::AV_CODEC_ID_VP8),
            None,
            None,
            None,
        ],
        audio_codec: [Some(ffi::AV_CODEC_ID_AAC), None, None, None, None],
        max_h264_profile: H264Profile::High,
        max_h264_level: H264Level::Level4_2,
        max_fps: 60,
        max_resolution: Resolution::Resolution2160p,
    },
    // Chromecast with Google TV
    StreamingDevice {
        name: "Chromecast with Google TV",
        model: "chromecast_google_tv",
        maker: "Google",
        video_codec: [
            Some(ffi::AV_CODEC_ID_H264),
            Some(ffi::AV_CODEC_ID_HEVC),
            Some(ffi::AV_CODEC_ID_VP9),
            None,
            None,
        ],
        audio_codec: [Some(ffi::AV_CODEC_ID_AAC), None, None, None, None],
        max_h264_profile: H264Profile::High,
        max_h264_level: H264Level::Level5_1,
        max_fps: 30,
        max_resolution: Resolution::Resolution2160p,
    },
    // Google TV Streamer
    StreamingDevice {
        name: "Google TV Streamer",
        model: "google_tv_streamer",
        maker: "Google",
        video_codec: [
            Some(ffi::AV_CODEC_ID_H264),
            Some(ffi::AV_CODEC_ID_HEVC),
            Some(ffi::AV_CODEC_ID_VP9),
            None,
            None,
        ],
        audio_codec: [Some(ffi::AV_CODEC_ID_AAC), None, None, None, None],
        max_h264_profile: H264Profile::High,
        max_h264_level: H264Level::Level5_2,
        max_fps: 60,
        max_resolution: Resolution::Resolution2160p,
    },
    // Nest Hub
    StreamingDevice {
        name: "Nest Hub",
        model: "nest_hub",
        maker: "Google",
        video_codec: [
            Some(ffi::AV_CODEC_ID_H264),
            Some(ffi::AV_CODEC_ID_VP9),
            None,
            None,
            None,
        ],
        audio_codec: [Some(ffi::AV_CODEC_ID_AAC), None, None, None, None],
        max_h264_profile: H264Profile::High,
        max_h264_level: H264Level::Level4_1,
        max_fps: 60,
        max_resolution: Resolution::Resolution720p,
    },
    // Nest Hub Max
    StreamingDevice {
        name: "Nest Hub Max",
        model: "nest_hub_max",
        maker: "Google",
        video_codec: [
            Some(ffi::AV_CODEC_ID_H264),
            Some(ffi::AV_CODEC_ID_VP9),
            None,
            None,
            None,
        ],
        audio_codec: [Some(ffi::AV_CODEC_ID_AAC), None, None, None, None],
        max_h264_profile: H264Profile::High,
        max_h264_level: H264Level::Level4_1,
        max_fps: 30,
        max_resolution: Resolution::Resolution720p,
    },
];
