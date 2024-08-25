pub(crate) mod streaming_device;

use rsmpeg::ffi::{self};
pub use streaming_device::{H264Level, H264Profile, Resolution, StreamingDevice};

/// All supported `StreamingDevices`
pub const STREAMING_DEVICES: &[StreamingDevice; 3] = &[
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
    // Chromecast Ultra
    StreamingDevice {
        name: "Chromecast Ultra",
        model: "chromecast_ultra",
        maker: "Google",
        video_codec: [Some(ffi::AV_CODEC_ID_H264), None, None, None, None],
        audio_codec: [Some(ffi::AV_CODEC_ID_AAC), None, None, None, None],
        max_h264_profile: H264Profile::High,
        max_h264_level: H264Level::Level4_2,
        max_fps: 60,
        max_resolution: Resolution::Resolution2160p,
    },
];
