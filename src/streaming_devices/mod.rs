pub(crate) mod streaming_device;

use rsmpeg::ffi::{self};
pub use streaming_device::{Resolution, StreamingDevice};

// get all Streaming Devices
pub const STREAMING_DEVICES: [StreamingDevice; 3] = [
    // Chromecast (1st gen)
    StreamingDevice {
        name: "Chromecast (1st gen)",
        model: "chromecast_1st_gen",
        maker: "Google",
        video_codec: ffi::AV_CODEC_ID_H264,
        audio_codec: ffi::AV_CODEC_ID_AAC,
        max_h264_profile: "High",
        max_h264_level: 4.1,
        max_fps: 30,
        max_resolution: Resolution::Resolution1080p,
    },
    // Chromecast (2nd gen)
    StreamingDevice {
        name: "Chromecast (2nd gen)",
        model: "chromecast_2nd_gen",
        maker: "Google",
        video_codec: ffi::AV_CODEC_ID_H264,
        audio_codec: ffi::AV_CODEC_ID_AAC,
        max_h264_profile: "High",
        max_h264_level: 4.1,
        max_fps: 60,
        max_resolution: Resolution::Resolution1080p,
    },
    // Chromecast Ultra
    StreamingDevice {
        name: "Chromecast Ultra",
        model: "chromecast_ultra",
        maker: "Google",
        video_codec: ffi::AV_CODEC_ID_H264,
        audio_codec: ffi::AV_CODEC_ID_AAC,
        max_h264_profile: "High",
        max_h264_level: 4.2,
        max_fps: 60,
        max_resolution: Resolution::Resolution2160p,
    },
];
