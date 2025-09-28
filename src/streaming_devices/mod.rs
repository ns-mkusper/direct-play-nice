pub(crate) mod streaming_device;

use rsmpeg::ffi::{self};
pub use streaming_device::{H264Level, H264Profile, Resolution, StreamingDevice};

/// Pseudo device selector to allow `-s all` on the CLI.
/// This is not included in `STREAMING_DEVICES` and should be expanded to
/// the full list by the CLI layer before computing intersections.
pub const ALL_SELECTOR: StreamingDevice = StreamingDevice {
    name: "All Devices (selector)",
    model: "all",
    maker: "<selector>",
    video_codec: [None, None, None, None, None],
    audio_codec: [None, None, None, None, None],
    max_h264_profile: H264Profile::High,
    max_h264_level: H264Level::Level5_2,
    max_fps: 120,
    max_resolution: Resolution::Resolution2160p,
};

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
