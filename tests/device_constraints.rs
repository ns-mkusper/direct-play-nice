use direct_play_nice::devices::{
    self, plan_output_profile, resolve_target_profile, ContainerFormat, DeviceFamily,
    InputMediaProfile, StreamingDevice,
};
use rsmpeg::ffi;

fn min_video_limit(devices: &[&StreamingDevice]) -> Option<i64> {
    devices.iter().filter_map(|d| d.max_video_bitrate).min()
}

fn min_audio_limit(devices: &[&StreamingDevice]) -> Option<i64> {
    devices.iter().filter_map(|d| d.max_audio_bitrate).min()
}

#[test]
fn test_chromecast_constraints() {
    let selected = devices::devices_for_family(DeviceFamily::Chromecast);
    let input = InputMediaProfile {
        container: Some(ContainerFormat::Mkv),
        video_codec: ffi::AV_CODEC_ID_AV1,
        audio_codec: ffi::AV_CODEC_ID_AC3,
        video_bitrate: Some(80_000_000),
        audio_bitrate: Some(1_500_000),
    };

    let planned = plan_output_profile(&selected, &input).expect("plan should resolve");
    assert_eq!(planned.container, ContainerFormat::Mp4);
    assert_eq!(planned.video_codec, ffi::AV_CODEC_ID_H264);
    assert_eq!(planned.audio_codec, ffi::AV_CODEC_ID_AAC);
    assert!(planned.target_video_bitrate <= min_video_limit(&selected));
    assert!(planned.target_audio_bitrate <= min_audio_limit(&selected));
}

#[test]
fn test_roku_constraints() {
    let selected = devices::devices_for_family(DeviceFamily::Roku);
    let input = InputMediaProfile {
        container: Some(ContainerFormat::Mkv),
        video_codec: ffi::AV_CODEC_ID_HEVC,
        audio_codec: ffi::AV_CODEC_ID_EAC3,
        video_bitrate: Some(90_000_000),
        audio_bitrate: Some(2_000_000),
    };

    let planned = plan_output_profile(&selected, &input).expect("plan should resolve");
    assert_eq!(planned.container, ContainerFormat::Mkv);
    assert_eq!(planned.video_codec, ffi::AV_CODEC_ID_HEVC);
    assert_eq!(planned.audio_codec, ffi::AV_CODEC_ID_EAC3);
    assert!(planned.target_video_bitrate <= min_video_limit(&selected));
    assert!(planned.target_audio_bitrate <= min_audio_limit(&selected));
}

#[test]
fn test_apple_tv_constraints() {
    let selected = devices::devices_for_family(DeviceFamily::AppleTv);
    let input = InputMediaProfile {
        container: Some(ContainerFormat::Mkv),
        video_codec: ffi::AV_CODEC_ID_HEVC,
        audio_codec: ffi::AV_CODEC_ID_EAC3,
        video_bitrate: Some(70_000_000),
        audio_bitrate: Some(2_000_000),
    };

    let planned = plan_output_profile(&selected, &input).expect("plan should resolve");
    assert_eq!(planned.container, ContainerFormat::Mp4);
    assert_eq!(planned.video_codec, ffi::AV_CODEC_ID_H264);
    assert_eq!(planned.audio_codec, ffi::AV_CODEC_ID_EAC3);
    assert!(planned.target_video_bitrate <= min_video_limit(&selected));
    assert!(planned.target_audio_bitrate <= min_audio_limit(&selected));
}

#[test]
fn test_fire_tv_constraints() {
    let selected = devices::devices_for_family(DeviceFamily::FireTv);
    let input = InputMediaProfile {
        container: Some(ContainerFormat::Mkv),
        video_codec: ffi::AV_CODEC_ID_AV1,
        audio_codec: ffi::AV_CODEC_ID_FLAC,
        video_bitrate: Some(80_000_000),
        audio_bitrate: Some(2_000_000),
    };

    let planned = plan_output_profile(&selected, &input).expect("plan should resolve");
    assert_eq!(planned.container, ContainerFormat::Mkv);
    assert_eq!(planned.video_codec, ffi::AV_CODEC_ID_AV1);
    assert_eq!(planned.audio_codec, ffi::AV_CODEC_ID_FLAC);
    assert!(planned.target_video_bitrate <= min_video_limit(&selected));
    assert!(planned.target_audio_bitrate <= min_audio_limit(&selected));
}

#[test]
fn test_all_devices_constraints() {
    let selected: Vec<&StreamingDevice> = devices::STREAMING_DEVICES.iter().collect();
    let all_profile = resolve_target_profile(&selected).expect("all profile should resolve");

    for device in devices::STREAMING_DEVICES {
        assert!(
            all_profile.is_compatible_with_device(device),
            "all-device profile must be valid for {}",
            device.model
        );
    }

    let input = InputMediaProfile {
        container: Some(ContainerFormat::Mkv),
        video_codec: ffi::AV_CODEC_ID_AV1,
        audio_codec: ffi::AV_CODEC_ID_DTS,
        video_bitrate: Some(90_000_000),
        audio_bitrate: Some(2_000_000),
    };
    let planned = plan_output_profile(&selected, &input).expect("plan should resolve");

    for device in devices::STREAMING_DEVICES {
        assert!(
            device.containers.contains(&planned.container),
            "planned container must be valid for {}",
            device.model
        );
        assert!(
            device.video_codecs.contains(&planned.video_codec),
            "planned video codec must be valid for {}",
            device.model
        );
        assert!(
            device.audio_codecs.contains(&planned.audio_codec),
            "planned audio codec must be valid for {}",
            device.model
        );
        if let (Some(target), Some(limit)) =
            (planned.target_video_bitrate, device.max_video_bitrate)
        {
            assert!(
                target <= limit,
                "video bitrate target exceeds device limit for {}",
                device.model
            );
        }
        if let (Some(target), Some(limit)) =
            (planned.target_audio_bitrate, device.max_audio_bitrate)
        {
            assert!(
                target <= limit,
                "audio bitrate target exceeds device limit for {}",
                device.model
            );
        }
    }
}
