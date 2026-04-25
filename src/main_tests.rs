#[cfg(test)]
mod rate_limit_tests {
    use crate::*;

    #[test]
    fn level_4_1_limits_match_table() {
        let limits = h264_high_profile_rate_limits(H264Level::Level4_1).unwrap();
        assert_eq!(limits.max_bitrate_bits, 62_500_000);
        assert_eq!(limits.max_buffer_bits, 62_500_000);
    }
}

#[cfg(test)]
mod video_tests {
    use crate::*;
    use crate::devices::DeviceFamily;
    use std::process::Command;
    use tempfile::tempdir;

    fn ffmpeg_present() -> bool {
        let out = Command::new("ffmpeg").arg("-version").output();
        matches!(out, Ok(o) if o.status.success())
    }

    #[test]
    fn profile_option_applies_to_supported_encoders() {
        assert!(should_apply_profile_option("libx264"));
        assert!(should_apply_profile_option("LIBX264"));
        assert!(should_apply_profile_option("h264_nvenc"));
        assert!(should_apply_profile_option("H264_NVENC"));
        assert!(!should_apply_profile_option("amf_h264"));
    }

    #[test]
    fn enforce_h264_constraints_sets_target_profile_and_level_for_nvenc() {
        let codec = AVCodec::find_encoder(ffi::AV_CODEC_ID_H264).expect("libx264 missing");
        let mut ctx = AVCodecContext::new(&codec);
        enforce_h264_constraints(
            &mut ctx,
            H264Profile::High,
            H264Level::Level4_1,
            "h264_nvenc",
        );
        assert_eq!(ctx.profile, H264Profile::High as i32);
        assert_eq!(ctx.level, H264Level::Level4_1 as i32);
    }

    #[test]
    fn enforce_h264_constraints_sets_target_profile_and_level_for_x264() {
        let codec = AVCodec::find_encoder(ffi::AV_CODEC_ID_H264).expect("libx264 missing");
        let mut ctx = AVCodecContext::new(&codec);
        enforce_h264_constraints(&mut ctx, H264Profile::High, H264Level::Level4, "libx264");
        assert_eq!(ctx.profile, H264Profile::High as i32);
        assert_eq!(ctx.level, H264Level::Level4 as i32);
    }

    #[test]
    fn nvenc_rate_controls_obey_level_limits() {
        let codec = AVCodec::find_encoder(ffi::AV_CODEC_ID_H264).expect("libx264 missing");
        let mut ctx = AVCodecContext::new(&codec);
        ctx.set_bit_rate(2_000_000);
        ctx.set_width(1280);
        ctx.set_height(720);
        unsafe {
            (*ctx.as_mut_ptr()).refs = 16;
        }

        apply_hw_encoder_quality(
            ctx.as_mut_ptr(),
            "h264_nvenc",
            Some(2_000_000),
            false,
            Some(H264Level::Level4_1),
        );

        assert_eq!(ctx.rc_max_rate, 2_000_000);
        assert_eq!(ctx.rc_min_rate, 2_000_000);
        assert_eq!(ctx.rc_buffer_size, 4_000_000);
        assert_eq!(ctx.rc_initial_buffer_occupancy, 4_000_000);
        assert!(ctx.refs >= 1, "expected at least one reference frame");
    }

    #[test]
    fn level_option_values_match_encoder_type() {
        assert_eq!(
            level_option_value_for_encoder("h264_nvenc", H264Level::Level4_1),
            "4.1"
        );
        assert_eq!(
            level_option_value_for_encoder("amf_h264", H264Level::Level5_1),
            "5.1"
        );
        assert_eq!(
            level_option_value_for_encoder("libx264", H264Level::Level4_1),
            "41"
        );
    }

    #[test]
    fn h264_constraints_ignore_non_h264_streams() {
        let mut reasons = Vec::new();
        check_h264_profile_level_constraints(
            ffi::AV_CODEC_ID_HEVC,
            ffi::AV_PROFILE_UNKNOWN,
            0,
            H264Profile::High,
            H264Level::Level4_1,
            &mut reasons,
        );
        assert!(
            reasons.is_empty(),
            "expected no H.264 warnings for HEVC stream"
        );
    }

    #[test]
    fn h264_constraints_flag_out_of_bounds_profiles_and_levels() {
        let mut reasons = Vec::new();
        check_h264_profile_level_constraints(
            ffi::AV_CODEC_ID_H264,
            ffi::AV_PROFILE_H264_HIGH_444 as i32,
            H264Level::Level5_2 as i32,
            H264Profile::High,
            H264Level::Level4_1,
            &mut reasons,
        );
        assert!(
            reasons.iter().any(|reason| reason.contains("profile")),
            "expected profile violation"
        );
        assert!(
            reasons.iter().any(|reason| reason.contains("level")),
            "expected level violation"
        );
    }

    #[test]
    fn verify_output_detects_nvenc_mismatch() {
        if !ffmpeg_present() {
            eprintln!("skipping verify_output_detects_nvenc_mismatch: ffmpeg not found on PATH");
            return;
        }
        let tmp = tempdir().expect("tempdir");
        let output = tmp.path().join("nvenc_mismatch.mp4");
        let status = std::process::Command::new("ffmpeg")
            .args([
                "-y",
                "-f",
                "lavfi",
                "-i",
                "testsrc=size=1280x720:rate=23.976:duration=1",
                "-pix_fmt",
                "yuv420p",
                "-c:v",
                "libx264",
                "-profile:v",
                "main",
                "-level:v",
                "5.2",
                output.to_str().unwrap(),
            ])
            .status()
            .expect("run ffmpeg");
        assert!(status.success(), "ffmpeg profile fixture failed");

        let cstr = CString::new(output.to_string_lossy().to_string()).unwrap();
        let err = verify_output_h264_profile_level(
            cstr.as_c_str(),
            output.as_path(),
            H264Profile::High,
            H264Level::Level4_1,
            Some("h264_nvenc"),
            true,
        )
        .expect_err("expected mismatch error");
        let mismatch = err
            .downcast::<HwProfileLevelMismatch>()
            .expect("expected HwProfileLevelMismatch");
        assert_eq!(mismatch.expected_profile, H264Profile::High);
        assert_eq!(mismatch.expected_level, H264Level::Level4_1);
        assert!(matches!(mismatch.actual_profile, Some(H264Profile::Main)));
    }

    #[test]
    fn parse_new_device_models() {
        let models = devices::STREAMING_DEVICES
            .iter()
            .map(|d| d.model)
            .collect::<Vec<_>>();
        for required in [
            "chromecast_3rd_gen",
            "chromecast_google_tv",
            "google_tv_streamer",
            "nest_hub_max",
            "roku_ultra",
            "apple_tv_4k_2nd_gen",
            "fire_tv_cube_3rd_gen",
        ] {
            assert!(
                models.contains(&required),
                "STREAMING_DEVICES missing {}",
                required
            );
        }
    }

    #[test]
    fn min_level_respects_strictest_device() {
        use devices::StreamingDevice;
        let devices = devices::STREAMING_DEVICES;
        let third_gen = devices
            .iter()
            .find(|d| d.model == "chromecast_3rd_gen")
            .unwrap();
        let nest_hub = devices.iter().find(|d| d.model == "nest_hub").unwrap();
        let combo = vec![third_gen, nest_hub];
        let min_level = StreamingDevice::get_min_h264_level(&combo).unwrap();
        assert_eq!(min_level, H264Level::Level4_1);
    }

    #[test]
    fn parse_device_selection_accepts_new_models() {
        let selection = Args::parse_device_selection("google_tv_streamer").unwrap();
        match selection {
            StreamingDeviceSelection::Model(device) => {
                assert_eq!(device.model, "google_tv_streamer");
            }
            _ => panic!("Expected model selection"),
        }
    }

    #[test]
    fn parse_device_selection_accepts_families() {
        let selection = Args::parse_device_selection("roku").unwrap();
        assert!(matches!(
            selection,
            StreamingDeviceSelection::Family(DeviceFamily::Roku)
        ));
    }
}

#[cfg(test)]
mod direct_play_tests {
    use crate::*;

    #[test]
    fn rational_to_f64_handles_valid_fraction() {
        let val = rational_to_f64(ffi::AVRational {
            num: 60000,
            den: 1001,
        })
        .expect("should convert");
        // 59.94 fps typical NTSC
        assert!((val - 59.94).abs() < 0.01);
    }

    #[test]
    fn rational_to_f64_rejects_non_positive() {
        assert!(rational_to_f64(ffi::AVRational { num: 0, den: 1 }).is_none());
        assert!(rational_to_f64(ffi::AVRational { num: 1, den: 0 }).is_none());
    }
}

#[cfg(test)]
mod ocr_sidecar_tests {
    use crate::*;
    use tempfile::tempdir;

    #[test]
    fn sidecar_paths_are_stable_and_language_scoped() {
        let output = PathBuf::from("/tmp/movie.fixed.mp4");
        let p1 = sidecar_path_for_track(&output, "eng", 1);
        let p2 = sidecar_path_for_track(&output, "eng", 2);
        let p3 = sidecar_path_for_track(&output, "fra", 1);
        assert_eq!(p1, PathBuf::from("/tmp/movie.fixed.eng.srt"));
        assert_eq!(p2, PathBuf::from("/tmp/movie.fixed.eng.2.srt"));
        assert_eq!(p3, PathBuf::from("/tmp/movie.fixed.fra.srt"));
    }

    #[test]
    fn sidecar_writer_copies_only_srt_tracks() {
        let dir = tempdir().expect("tmpdir");
        let output_path = dir.path().join("sample.mp4");
        fs::write(&output_path, b"placeholder").expect("write output placeholder");

        let srt_src = dir.path().join("stream-2.srt");
        let ass_src = dir.path().join("stream-3.ass");
        fs::write(&srt_src, b"1\n00:00:00,000 --> 00:00:01,000\nhello\n\n").expect("write srt");
        fs::write(&ass_src, b"[Script Info]\n").expect("write ass");

        let tracks = vec![
            subtitle_ocr::OcrSubtitleTrack {
                language: "eng".to_string(),
                subtitle_path: srt_src.clone(),
                format: OcrFormat::Srt,
            },
            subtitle_ocr::OcrSubtitleTrack {
                language: "eng".to_string(),
                subtitle_path: ass_src,
                format: OcrFormat::Ass,
            },
        ];

        let output_cstr = CString::new(output_path.to_string_lossy().to_string()).unwrap();
        write_ocr_srt_sidecars(output_cstr.as_c_str(), &tracks, true).expect("write sidecars");

        let sidecar = dir.path().join("sample.eng.srt");
        assert!(sidecar.exists(), "expected sidecar to be written");
        let content = fs::read_to_string(sidecar).expect("read sidecar");
        assert!(content.contains("hello"));
        assert!(!dir.path().join("sample.eng.2.srt").exists());
    }
}
