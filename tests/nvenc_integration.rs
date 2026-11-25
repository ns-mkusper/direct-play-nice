#![cfg(feature = "ffmpeg-cli-tests")]

#[path = "common/mod.rs"]
mod common;

use common::frame_order::{check_scene_frames, create_scene_cut_clip, decoded_scene_frames};
use std::error::Error;
use std::fs;
use std::path::Path;
use std::process::Command;

use tempfile::tempdir;

fn nvenc_tests_enabled() -> bool {
    matches!(
        std::env::var("ENABLE_NVENC_TESTS").ok().as_deref(),
        Some("1")
    )
}

fn tool_available(binary: &str, args: &[&str]) -> bool {
    Command::new(binary)
        .args(args)
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

fn ffmpeg_has_nvenc() -> bool {
    if let Ok(output) = Command::new("ffmpeg")
        .args(["-hide_banner", "-encoders"])
        .output()
    {
        if output.status.success() {
            return String::from_utf8_lossy(&output.stdout).contains("h264_nvenc");
        }
    }
    false
}

fn create_input_clip(path: &Path) -> Result<(), Box<dyn Error>> {
    let status = Command::new("ffmpeg")
        .args([
            "-y",
            "-f",
            "lavfi",
            "-i",
            "testsrc=size=1920x1080:rate=30:duration=2",
            "-f",
            "lavfi",
            "-i",
            "sine=frequency=1000:duration=2",
            "-c:v",
            "libx264",
            "-preset",
            "ultrafast",
            "-pix_fmt",
            "yuv420p",
            "-c:a",
            "aac",
            "-shortest",
            path.to_str().unwrap(),
        ])
        .status()?;
    if !status.success() {
        Err("ffmpeg could not create test clip".into())
    } else {
        Ok(())
    }
}

fn run_nvenc_conversion(input: &Path, output: &Path) -> Result<(String, String), Box<dyn Error>> {
    let exe = env!("CARGO_BIN_EXE_direct_play_nice");
    let output = Command::new(exe)
        .args([
            "-s",
            "chromecast_2nd_gen",
            "--hw-accel=nvenc",
            input.to_str().unwrap(),
            output.to_str().unwrap(),
        ])
        .output()?;
    if !output.status.success() {
        return Err(format!(
            "direct_play_nice failed: stdout=\n{}\nstderr=\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        )
        .into());
    }
    Ok((
        String::from_utf8_lossy(&output.stdout).to_string(),
        String::from_utf8_lossy(&output.stderr).to_string(),
    ))
}

fn probe_output(path: &Path) -> Result<String, Box<dyn Error>> {
    let output = Command::new("ffprobe")
        .args([
            "-v",
            "error",
            "-select_streams",
            "v:0",
            "-show_entries",
            "stream=codec_name,profile,level:stream_tags=encoder:format_tags=encoder",
            "-of",
            "default=nw=1",
            path.to_str().unwrap(),
        ])
        .output()?;
    if !output.status.success() {
        Err("ffprobe failed to inspect output".into())
    } else {
        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }
}

#[test]
fn nvenc_native_h264_cuda_preserves_scene_order() -> Result<(), Box<dyn Error>> {
    if !nvenc_tests_enabled() {
        eprintln!("skipping NVENC integration test (set ENABLE_NVENC_TESTS=1 to enable)");
        return Ok(());
    }
    // Once explicitly enabled, missing tools or hardware must be failures, not skips.
    assert!(
        tool_available("ffmpeg", &["-version"]),
        "ffmpeg is required"
    );
    assert!(
        tool_available("ffprobe", &["-version"]),
        "ffprobe is required"
    );
    assert!(ffmpeg_has_nvenc(), "ffmpeg must support h264_nvenc");

    let dir = tempdir()?;
    let input = dir.path().join("scene-cuts.mkv");
    create_scene_cut_clip(&input)?;

    let probe = Command::new("ffprobe")
        .args([
            "-v",
            "error",
            "-select_streams",
            "v:0",
            "-show_entries",
            "frame=pict_type",
            "-of",
            "csv=p=0",
        ])
        .arg(&input)
        .output()?;
    assert!(probe.status.success(), "ffprobe could not inspect fixture");
    assert!(
        String::from_utf8_lossy(&probe.stdout)
            .lines()
            .any(|line| line.trim().starts_with('B')),
        "fixture must contain B-frames"
    );

    for backend in ["software", "auto"] {
        let output_path = dir.path().join(format!("scene-cuts-{backend}.mp4"));
        let conversion = Command::new(env!("CARGO_BIN_EXE_direct_play_nice"))
            .env("HOME", dir.path())
            .env("XDG_CONFIG_HOME", dir.path().join("config"))
            .env_remove("DIRECT_PLAY_NICE_CONFIG")
            .env("RUST_LOG", "trace")
            .args([
                "-s",
                "chromecast_2nd_gen",
                "--hw-accel=nvenc",
                "--video-quality=360p",
                "--resize-backend",
                backend,
                "--delete-source",
                "false",
            ])
            .arg(&input)
            .arg(&output_path)
            .output()?;
        let logs = format!(
            "stdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&conversion.stdout),
            String::from_utf8_lossy(&conversion.stderr)
        );
        assert!(conversion.status.success(), "conversion failed: {logs}");
        let source = decoded_scene_frames(&input)?;
        let converted = decoded_scene_frames(&output_path)?;
        check_scene_frames(&source, &converted)
            .map_err(|message| format!("{backend} transcode: {message}"))?;
        common::assert_video_timestamps_match(&input, &output_path, 0.025);
        common::assert_audio_packet_range_matches(&input, &output_path, 0.1);

        for expected in [
            "Selected decoder 'h264' for stream 0 (codec h264)",
            "Configured CUDA hardware decoder 'h264' for stream 0",
            "Decoder 'h264' ready for stream 0 (hardware=true)",
        ] {
            assert!(
                logs.contains(expected),
                "{backend}: missing '{expected}' in {logs}"
            );
        }
        let resize_evidence: &[&str] = if backend == "auto" {
            &[
                "Resize backend selected: cuda (scale_cuda",
                "Initialized CUDA resize filter graph: 960x540 -> 640x360 via scale_cuda",
                "Submitting CUDA frame 960x540 to scale_cuda resize graph",
            ]
        } else {
            &[
                "Resize backend selected: software (libswscale)",
                "Transferred CUDA frame 960x540 from cuda",
            ]
        };
        for expected in resize_evidence {
            assert!(
                logs.contains(expected),
                "{backend}: missing '{expected}' in {logs}"
            );
        }
        if backend == "auto" {
            assert!(
                !logs.contains("Resize backend selected: software")
                    && !logs
                        .to_ascii_lowercase()
                        .contains("falling back to software"),
                "auto resize fell back to software: {logs}"
            );
        }
        assert!(
            logs.lines()
                .any(|line| line.contains("Video encoder selected: hardware")
                    && line.contains("h264_nvenc")),
            "NVENC encoder was not selected: {logs}"
        );
        assert!(
            !logs.to_ascii_lowercase().contains("retrying with software"),
            "conversion retried on CPU: {logs}"
        );
    }
    Ok(())
}

#[test]
fn nvenc_unsupported_h264_retries_only_auto_resize() -> Result<(), Box<dyn Error>> {
    if !nvenc_tests_enabled()
        || std::env::var("DPN_TEST_UNSUPPORTED_H264").ok().as_deref() != Some("1")
    {
        eprintln!("skipping unsupported H.264 GPU test (set ENABLE_NVENC_TESTS=1 and DPN_TEST_UNSUPPORTED_H264=1)");
        return Ok(());
    }
    assert!(
        tool_available("ffmpeg", &["-version"]),
        "ffmpeg is required"
    );
    assert!(
        tool_available("ffprobe", &["-version"]),
        "ffprobe is required"
    );
    assert!(ffmpeg_has_nvenc(), "ffmpeg must support h264_nvenc");

    let dir = tempdir()?;
    let input = dir.path().join("high10.mkv");
    let generated = Command::new("ffmpeg")
        .args([
            "-hide_banner",
            "-loglevel",
            "error",
            "-y",
            "-f",
            "lavfi",
            "-i",
            "testsrc2=size=960x540:rate=24:duration=2",
            "-f",
            "lavfi",
            "-i",
            "sine=frequency=1000:sample_rate=48000:duration=2",
            "-c:v",
            "libx264",
            "-profile:v",
            "high10",
            "-pix_fmt",
            "yuv420p10le",
            "-bf",
            "3",
            "-g",
            "48",
            "-c:a",
            "aac",
        ])
        .arg(&input)
        .output()?;
    assert!(
        generated.status.success(),
        "High 10 fixture generation failed: {}",
        String::from_utf8_lossy(&generated.stderr)
    );
    let fixture_probe = Command::new("ffprobe")
        .args([
            "-v",
            "error",
            "-select_streams",
            "v:0",
            "-show_entries",
            "stream=codec_name,profile,pix_fmt",
            "-of",
            "default=nw=1",
        ])
        .arg(&input)
        .output()?;
    assert!(fixture_probe.status.success(), "fixture probe failed");
    let fixture_info = String::from_utf8_lossy(&fixture_probe.stdout);
    for expected in ["codec_name=h264", "profile=High 10", "pix_fmt=yuv420p10le"] {
        assert!(
            fixture_info.contains(expected),
            "missing {expected}: {fixture_info}"
        );
    }

    const RETRY: &str = "CUDA decoding fell back to software; retrying with software resize";
    for (codec, encoder) in [("h264", "h264_nvenc"), ("hevc", "hevc_nvenc")] {
        for backend in ["auto", "cuda"] {
            let output_path = dir.path().join(format!("{codec}-{backend}.mp4"));
            let result = Command::new(env!("CARGO_BIN_EXE_direct_play_nice"))
                .env("HOME", dir.path())
                .env("XDG_CONFIG_HOME", dir.path().join("config"))
                .env_remove("DIRECT_PLAY_NICE_CONFIG")
                .env("RUST_LOG", "debug")
                .args([
                    "--device",
                    "apple_tv_4k_3rd_gen",
                    "--hw-accel",
                    "nvenc",
                    "--video-codec",
                    codec,
                    "--video-quality",
                    "360p",
                    "--resize-backend",
                    backend,
                    "--delete-source",
                    "false",
                ])
                .arg(&input)
                .arg(&output_path)
                .output()?;
            let logs = format!(
                "stdout:\n{}\nstderr:\n{}",
                String::from_utf8_lossy(&result.stdout),
                String::from_utf8_lossy(&result.stderr)
            );
            let case = format!("{codec} / {backend}");
            for expected in [
                "Selected decoder 'h264' for stream 0 (codec h264)",
                "Configured CUDA hardware decoder 'h264' for stream 0",
                "Decoder 'h264' ready for stream 0 (hardware=true)",
                "Resize backend selected: cuda",
            ] {
                assert!(
                    logs.contains(expected),
                    "{case}: missing '{expected}': {logs}"
                );
            }
            if backend == "auto" {
                assert!(result.status.success(), "{case}: conversion failed: {logs}");
                let (_, retry_logs) = logs
                    .split_once(RETRY)
                    .unwrap_or_else(|| panic!("{case}: software resize retry missing: {logs}"));
                assert!(
                    !retry_logs.contains(RETRY),
                    "{case}: repeated resize retry: {logs}"
                );
                assert!(
                    retry_logs.contains("Resize backend selected: software (libswscale)"),
                    "{case}: retry did not use software resize: {logs}"
                );
                assert!(
                    retry_logs
                        .lines()
                        .any(|line| line.contains("Video encoder selected: hardware")
                            && line.contains(encoder)),
                    "{case}: retry did not retain {encoder}: {logs}"
                );
                assert!(
                    !retry_logs
                        .to_ascii_lowercase()
                        .contains("retrying with software encoder"),
                    "{case}: encoder fell back to software: {logs}"
                );
                let probe = probe_output(&output_path)?;
                assert!(
                    probe
                        .lines()
                        .any(|line| line == format!("codec_name={codec}")),
                    "{case}: unexpected output codec: {probe}"
                );
                common::assert_video_timestamps_match(&input, &output_path, 0.025);
            } else {
                assert!(
                    !result.status.success(),
                    "{case}: explicit CUDA unexpectedly succeeded: {logs}"
                );
                assert!(
                    logs.contains(
                        "CUDA resize requires CUDA input frames, but the decoder returned format"
                    ),
                    "{case}: expected non-CUDA frame error: {logs}"
                );
                assert!(
                    !logs.contains(RETRY),
                    "{case}: explicit CUDA retried: {logs}"
                );
                assert!(
                    !logs.contains("Resize backend selected: software")
                        && !logs
                            .to_ascii_lowercase()
                            .contains("retrying with software encoder"),
                    "{case}: explicit CUDA used a software fallback: {logs}"
                );
            }
            assert!(input.exists(), "{case}: source was deleted");
        }
    }
    Ok(())
}

#[test]
fn nvenc_h264_profile_and_level_are_correct() -> Result<(), Box<dyn Error>> {
    if !nvenc_tests_enabled() {
        eprintln!("skipping NVENC integration test (set ENABLE_NVENC_TESTS=1 to enable)");
        return Ok(());
    }
    if !tool_available("ffmpeg", &["-version"]) || !tool_available("ffprobe", &["-version"]) {
        eprintln!("skipping NVENC integration test because ffmpeg/ffprobe not found");
        return Ok(());
    }
    if !ffmpeg_has_nvenc() {
        eprintln!("skipping NVENC integration test because h264_nvenc is unavailable");
        return Ok(());
    }

    let dir = tempdir()?;
    let input_path = dir.path().join("input.mkv");
    let output_path = dir.path().join("output.mp4");

    create_input_clip(&input_path)?;
    let (stdout_log, stderr_log) = run_nvenc_conversion(&input_path, &output_path)?;

    assert!(
        stdout_log.contains("Video encoder selected: hardware")
            || stderr_log.contains("Video encoder selected: hardware"),
        "expected hardware encoder selection, got stdout=\n{}\nstderr=\n{}",
        stdout_log,
        stderr_log
    );
    assert!(
        !stdout_log.contains("NVENC initialization failed")
            && !stderr_log.contains("NVENC initialization failed"),
        "NVENC initialization unexpectedly failed; stdout=\n{}\nstderr=\n{}",
        stdout_log,
        stderr_log
    );

    assert!(output_path.exists(), "expected output file to exist");

    let probe = probe_output(&output_path)?;
    let mut codec_name = None;
    let mut profile = None;
    let mut level = None;
    let mut encoder_tag = None;

    for line in probe.lines() {
        if let Some(v) = line.strip_prefix("codec_name=") {
            codec_name = Some(v.trim().to_string());
        } else if let Some(v) = line.strip_prefix("profile=") {
            profile = Some(v.trim().to_string());
        } else if let Some(v) = line.strip_prefix("level=") {
            level = Some(v.trim().to_string());
        } else if let Some(v) = line.strip_prefix("TAG:encoder=") {
            if encoder_tag.is_none() {
                encoder_tag = Some(v.trim().to_string());
            }
        }
    }

    let codec_name = codec_name.ok_or("missing codec_name from ffprobe")?;
    assert_eq!(
        codec_name, "h264",
        "unexpected codec name: {}\n{}",
        codec_name, probe
    );

    let profile = profile.ok_or("missing profile from ffprobe")?;
    assert!(
        profile.to_lowercase().contains("high"),
        "expected High profile, got {}.\n{}",
        profile,
        probe
    );

    let level_value = level.ok_or("missing level from ffprobe")?;
    let level_num: i32 = level_value.parse().unwrap_or(-1);
    assert!(
        level_num > 0 && level_num <= 41,
        "expected Level <= 41, got {}.\n{}",
        level_num,
        probe
    );

    let encoder_tag = encoder_tag.ok_or("missing encoder tag from ffprobe")?;
    assert!(
        encoder_tag.to_lowercase().contains("nvenc"),
        "expected NVENC encoder tag, got {}.\n{}",
        encoder_tag,
        probe
    );

    let metadata = fs::metadata(&output_path)?;
    assert!(metadata.len() > 0, "output file is empty");

    Ok(())
}
