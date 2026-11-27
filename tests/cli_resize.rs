#![cfg(feature = "ffmpeg-cli-tests")]

#[path = "common/mod.rs"]
mod common;

use rsmpeg::avformat::AVFormatContextInput;
use rsmpeg::ffi;
use std::ffi::CString;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

fn generate_input(dir: &Path, name: &str, size: &str) -> PathBuf {
    let input = dir.join(name);
    let video_filter = format!("testsrc2=size={size}:rate=24:duration=2");
    let status = Command::new("ffmpeg")
        .args([
            "-hide_banner",
            "-y",
            "-f",
            "lavfi",
            "-i",
            video_filter.as_str(),
            "-f",
            "lavfi",
            "-i",
            "sine=frequency=1000:sample_rate=48000:duration=2",
            "-c:v",
            "mpeg2video",
            "-pix_fmt",
            "yuv420p",
            "-b:v",
            "4M",
            "-c:a",
            "mp2",
            "-shortest",
        ])
        .arg(input.as_os_str())
        .status()
        .expect("run ffmpeg test generator");
    assert!(status.success(), "ffmpeg test generation failed");
    input
}

fn video_dimensions(path: &Path) -> (i32, i32) {
    let cstr = CString::new(path.to_string_lossy().to_string()).unwrap();
    let ctx = AVFormatContextInput::open(cstr.as_c_str()).unwrap();
    for stream in ctx.streams() {
        let par = stream.codecpar();
        if par.codec_type == ffi::AVMEDIA_TYPE_VIDEO {
            return (par.width, par.height);
        }
    }
    panic!("missing video stream");
}

#[test]
fn cli_preserves_low_res_source_even_with_quality_target() {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new().unwrap();
    let input = generate_input(tmp.path(), "low_res.mkv", "640x360");
    let output = tmp.path().join("resized.mp4");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.args([
        "--device",
        "chromecast",
        "--hw-accel",
        "none",
        "--video-quality",
        "720p",
        "--resize-quality",
        "lanczos",
        "--validate-output",
    ])
    .arg(&input)
    .arg(&output);
    common::assert_cli_success(cmd);

    assert_eq!(video_dimensions(&output), (640, 360));
}

#[test]
fn cli_preserves_low_res_source_by_default() {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new().unwrap();
    let input = generate_input(tmp.path(), "low_res.mkv", "640x360");
    let output = tmp.path().join("preserved.mp4");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.args([
        "--device",
        "chromecast",
        "--hw-accel",
        "none",
        "--video-quality",
        "720p",
        "--validate-output",
    ])
    .arg(&input)
    .arg(&output);
    common::assert_cli_success(cmd);

    assert_eq!(video_dimensions(&output), (640, 360));
}

fn h264_frame_count(path: &Path) -> (usize, usize) {
    let output = Command::new("ffprobe")
        .args([
            "-v",
            "error",
            "-select_streams",
            "v:0",
            "-count_frames",
            "-show_entries",
            "stream=has_b_frames,nb_read_frames",
            "-of",
            "default=nw=1",
        ])
        .arg(path)
        .output()
        .expect("run ffprobe");
    assert!(output.status.success(), "ffprobe failed");
    let text = String::from_utf8(output.stdout).unwrap();
    let value = |key: &str| {
        text.lines()
            .find_map(|line| line.strip_prefix(key))
            .unwrap_or_else(|| panic!("missing {key} in {text}"))
            .parse::<usize>()
            .unwrap()
    };
    (value("has_b_frames="), value("nb_read_frames="))
}

#[test]
fn cli_flushes_delayed_h264_frames_at_end_of_input() {
    for audio_duration in ["1.5", "2", "3"] {
        assert_delayed_frames_survive(audio_duration);
    }
}

fn assert_delayed_frames_survive(audio_duration: &str) {
    common::ensure_ffmpeg_present();
    let tmp = TempDir::new().unwrap();
    let input = tmp.path().join("b-frames.mkv");
    let output = tmp.path().join("converted.mp4");
    let audio = format!("sine=frequency=1000:sample_rate=48000:duration={audio_duration}");
    let generated = Command::new("ffmpeg")
        .args([
            "-hide_banner",
            "-loglevel",
            "error",
            "-f",
            "lavfi",
            "-i",
            "testsrc2=size=960x540:rate=24:duration=2",
            "-f",
            "lavfi",
            "-i",
            audio.as_str(),
            "-c:v",
            "libx264",
            "-bf",
            "3",
            "-g",
            "48",
            "-pix_fmt",
            "yuv420p",
            "-c:a",
            "aac",
        ])
        .arg(&input)
        .status()
        .expect("generate H.264 fixture");
    assert!(generated.success(), "fixture generation failed");
    let (b_frames, source_frames) = h264_frame_count(&input);
    assert!(b_frames > 0, "fixture must use B-frames");
    assert_eq!(source_frames, 48);

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env("HOME", tmp.path())
        .env("XDG_CONFIG_HOME", tmp.path().join("config"))
        .args([
            "--device",
            "chromecast",
            "--hw-accel",
            "none",
            "--video-quality",
            "360p",
            "--delete-source",
            "false",
        ])
        .arg(&input)
        .arg(&output);
    common::assert_cli_success(cmd);
    assert_eq!(h264_frame_count(&output).1, source_frames);
    common::assert_video_timestamps_match(&input, &output, 0.025);
    common::assert_audio_packet_range_matches(&input, &output, 0.1);
    assert!(
        common::probe_duration_ms(&input).abs_diff(common::probe_duration_ms(&output)) <= 100,
        "output duration changed with audio duration {audio_duration}"
    );
}

#[test]
fn cli_downscales_source_that_exceeds_quality_target() {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new().unwrap();
    let input = generate_input(tmp.path(), "high_res.mkv", "1280x720");
    let output = tmp.path().join("downscaled.mp4");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.args([
        "--device",
        "chromecast",
        "--hw-accel",
        "none",
        "--video-quality",
        "360p",
        "--resize-quality",
        "lanczos",
        "--validate-output",
    ])
    .arg(&input)
    .arg(&output);
    common::assert_cli_success(cmd);

    let (width, height) = video_dimensions(&output);
    assert!(width <= 640, "width {width} exceeds 360p cap");
    assert!(height <= 360, "height {height} exceeds 360p cap");
    assert!(width <= 1280, "width {width} exceeds source width");
    assert!(height <= 720, "height {height} exceeds source height");
    assert!(
        width < 1280 || height < 720,
        "expected resize below source dimensions, got {width}x{height}"
    );
}
