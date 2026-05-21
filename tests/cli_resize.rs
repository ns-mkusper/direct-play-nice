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
