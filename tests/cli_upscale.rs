#![cfg(feature = "ffmpeg-cli-tests")]

#[path = "common/mod.rs"]
mod common;

use rsmpeg::avformat::AVFormatContextInput;
use rsmpeg::ffi;
use std::ffi::CString;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

fn generate_low_res_input(dir: &Path) -> PathBuf {
    let input = dir.join("low_res.mkv");
    let status = Command::new("ffmpeg")
        .args([
            "-hide_banner",
            "-y",
            "-f",
            "lavfi",
            "-i",
            "testsrc2=size=640x360:rate=24:duration=2",
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
        .expect("run ffmpeg low-res generator");
    assert!(status.success(), "ffmpeg low-res generation failed");
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
fn cli_upscales_when_explicitly_requested() {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new().unwrap();
    let input = generate_low_res_input(tmp.path());
    let output = tmp.path().join("upscaled.mp4");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.args([
        "--device",
        "chromecast",
        "--hw-accel",
        "none",
        "--video-quality",
        "720p",
        "--upscale-mode",
        "fit-quality",
        "--scaler-quality",
        "lanczos",
        "--validate-output",
    ])
    .arg(&input)
    .arg(&output);
    common::assert_cli_success(cmd);

    assert_eq!(video_dimensions(&output), (1280, 720));
}

#[test]
fn cli_does_not_upscale_by_default() {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new().unwrap();
    let input = generate_low_res_input(tmp.path());
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
