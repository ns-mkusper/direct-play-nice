#![cfg(feature = "ffmpeg-cli-tests")]

//! Re-encoded video keeps its display geometry and colour interpretation.

#[path = "common/mod.rs"]
mod common;

use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

/// Write a one-second fixture: `video` (a lavfi source) encoded with `opts`,
/// plus an AAC tone so the output passes DPN's device validation.
fn fixture(video: &str, opts: &[&str], output: &Path) {
    let status = Command::new("ffmpeg")
        .args([
            "-hide_banner",
            "-loglevel",
            "error",
            "-y",
            "-f",
            "lavfi",
            "-i",
            video,
        ])
        .args([
            "-f",
            "lavfi",
            "-i",
            "sine=frequency=1000:sample_rate=48000:duration=1",
        ])
        .args(opts)
        .args(["-c:a", "aac", "-shortest"])
        .arg(output)
        .status()
        .expect("run ffmpeg test generator");
    assert!(status.success(), "ffmpeg test generation failed");
}

fn convert(input: &Path, output: &Path) {
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.args([
        "--device",
        "chromecast",
        "--hw-accel",
        "none",
        "--validate-output",
    ])
    .arg(input)
    .arg(output);
    common::assert_cli_success(cmd);
}

/// `key=value` pairs for the first video stream.
fn probe(path: &Path) -> std::collections::HashMap<String, String> {
    let out = Command::new("ffprobe")
        .args([
            "-v",
            "error",
            "-select_streams",
            "v:0",
            "-show_entries",
            "stream=width,height,sample_aspect_ratio,display_aspect_ratio,color_range,color_space,color_primaries,color_transfer",
            "-of",
            "default=noprint_wrappers=1",
        ])
        .arg(path)
        .output()
        .expect("run ffprobe");
    String::from_utf8_lossy(&out.stdout)
        .lines()
        .filter_map(|l| l.split_once('='))
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect()
}

/// Mean luma code value of the first frame, read as stored (no range conversion).
fn first_frame_mean_luma(path: &Path) -> f64 {
    let out = Command::new("ffmpeg")
        .args(["-v", "error", "-i"])
        .arg(path)
        .args([
            "-frames:v",
            "1",
            "-vf",
            "extractplanes=y",
            "-f",
            "rawvideo",
            "-pix_fmt",
            "gray",
            "-",
        ])
        .output()
        .expect("decode first frame");
    assert!(
        !out.stdout.is_empty(),
        "no frame decoded from {}",
        path.display()
    );
    out.stdout.iter().map(|&b| b as f64).sum::<f64>() / out.stdout.len() as f64
}

fn input(tmp: &TempDir, name: &str) -> PathBuf {
    tmp.path().join(name)
}

#[test]
fn container_level_sample_aspect_ratio_survives_reencode() {
    common::ensure_ffmpeg_present();
    let tmp = TempDir::new().unwrap();
    // NTSC DVD geometry in Matroska/FFV1, where SAR lives in the container,
    // not the codec. FFV1 is not direct-play, so DPN must re-encode it.
    let src = input(&tmp, "dvd.mkv");
    fixture(
        "testsrc2=size=720x480:rate=24000/1001:duration=1",
        &["-vf", "setsar=8/9", "-c:v", "ffv1", "-pix_fmt", "yuv420p"],
        &src,
    );
    assert_eq!(probe(&src)["sample_aspect_ratio"], "8:9");
    let out = input(&tmp, "out.mp4");
    convert(&src, &out);
    let p = probe(&out);
    assert_eq!((p["width"].as_str(), p["height"].as_str()), ("720", "480"));
    assert_eq!(p["sample_aspect_ratio"], "8:9");
    assert_eq!(p["display_aspect_ratio"], "4:3");
}

#[test]
fn untagged_sd_is_tagged_bt601_limited() {
    common::ensure_ffmpeg_present();
    let tmp = TempDir::new().unwrap();
    let src = input(&tmp, "sd.mkv");
    fixture(
        "testsrc2=size=720x480:rate=24:duration=1",
        &["-c:v", "ffv1", "-pix_fmt", "yuv420p"],
        &src,
    );
    assert_eq!(probe(&src)["color_space"], "unknown");
    let out = input(&tmp, "out.mp4");
    convert(&src, &out);
    let p = probe(&out);
    assert_eq!(p["color_space"], "smpte170m");
    assert_eq!(p["color_range"], "tv");
}

#[test]
fn tagged_hd_colour_description_is_preserved() {
    common::ensure_ffmpeg_present();
    let tmp = TempDir::new().unwrap();
    // MPEG-2 carries matrix, primaries and transfer in its bitstream.
    let src = input(&tmp, "hd.mkv");
    fixture(
        "testsrc2=size=1280x720:rate=24:duration=1",
        &[
            "-c:v",
            "mpeg2video",
            "-vf",
            "setparams=range=tv:colorspace=bt709:color_primaries=bt709:color_trc=bt709",
            "-pix_fmt",
            "yuv420p",
        ],
        &src,
    );
    assert_eq!(probe(&src)["color_primaries"], "bt709");
    let out = input(&tmp, "out.mp4");
    convert(&src, &out);
    let p = probe(&out);
    assert_eq!(p["color_space"], "bt709");
    assert_eq!(p["color_primaries"], "bt709");
    assert_eq!(p["color_transfer"], "bt709");
    assert_eq!(p["color_range"], "tv");
}

#[test]
fn full_range_yuv420p_is_converted_to_limited_range() {
    common::ensure_ffmpeg_present();
    let tmp = TempDir::new().unwrap();
    // Black stored as full-range code 0 in plain yuv420p, tagged "pc".
    let src = input(&tmp, "full.mkv");
    fixture(
        "color=c=black:size=320x240:rate=24:duration=1",
        &[
            "-vf",
            "scale=out_range=full,format=yuv420p",
            "-c:v",
            "ffv1",
            "-color_range",
            "pc",
            "-colorspace",
            "bt709",
        ],
        &src,
    );
    assert_eq!(probe(&src)["color_range"], "pc");
    assert!(
        first_frame_mean_luma(&src) < 2.0,
        "fixture should hold full-range black"
    );
    let out = input(&tmp, "out.mp4");
    convert(&src, &out);
    assert_eq!(probe(&out)["color_range"], "tv");
    let y = first_frame_mean_luma(&out);
    assert!(
        (14.0..=18.0).contains(&y),
        "black should map to limited-range 16, got {y:.1}"
    );
}
