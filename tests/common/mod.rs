#![allow(dead_code)]

use predicates::str;
use rsmpeg::avformat::AVFormatContextInput;
use rsmpeg::ffi;
use std::env;
use std::error::Error;
use std::ffi::CString;
use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

const NV_LIB_DIR: &str = "/usr/lib/x86_64-linux-gnu/nvidia/current";

pub fn ensure_ffmpeg_present() {
    let out = Command::new("ffmpeg").arg("-version").output();
    match out {
        Ok(o) if o.status.success() => {}
        _ => panic!("ffmpeg CLI not found. Install ffmpeg and ensure it is on PATH."),
    }
}

fn mk_subs_file(path: &PathBuf) {
    let mut f = File::create(path).expect("create srt");
    writeln!(
        f,
        "1\n00:00:00,000 --> 00:00:00,800\nhello world\n\n2\n00:00:01,000 --> 00:00:01,600\nsecond line\n"
    )
    .unwrap();
}

pub fn gen_problem_input(tmp: &TempDir) -> (PathBuf, u64) {
    let dir = tmp.path();
    let video = dir.join("v.mkv");
    let audio = dir.join("a.mp2");
    let subs = dir.join("subs.srt");
    let input = dir.join("input.mkv");

    mk_subs_file(&subs);

    let status_v = Command::new("ffmpeg")
        .args([
            "-y",
            "-f",
            "lavfi",
            "-i",
            "testsrc=size=160x120:rate=25:duration=2",
            "-pix_fmt",
            "yuv444p",
            "-c:v",
            "mpeg4",
            &video.to_string_lossy(),
        ])
        .status()
        .expect("run ffmpeg video");
    assert!(status_v.success(), "ffmpeg video generation failed");

    let status_a = Command::new("ffmpeg")
        .args([
            "-y",
            "-f",
            "lavfi",
            "-i",
            "sine=frequency=1000:sample_rate=44100:duration=2",
            "-c:a",
            "mp2",
            &audio.to_string_lossy(),
        ])
        .status()
        .expect("run ffmpeg audio");
    assert!(status_a.success(), "ffmpeg audio generation failed");

    let status_mux = Command::new("ffmpeg")
        .args([
            "-y",
            "-i",
            &video.to_string_lossy(),
            "-i",
            &audio.to_string_lossy(),
            "-i",
            &subs.to_string_lossy(),
            "-c:v",
            "copy",
            "-c:a",
            "copy",
            "-c:s",
            "srt",
            "-map",
            "0:v:0",
            "-map",
            "1:a:0",
            "-map",
            "2:0",
            &input.to_string_lossy(),
        ])
        .status()
        .expect("run ffmpeg mux");
    assert!(status_mux.success(), "ffmpeg mux failed");

    let dur_ms = probe_duration_ms(&input);
    (input, dur_ms)
}

pub fn gen_odd_width_input(tmp: &TempDir) -> (PathBuf, u64) {
    let dir = tmp.path();
    let input = dir.join("odd_width.mkv");
    let status = Command::new("ffmpeg")
        .args([
            "-y",
            "-f",
            "lavfi",
            "-i",
            "testsrc=size=1792x1080:rate=24:duration=2",
            "-f",
            "lavfi",
            "-i",
            "sine=frequency=1000:sample_rate=48000:duration=2",
            "-c:v",
            "libx264",
            "-preset",
            "ultrafast",
            "-pix_fmt",
            "yuv420p",
            "-c:a",
            "aac",
            "-shortest",
            input.to_str().unwrap(),
        ])
        .status()
        .expect("run ffmpeg odd-width generator");
    assert!(status.success(), "ffmpeg odd-width generation failed");
    let dur_ms = probe_duration_ms(&input);
    (input, dur_ms)
}

pub fn probe_duration_ms(path: &PathBuf) -> u64 {
    let input_cstr = CString::new(path.to_string_lossy().to_string()).unwrap();
    let ictx = AVFormatContextInput::open(input_cstr.as_c_str()).unwrap();
    (ictx.duration as i64 / 1000).max(0) as u64
}

pub fn assert_cli_success(mut cmd: Command) {
    use assert_cmd::prelude::*;
    cmd.assert().success().stdout(str::is_empty());
}

pub fn nv_library_env_value() -> Option<String> {
    if fs::metadata(NV_LIB_DIR)
        .map(|m| m.is_dir())
        .unwrap_or(false)
    {
        let existing = env::var("LD_LIBRARY_PATH").unwrap_or_default();
        if existing.is_empty() {
            Some(NV_LIB_DIR.to_string())
        } else {
            Some(format!("{NV_LIB_DIR}:{existing}"))
        }
    } else {
        None
    }
}

pub fn ffprobe_duration(path: &Path) -> Result<f64, Box<dyn Error>> {
    let output = Command::new("ffprobe")
        .args([
            "-v",
            "error",
            "-show_entries",
            "format=duration",
            "-of",
            "default=noprint_wrappers=1:nokey=1",
            path.to_str().expect("path utf8"),
        ])
        .output()?;
    if !output.status.success() {
        return Err(format!(
            "ffprobe failed: {}",
            String::from_utf8_lossy(&output.stderr)
        )
        .into());
    }
    let duration = String::from_utf8(output.stdout)?.trim().parse::<f64>()?;
    Ok(duration)
}

fn parse_fraction(value: &str) -> Result<f64, Box<dyn Error>> {
    if let Some((num, den)) = value.split_once('/') {
        let num = num.trim().parse::<f64>()?;
        let den = den.trim().parse::<f64>()?;
        if den == 0.0 {
            Err("fraction denominator cannot be zero".into())
        } else {
            Ok(num / den)
        }
    } else {
        Ok(value.trim().parse::<f64>()?)
    }
}

pub fn ffprobe_avg_frame_rate(path: &Path) -> Result<f64, Box<dyn Error>> {
    let output = Command::new("ffprobe")
        .args([
            "-v",
            "error",
            "-select_streams",
            "v:0",
            "-show_entries",
            "stream=avg_frame_rate",
            "-of",
            "default=noprint_wrappers=1:nokey=1",
            path.to_str().expect("path utf8"),
        ])
        .output()?;
    if !output.status.success() {
        return Err(format!(
            "ffprobe failed: {}",
            String::from_utf8_lossy(&output.stderr)
        )
        .into());
    }
    let rate = String::from_utf8(output.stdout)?;
    parse_fraction(rate.trim())
}

pub fn gen_h264_high_input(tmp: &TempDir) -> PathBuf {
    let dir = tmp.path();
    let video = dir.join("v_h264.mp4");
    let audio = dir.join("a_eac3.mka");
    let input = dir.join("input_h264.mkv");

    // Create 1920x1080 H.264 High@L5.2 video
    assert!(
        Command::new("ffmpeg")
            .args([
                "-y",
                "-f",
                "lavfi",
                "-i",
                "testsrc=size=1920x1080:rate=23.976:duration=2",
                "-pix_fmt",
                "yuv420p",
                "-c:v",
                "libx264",
                "-profile:v",
                "high",
                "-level:v",
                "5.2",
                "-x264-params",
                "ref=4",
                &video.to_string_lossy(),
            ])
            .status()
            .expect("run ffmpeg video")
            .success(),
        "ffmpeg H.264 video generation failed"
    );

    // Create 6 channel E-AC3 audio to mirror real-world input
    assert!(
        Command::new("ffmpeg")
            .args([
                "-y",
                "-f",
                "lavfi",
                "-i",
                "anoisesrc=duration=2",
                "-ac",
                "6",
                "-c:a",
                "eac3",
                "-b:a",
                "384k",
                &audio.to_string_lossy(),
            ])
            .status()
            .expect("run ffmpeg audio")
            .success(),
        "ffmpeg E-AC3 generation failed"
    );

    assert!(
        Command::new("ffmpeg")
            .args([
                "-y",
                "-i",
                &video.to_string_lossy(),
                "-i",
                &audio.to_string_lossy(),
                "-c:v",
                "copy",
                "-c:a",
                "copy",
                &input.to_string_lossy(),
            ])
            .status()
            .expect("run ffmpeg mux")
            .success(),
        "ffmpeg mux for H.264 input failed"
    );

    input
}

pub fn read_video_profile_level(path: &Path) -> (i32, i32) {
    let cstr = CString::new(path.to_string_lossy().to_string()).unwrap();
    let ictx = AVFormatContextInput::open(cstr.as_c_str()).expect("open output file");
    for stream in ictx.streams() {
        let par = stream.codecpar();
        if par.codec_type == ffi::AVMEDIA_TYPE_VIDEO {
            return (par.profile, par.level);
        }
    }
    panic!("no video stream found in {}", path.display());
}
