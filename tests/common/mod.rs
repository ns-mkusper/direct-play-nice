#![allow(dead_code)]

use predicates::str;
use rsmpeg::avformat::AVFormatContextInput;
use rsmpeg::ffi;
use std::ffi::CString;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

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

pub fn probe_duration_ms(path: &PathBuf) -> u64 {
    let input_cstr = CString::new(path.to_string_lossy().to_string()).unwrap();
    let ictx = AVFormatContextInput::open(input_cstr.as_c_str()).unwrap();
    (ictx.duration as i64 / 1000).max(0) as u64
}

pub fn assert_cli_success(mut cmd: Command) {
    use assert_cmd::prelude::*;
    cmd.assert().success().stdout(str::is_empty());
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
