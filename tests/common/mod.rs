use predicates::str;
use rsmpeg::avformat::AVFormatContextInput;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
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
    let ictx = AVFormatContextInput::open(
        std::ffi::CString::new(path.to_string_lossy().to_string())
            .unwrap()
            .as_c_str(),
        None,
        &mut None,
    )
    .unwrap();
    (ictx.duration as i64 / 1000).max(0) as u64
}

pub fn assert_cli_success(mut cmd: Command) {
    use assert_cmd::prelude::*;
    cmd.assert().success().stdout(str::is_empty());
}
