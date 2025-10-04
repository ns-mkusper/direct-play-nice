//! GPU acceleration: flag smoke tests
//!
//! These tests exercise `--hw-accel` with `auto` and `none` to ensure the
//! flag is accepted and the conversion succeeds. They do not assert that a
//! hardware encoder was used, since CI machines may lack GPUs.

use assert_cmd::prelude::*;
use predicates::str;
use std::ffi::CString;
use std::path::PathBuf;
use std::process::Command;
use tempfile::TempDir;

use rsmpeg::avformat::AVFormatContextInput;
use rsmpeg::ffi;

fn ensure_ffmpeg_present() {
    let out = Command::new("ffmpeg").arg("-version").output();
    match out {
        Ok(o) if o.status.success() => return,
        _ => panic!("ffmpeg CLI not found. Install ffmpeg and ensure it is on PATH."),
    }
}

fn gen_tiny_input(tmp: &TempDir) -> PathBuf {
    let dir = tmp.path();
    let video = dir.join("v.mkv");
    let audio = dir.join("a.mp2");
    let input = dir.join("input_simple.mkv");

    // 2 seconds MPEG4 yuv420p video
    assert!(
        Command::new("ffmpeg")
            .args([
                "-y",
                "-f",
                "lavfi",
                "-i",
                "testsrc=size=160x120:rate=25:duration=2",
                "-pix_fmt",
                "yuv420p",
                "-c:v",
                "mpeg4",
                &video.to_string_lossy(),
            ])
            .status()
            .expect("run ffmpeg video")
            .success(),
        "ffmpeg video generation failed"
    );

    // 2 seconds MP2 audio
    assert!(
        Command::new("ffmpeg")
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
            .expect("run ffmpeg audio")
            .success(),
        "ffmpeg audio generation failed"
    );

    // Mux into MKV (no subs needed here)
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
        "ffmpeg mux failed"
    );

    input
}

fn assert_output_basic(output: &PathBuf) {
    let output_cstr = CString::new(output.to_string_lossy().to_string()).unwrap();
    let octx = AVFormatContextInput::open(output_cstr.as_c_str()).expect("open output");

    let mut saw_v = false;
    let mut saw_a = false;
    let mut pix_fmt = -1i32;
    for st in octx.streams() {
        let par = st.codecpar();
        if par.codec_type == ffi::AVMEDIA_TYPE_VIDEO {
            saw_v = true;
            assert_eq!(par.codec_id, ffi::AV_CODEC_ID_H264, "video must be H.264");
            pix_fmt = par.format;
        } else if par.codec_type == ffi::AVMEDIA_TYPE_AUDIO {
            saw_a = true;
            assert_eq!(par.codec_id, ffi::AV_CODEC_ID_AAC, "audio must be AAC");
        }
    }
    assert!(saw_v && saw_a, "missing video or audio stream");
    assert_eq!(pix_fmt, ffi::AV_PIX_FMT_YUV420P, "pix fmt must be yuv420p");
}

#[test]
fn cli_hw_accel_none_and_auto_succeed() -> Result<(), Box<dyn std::error::Error>> {
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let input = gen_tiny_input(&tmp);

    // none
    let out_none = tmp.path().join("out_none.mp4");
    let mut cmd = Command::cargo_bin("direct_play_nice")?;
    cmd.arg("-s")
        .arg("chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra")
        .arg(&input)
        .arg(&out_none)
        .arg("--hw-accel")
        .arg("none");
    cmd.assert().success().stdout(str::is_empty());
    assert!(out_none.exists(), "output file (none) was not created");
    assert_output_basic(&out_none);

    // auto
    let out_auto = tmp.path().join("out_auto.mp4");
    let mut cmd2 = Command::cargo_bin("direct_play_nice")?;
    cmd2.arg("-s")
        .arg("chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra")
        .arg(&input)
        .arg(&out_auto)
        .arg("--hw-accel")
        .arg("auto");
    cmd2.assert().success().stdout(str::is_empty());
    assert!(out_auto.exists(), "output file (auto) was not created");
    assert_output_basic(&out_auto);

    Ok(())
}
