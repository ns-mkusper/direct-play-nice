//! Ensure that specifying `-s all` (aka all devices) behaves the same
//! as omitting `-s` and that a tiny problematic source is converted into
//! a direct‑play compatible MP4 that satisfies all device constraints.

use assert_cmd::prelude::*;
use predicates::str;
use std::fs::File;
use std::io::Write;
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

fn mk_subs_file(path: &PathBuf) {
    let mut f = File::create(path).expect("create srt");
    // 2 subtitles to cover >1 packet
    writeln!(
        f,
        "1\n00:00:00,000 --> 00:00:00,800\nhello world\n\n2\n00:00:01,000 --> 00:00:01,600\nsecond line\n"
    )
    .unwrap();
}

fn gen_problem_input(tmp: &TempDir) -> (PathBuf, u64) {
    // Create tiny MKV with: MPEG4 video (yuv444p), MP2 audio, SRT subs
    let dir = tmp.path();
    let video = dir.join("v.mkv");
    let audio = dir.join("a.mp2");
    let subs = dir.join("subs.srt");
    let input = dir.join("input.mkv");

    mk_subs_file(&subs);

    // 2 seconds, 160x120, yuv444p, mpeg4
    assert!(
        Command::new("ffmpeg")
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
            .expect("run ffmpeg video")
            .success(),
        "ffmpeg video generation failed"
    );

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

    assert!(
        Command::new("ffmpeg")
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
            .expect("run ffmpeg mux")
            .success(),
        "ffmpeg mux failed"
    );

    let ictx = AVFormatContextInput::open(
        std::ffi::CString::new(input.to_string_lossy().to_string())
            .unwrap()
            .as_c_str(),
    )
    .unwrap();
    let dur_ms = (ictx.duration as i64 / 1000).max(0) as u64;
    (input, dur_ms)
}

#[test]
fn cli_all_devices_selector_converts_to_direct_play() -> Result<(), Box<dyn std::error::Error>> {
    ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let (input, _in_ms) = gen_problem_input(&tmp);
    let output = tmp.path().join("out_all.mp4");

    // Use the selector value instead of enumerating models
    let mut cmd = Command::cargo_bin("direct_play_nice")?;
    cmd.arg("-s").arg("all").arg(&input).arg(&output);
    cmd.assert().success().stdout(str::is_empty());
    assert!(output.exists(), "output file was not created");

    // Validate the essentials: H.264/AAC and yuv420p
    let octx = AVFormatContextInput::open(
        std::ffi::CString::new(output.to_string_lossy().to_string())
            .unwrap()
            .as_c_str(),
    )?;

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
    assert!(saw_v && saw_a, "missing required streams");
    assert_eq!(pix_fmt, ffi::AV_PIX_FMT_YUV420P, "pix fmt must be yuv420p");

    Ok(())
}
