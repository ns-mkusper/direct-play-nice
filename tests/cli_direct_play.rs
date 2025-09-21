//! Integration test: ensure CLI converts a problematic video
//! into a Chromecast direct‑play compatible MP4 without 
//! breaking duration or stream mappings.

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
    // These settings should force the CLI to re-encode to H.264/AAC/MOV_TEXT YUV420p
    let dir = tmp.path();
    let video = dir.join("v.mkv");
    let audio = dir.join("a.mp2");
    let subs = dir.join("subs.srt");
    let input = dir.join("input.mkv");

    mk_subs_file(&subs);

    // 2 seconds, 160x120, yuv444p, mpeg4
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

fn probe_duration_ms(path: &PathBuf) -> u64 {
    let ictx = AVFormatContextInput::open(
        std::ffi::CString::new(path.to_string_lossy().to_string()).unwrap().as_c_str(),
        None,
        &mut None,
    )
    .unwrap();
    // duration in AV_TIME_BASE (microseconds)
    (ictx.duration as i64 / 1000).max(0) as u64
}

#[test]
fn cli_produces_chromecast_direct_play_mp4() -> Result<(), Box<dyn std::error::Error>> {
    // Fail fast if ffmpeg CLI is not present
    ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let (input, in_dur_ms) = gen_problem_input(&tmp);
    let output = tmp.path().join("out.mp4");

    // Run the CLI for all Chromecast models (intersection guarantees
    // output is direct-play-compatible across all of them).
    let mut cmd = Command::cargo_bin("direct_play_nice")?;
    cmd.arg("-s")
        .arg("chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra")
        .arg(&input)
        .arg(&output);
    cmd.assert().success().stdout(str::is_empty());

    assert!(output.exists(), "output file was not created");

    // Validate via rsmpeg
    let mut octx = AVFormatContextInput::open(
        std::ffi::CString::new(output.to_string_lossy().to_string())
            .unwrap()
            .as_c_str(),
        None,
        &mut None,
    )?;

    let mut saw_v = false;
    let mut saw_a = false;
    let mut saw_s = false;
    let mut width = 0i32;
    let mut height = 0i32;
    let mut fps_num = 0i32;
    let mut fps_den = 1i32;
    let mut level = 0i32;
    let mut pix_fmt = -1i32;

    for st in octx.streams() {
        let par = st.codecpar();
        match par.codec_type {
            t if t == ffi::AVMEDIA_TYPE_VIDEO => {
                saw_v = true;
                assert_eq!(par.codec_id, ffi::AV_CODEC_ID_H264, "video must be H.264");
                width = par.width;
                height = par.height;
                level = par.level;
                pix_fmt = par.format;
                let rate = st.avg_frame_rate;
                fps_num = rate.num;
                fps_den = rate.den;
            }
            t if t == ffi::AVMEDIA_TYPE_AUDIO => {
                saw_a = true;
                assert_eq!(par.codec_id, ffi::AV_CODEC_ID_AAC, "audio must be AAC");
            }
            t if t == ffi::AVMEDIA_TYPE_SUBTITLE => {
                saw_s = true;
                assert_eq!(par.codec_id, ffi::AV_CODEC_ID_MOV_TEXT, "subs must be MOV_TEXT");
            }
            _ => {}
        }
    }

    assert!(saw_v && saw_a && saw_s, "missing one or more required streams");

    // Chromecast (1st gen) bounds
    assert!(width as u32 <= 1920 && height as u32 <= 1080, "resolution too high");
    // Accept any profile but ensure level <= 4.1 (41)
    assert!(level <= 41, "H.264 level too high: {}", level);
    // yuv420p pixel format
    assert_eq!(pix_fmt, ffi::AV_PIX_FMT_YUV420P, "pix fmt must be yuv420p");
    // fps <= 30
    if fps_den != 0 { // guard
        let fps = (fps_num as f64) / (fps_den as f64);
        assert!(fps <= 30.01, "fps too high: {}", fps);
    }

    // Duration close to input (within 200ms)
    let out_dur_ms = probe_duration_ms(&output);
    let diff = if out_dur_ms > in_dur_ms {
        out_dur_ms - in_dur_ms
    } else {
        in_dur_ms - out_dur_ms
    };
    assert!(diff <= 200, "duration drift too large: in={}ms out={}ms", in_dur_ms, out_dur_ms);

    Ok(())
}
