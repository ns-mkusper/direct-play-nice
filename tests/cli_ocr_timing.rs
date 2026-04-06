#![cfg(feature = "ffmpeg-cli-tests")]

//! Integration test: bitmap subtitle OCR preserves cue timing when muxed to MOV_TEXT.

use assert_cmd::prelude::*;
use predicates::str;
use std::ffi::CString;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

use rsmpeg::avformat::AVFormatContextInput;

fn ensure_ffmpeg_present() {
    let out = Command::new("ffmpeg").arg("-version").output();
    match out {
        Ok(o) if o.status.success() => (),
        _ => panic!("ffmpeg CLI not found. Install ffmpeg and ensure it is on PATH."),
    }
}

fn mk_subs_file(path: &Path) {
    let mut f = fs::File::create(path).expect("create srt");
    writeln!(
        f,
        "1\n00:00:00,200 --> 00:00:00,800\nhello bitmap\n\n2\n00:00:01,200 --> 00:00:01,900\nsecond line\n"
    )
    .unwrap();
}

fn gen_bitmap_input(tmp: &TempDir) -> Option<PathBuf> {
    let dir = tmp.path();
    let video = dir.join("v.mkv");
    let audio = dir.join("a.mp2");
    let subs = dir.join("subs.srt");
    let input = dir.join("input_bitmap.mkv");

    mk_subs_file(&subs);

    let status_v = Command::new("ffmpeg")
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

    let candidates = ["dvd_subtitle", "dvb_subtitle", "hdmv_pgs_subtitle"];
    for codec in candidates {
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
                codec,
                "-map",
                "0:v:0",
                "-map",
                "1:a:0",
                "-map",
                "2:0",
                &input.to_string_lossy(),
            ])
            .status()
            .expect("run ffmpeg mux bitmap subs");
        if status_mux.success() {
            return Some(input);
        }
    }

    None
}

fn parse_timestamp_ms(raw: &str) -> Option<i64> {
    let trimmed = raw.trim();
    let (hms, ms) = trimmed.split_once(',')?;
    let mut parts = hms.split(':');
    let h = parts.next()?.parse::<i64>().ok()?;
    let m = parts.next()?.parse::<i64>().ok()?;
    let s = parts.next()?.parse::<i64>().ok()?;
    let ms = ms.parse::<i64>().ok()?;
    Some(h * 3_600_000 + m * 60_000 + s * 1_000 + ms)
}

fn parse_srt_times(path: &Path) -> Vec<(i64, i64)> {
    let contents = fs::read_to_string(path).expect("read srt");
    let mut out = Vec::new();
    let mut iter = contents.lines();
    while let Some(line) = iter.next() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let time_line = if line.contains("-->") {
            line.to_string()
        } else {
            iter.next().unwrap_or("").to_string()
        };
        if !time_line.contains("-->") {
            continue;
        }
        let mut parts = time_line.split("-->");
        let start = parts.next().and_then(parse_timestamp_ms);
        let end = parts.next().and_then(parse_timestamp_ms);
        if let (Some(s), Some(e)) = (start, end) {
            out.push((s, e));
        }
    }
    out
}

#[test]
fn cli_ocr_preserves_subtitle_timing() -> Result<(), Box<dyn std::error::Error>> {
    ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let Some(input) = gen_bitmap_input(&tmp) else {
        eprintln!("No bitmap subtitle encoder available; skipping OCR timing test.");
        return Ok(());
    };

    let output = tmp.path().join("out_ocr.mp4");
    let srt_out = tmp.path().join("out_ocr.srt");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.arg("--ocr-engine")
        .arg("external")
        .arg("--ocr-format")
        .arg("srt")
        .arg("--ocr-external-command")
        .arg("echo hello bitmap")
        .arg(&input)
        .arg(&output);
    cmd.assert().success().stdout(str::is_empty());

    assert!(output.exists(), "output file was not created");

    let output_cstr = CString::new(output.to_string_lossy().to_string()).unwrap();
    let octx = AVFormatContextInput::open(output_cstr.as_c_str())?;
    let has_subs = octx
        .streams()
        .iter()
        .any(|st| st.codecpar().codec_type == rsmpeg::ffi::AVMEDIA_TYPE_SUBTITLE);
    assert!(has_subs, "expected subtitle stream in output");

    let status_extract = Command::new("ffmpeg")
        .args([
            "-y",
            "-i",
            &output.to_string_lossy(),
            "-map",
            "0:s:0",
            &srt_out.to_string_lossy(),
        ])
        .status()
        .expect("run ffmpeg extract srt");
    assert!(status_extract.success(), "ffmpeg subtitle extract failed");

    let times = parse_srt_times(&srt_out);
    assert!(times.len() >= 2, "expected at least two subtitle cues");

    let (s1, e1) = times[0];
    let (s2, e2) = times[1];
    let tol = 200i64;
    assert!(
        (s1 - 200).abs() <= tol && (e1 - 800).abs() <= tol,
        "cue 1 timing drift: got {}-{}ms",
        s1,
        e1
    );
    assert!(
        (s2 - 1200).abs() <= tol && (e2 - 1900).abs() <= tol,
        "cue 2 timing drift: got {}-{}ms",
        s2,
        e2
    );

    Ok(())
}
