#![cfg(feature = "ffmpeg-cli-tests")]

#[path = "common/mod.rs"]
mod common;

use assert_cmd::prelude::*;
use predicates::str;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

fn mk_subs_file(path: &Path) {
    let mut f = fs::File::create(path).expect("create srt");
    use std::io::Write;
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

#[test]
fn test_ass_formatting() {
    let text = "Fully Featured!";
    let x = 960;
    let y = 1000;
    let formatted = format!("{{\\pos({},{})}}{}", x, y, text);
    assert_eq!(formatted, "{\\pos(960,1000)}Fully Featured!");
}

#[test]
#[ignore]
fn test_e2e_pgs_to_ass_conversion() -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let Some(input) = gen_bitmap_input(&tmp) else {
        eprintln!("No bitmap subtitle encoder available; skipping e2e OCR test.");
        return Ok(());
    };

    let output = tmp.path().join("out_ocr.mkv");
    let ass_out = tmp.path().join("out_ocr.ass");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.arg("--sub-mode")
        .arg("force")
        .arg("--ocr-engine")
        .arg("external")
        .arg("--ocr-format")
        .arg("ass")
        .arg("--ocr-external-command")
        .arg("echo hello bitmap")
        .arg(&input)
        .arg(&output);
    cmd.assert().success().stdout(str::is_empty());

    assert!(output.exists(), "output file was not created");

    let output_probe = Command::new("ffprobe")
        .args([
            "-v",
            "error",
            "-select_streams",
            "s",
            "-show_entries",
            "stream=codec_name",
            "-of",
            "csv=p=0",
            &output.to_string_lossy(),
        ])
        .output()?;
    assert!(
        output_probe.status.success(),
        "ffprobe failed: {}",
        String::from_utf8_lossy(&output_probe.stderr)
    );
    let codec_name = String::from_utf8(output_probe.stdout)?
        .lines()
        .next()
        .unwrap_or("")
        .trim()
        .to_string();
    assert_eq!(codec_name, "ass");

    let status_extract = Command::new("ffmpeg")
        .args([
            "-y",
            "-i",
            &output.to_string_lossy(),
            "-map",
            "0:s:0",
            &ass_out.to_string_lossy(),
        ])
        .status()
        .expect("run ffmpeg extract ass");
    assert!(status_extract.success(), "ffmpeg subtitle extract failed");

    let ass_text = fs::read_to_string(&ass_out)?;
    assert!(ass_text.contains("hello bitmap"));

    Ok(())
}
