#![cfg(feature = "ffmpeg-cli-tests")]

//! Regression test for corrupted input handling.
//! Old behavior could finish "successfully" while decoding through broken frames.
//! New behavior must fail fast to avoid emitting corrupted outputs.
//! Invalid audio packets are the exception: a few are dropped and padded with
//! silence, as the FFmpeg CLI does, while strict policy still fails fast.

use assert_cmd::prelude::*;
use predicates::prelude::PredicateBooleanExt;
use predicates::str;
use std::fs::OpenOptions;
use std::io::{Seek, SeekFrom, Write};
use std::process::Command;
use tempfile::TempDir;

mod common;

fn make_hevc_input(path: &std::path::Path) {
    assert!(
        Command::new("ffmpeg")
            .args([
                "-y",
                "-f",
                "lavfi",
                "-i",
                "testsrc2=size=1280x720:rate=24:duration=12",
                "-f",
                "lavfi",
                "-i",
                "sine=frequency=880:sample_rate=48000:duration=12",
                "-c:v",
                "libx265",
                "-preset",
                "medium",
                "-x265-params",
                "log-level=error",
                "-c:a",
                "aac",
                path.to_string_lossy().as_ref(),
            ])
            .status()
            .expect("invoke ffmpeg")
            .success(),
        "ffmpeg failed to generate HEVC sample"
    );
}

fn corrupt_middle_chunk(path: &std::path::Path, bytes: usize) {
    let meta = std::fs::metadata(path).expect("stat input");
    let offset = meta.len() / 2;
    let mut f = OpenOptions::new()
        .read(true)
        .write(true)
        .open(path)
        .expect("open for corruption");
    f.seek(SeekFrom::Start(offset)).expect("seek mid-file");
    let payload = vec![0xAAu8; bytes];
    f.write_all(&payload).expect("overwrite middle bytes");
    f.flush().expect("flush corruption payload");
}

#[test]
fn cli_fails_fast_on_corrupted_hevc_input() -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let input = tmp.path().join("input_hevc_corrupt.mkv");
    let output = tmp.path().join("out.mp4");

    make_hevc_input(&input);
    corrupt_middle_chunk(&input, 64 * 1024);

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.arg(&input).arg(&output);

    cmd.assert()
        .failure()
        .stderr(str::contains("decoder").and(str::contains("failed")));

    Ok(())
}

fn make_flac_input(path: &std::path::Path) {
    assert!(
        Command::new("ffmpeg")
            .args([
                "-y",
                "-v",
                "error",
                "-f",
                "lavfi",
                "-i",
                "testsrc2=size=320x240:rate=24:duration=10",
                "-f",
                "lavfi",
                "-i",
                "sine=frequency=440:sample_rate=48000:duration=10",
                "-c:v",
                "libx264",
                "-preset",
                "ultrafast",
                "-c:a",
                "flac",
                path.to_string_lossy().as_ref(),
            ])
            .status()
            .expect("invoke ffmpeg")
            .success(),
        "ffmpeg failed to generate FLAC sample"
    );
}

/// Breaks the frame header of `count` FLAC packets spread across the file so
/// the decoder rejects each of them with `AVERROR_INVALIDDATA`.
fn corrupt_flac_frame_headers(path: &std::path::Path, count: usize) -> usize {
    let probe = Command::new("ffprobe")
        .args([
            "-v",
            "error",
            "-select_streams",
            "a:0",
            "-show_entries",
            "packet=pos",
            "-of",
            "csv=p=0",
            path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("invoke ffprobe");
    assert!(probe.status.success(), "ffprobe failed");
    let positions: Vec<u64> = String::from_utf8_lossy(&probe.stdout)
        .lines()
        .filter_map(|line| line.trim().parse().ok())
        .collect();
    assert!(positions.len() > count * 4, "too few audio packets");

    let mut f = OpenOptions::new()
        .read(true)
        .write(true)
        .open(path)
        .expect("open for corruption");
    let step = positions.len() / (count + 1);
    let mut corrupted = 0;
    for pos in positions.iter().skip(step).step_by(step).take(count) {
        // Matroska packet positions point at the SimpleBlock payload: a one-byte
        // track number, a two-byte timecode, and a flags byte precede the frame.
        let header = pos + 4;
        let mut sync = [0u8; 2];
        f.seek(SeekFrom::Start(header)).expect("seek frame header");
        std::io::Read::read_exact(&mut f, &mut sync).expect("read frame header");
        if sync[0] != 0xFF || (sync[1] & 0xFE) != 0xF8 {
            continue;
        }
        f.seek(SeekFrom::Start(header)).expect("seek frame header");
        f.write_all(&[0u8; 4]).expect("overwrite frame header");
        corrupted += 1;
    }
    f.flush().expect("flush corruption payload");
    corrupted
}

fn audio_duration_secs(path: &std::path::Path) -> f64 {
    let probe = Command::new("ffprobe")
        .args([
            "-v",
            "error",
            "-select_streams",
            "a:0",
            "-show_entries",
            "stream=duration",
            "-of",
            "csv=p=0",
            path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("invoke ffprobe");
    String::from_utf8_lossy(&probe.stdout)
        .trim()
        .parse()
        .expect("audio duration")
}

#[test]
fn cli_tolerates_a_few_invalid_audio_packets() -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let input = tmp.path().join("input_flac_corrupt.mkv");
    make_flac_input(&input);
    let corrupted = corrupt_flac_frame_headers(&input, 4);
    assert_eq!(corrupted, 4, "expected to corrupt four FLAC frame headers");

    // Default policy: drop the packets, pad with silence, report the count.
    let output = tmp.path().join("out_tolerate.mp4");
    Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
        .args(["--hw-accel", "none", "--delete-source", "false"])
        .arg(&input)
        .arg(&output)
        .assert()
        .success()
        .stderr(str::contains("dropped 4 of"));
    let duration = audio_duration_secs(&output);
    assert!(
        (duration - 10.0).abs() < 0.1,
        "silence padding should keep the audio length, got {duration}s"
    );

    // Strict policy keeps the previous fail-fast behaviour.
    let strict_output = tmp.path().join("out_strict.mp4");
    Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
        .args(["--hw-accel", "none", "--delete-source", "false"])
        .args(["--audio-decode-error-policy", "fail"])
        .arg(&input)
        .arg(&strict_output)
        .assert()
        .failure()
        .stderr(str::contains("audio-decode-error-policy=fail"));

    // Exceeding the per-stream packet limit still fails.
    let capped_output = tmp.path().join("out_capped.mp4");
    Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
        .args(["--hw-accel", "none", "--delete-source", "false"])
        .args(["--audio-decode-error-max-packets", "2"])
        .arg(&input)
        .arg(&capped_output)
        .assert()
        .failure()
        .stderr(str::contains("audio-decode-error-max-packets=2"));

    Ok(())
}

#[test]
fn cli_fills_holes_in_audio_timestamps() -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    // Dropping two seconds of audio frames keeps their timestamps, which is how
    // lost Matroska clusters look to the decoder: a jump with no decode error.
    let tmp = TempDir::new()?;
    let input = tmp.path().join("input_audio_hole.mkv");
    assert!(
        Command::new("ffmpeg")
            .args([
                "-y",
                "-v",
                "error",
                "-f",
                "lavfi",
                "-i",
                "testsrc2=size=320x240:rate=24:duration=10",
                "-f",
                "lavfi",
                "-i",
                "sine=frequency=440:sample_rate=48000:duration=10",
                "-filter:a",
                "aselect=not(between(t\\,3\\,5))",
                "-c:v",
                "libx264",
                "-preset",
                "ultrafast",
                "-c:a",
                "flac",
                input.to_string_lossy().as_ref(),
            ])
            .status()?
            .success(),
        "ffmpeg failed to generate sample with an audio hole"
    );

    let output = tmp.path().join("out_hole.mp4");
    Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
        .args(["--hw-accel", "none", "--delete-source", "false"])
        .arg(&input)
        .arg(&output)
        .assert()
        .success()
        .stderr(str::contains("of missing audio with silence"));
    let duration = audio_duration_secs(&output);
    assert!(
        (duration - 10.0).abs() < 0.1,
        "the hole should be filled, got {duration}s of audio"
    );

    Ok(())
}
