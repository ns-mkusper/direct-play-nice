#![cfg(feature = "ffmpeg-cli-tests")]

//! Regression test for corrupted input handling.
//! Old behavior could finish "successfully" while decoding through broken frames.
//! New behavior must fail fast to avoid emitting corrupted outputs.

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
