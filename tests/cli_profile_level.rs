mod common;

use assert_cmd::prelude::*;
use common::{ensure_ffmpeg_present, gen_h264_high_input, read_video_profile_level};
use rsmpeg::ffi;
use std::error::Error;
use std::process::Command;
use tempfile::TempDir;

fn run_cli(
    input: &std::path::Path,
    output: &std::path::Path,
    hw_accel: &str,
) -> Result<String, Box<dyn Error>> {
    let mut cmd = Command::cargo_bin("direct_play_nice")?;
    cmd.env("RUST_LOG", "info")
        .arg("-s")
        .arg("chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra")
        .arg(input)
        .arg(output)
        .arg("--hw-accel")
        .arg(hw_accel);
    let out = cmd.output()?;
    assert!(
        out.status.success(),
        "direct_play_nice failed with status {:?}\nstderr: {}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        out.stdout.is_empty(),
        "expected no stdout, got: {}",
        String::from_utf8_lossy(&out.stdout)
    );
    Ok(String::from_utf8_lossy(&out.stderr).to_string())
}

fn assert_converted_profile_level(output: &std::path::Path) {
    assert!(
        output.exists(),
        "expected output file {} to exist",
        output.display()
    );
    let (profile, level) = read_video_profile_level(output);
    assert_eq!(
        profile,
        ffi::AV_PROFILE_H264_HIGH as i32,
        "output profile mismatch"
    );
    assert_eq!(level, 41, "output level mismatch");
}

#[test]
fn cli_enforces_profile_level_in_software_mode() -> Result<(), Box<dyn Error>> {
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let input = gen_h264_high_input(&tmp);
    let output = tmp.path().join("out_sw.mp4");

    let stderr = run_cli(input.as_path(), output.as_path(), "none")?;

    assert!(
        stderr.contains("H.264 level Level5_2 exceeds device limit Level4_1"),
        "expected detection warning about excessive H.264 level, got:\n{}",
        stderr
    );
    assert!(
        stderr.contains("requested profile High level 4.1; reported profile High level 4.1"),
        "expected encoder profile/level report, got:\n{}",
        stderr
    );

    assert_converted_profile_level(&output);
    Ok(())
}

#[test]
fn cli_enforces_profile_level_with_hw_accel_preference() -> Result<(), Box<dyn Error>> {
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let input = gen_h264_high_input(&tmp);
    let output = tmp.path().join("out_auto.mp4");

    let stderr = run_cli(input.as_path(), output.as_path(), "auto")?;

    assert!(
        stderr.contains("requested profile High level 4.1; reported profile High level 4.1"),
        "expected encoder profile/level report, got:\n{}",
        stderr
    );

    // Regardless of whether hardware was actually used, the output must meet the target.
    assert_converted_profile_level(&output);
    Ok(())
}
