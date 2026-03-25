//! Ensure sane defaults when no configuration file is available.

#[path = "common/mod.rs"]
mod common;

use rsmpeg::avformat::AVFormatContextInput;
use rsmpeg::ffi;
use std::ffi::CString;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

fn run_no_config_conversion(
    input: &Path,
    output: &Path,
    extra_args: &[&str],
    home: &Path,
) -> Result<String, Box<dyn std::error::Error>> {
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.args(extra_args).arg(input).arg(output);
    cmd.env_remove("DIRECT_PLAY_NICE_CONFIG");
    cmd.env_remove("XDG_CONFIG_HOME");
    cmd.env_remove("DIRECT_PLAY_NICE_CONFIG_FILE"); // legacy variable just in case
    cmd.env("HOME", home);
    // Keep test deterministic by preventing opportunistic GPU probing via PATH lookups.
    cmd.env("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/sbin");
    let output_result = cmd.output()?;
    assert!(
        output_result.status.success(),
        "command failed: {}",
        String::from_utf8_lossy(&output_result.stderr)
    );
    assert!(
        output_result.stdout.is_empty(),
        "stdout should be empty, got: {}",
        String::from_utf8_lossy(&output_result.stdout)
    );
    let stderr = String::from_utf8_lossy(&output_result.stderr).to_string();
    assert!(
        stderr.contains("No direct-play-nice configuration found"),
        "expected no-config warning in stderr, got:\n{}",
        stderr
    );
    Ok(stderr)
}

fn assert_no_config_output_sane(
    output: &PathBuf,
    in_dur_ms: u64,
) -> Result<(), Box<dyn std::error::Error>> {
    assert!(output.exists(), "expected output file {:?} to be created", output);

    let output_cstr = CString::new(output.to_string_lossy().to_string())?;
    let octx = AVFormatContextInput::open(output_cstr.as_c_str())?;

    let mut video_stream = None;
    for st in octx.streams() {
        let par = st.codecpar();
        if par.codec_type == ffi::AVMEDIA_TYPE_VIDEO {
            video_stream = Some((par.width, par.height, par.bit_rate, par.codec_id));
            break;
        }
    }

    let (width, height, mut bit_rate, codec_id) =
        video_stream.expect("expected a video stream in the output");

    assert_eq!(
        codec_id,
        ffi::AV_CODEC_ID_H264,
        "default conversion should output H.264 video"
    );
    assert_eq!(
        width % 16,
        0,
        "output width should align to 16px blocks (got {})",
        width
    );
    assert_eq!(
        height % 2,
        0,
        "output height should be even (got {})",
        height
    );
    assert!(
        height as u32 <= 720,
        "default device cap should clamp height to <= 720 (got {})",
        height
    );

    if bit_rate <= 0 {
        let duration_s = (octx.duration as f64 / ffi::AV_TIME_BASE as f64).max(0.1);
        let size_bytes = fs::metadata(&output)?.len() as f64;
        bit_rate = ((size_bytes * 8.0) / duration_s).round() as i64;
    }
    assert!(
        bit_rate > 0,
        "failed to derive a reasonable bitrate from output"
    );
    assert!(
        bit_rate <= 15_000_000,
        "fallback conversion should not exceed ~15 Mbps, got {}",
        bit_rate
    );

    let out_dur_ms = common::probe_duration_ms(&output);
    let diff = if out_dur_ms > in_dur_ms {
        out_dur_ms - in_dur_ms
    } else {
        in_dur_ms - out_dur_ms
    };
    assert!(
        diff <= 200,
        "duration drift too large: input={}ms output={}ms",
        in_dur_ms,
        out_dur_ms
    );

    Ok(())
}

#[test]
fn converts_with_sane_defaults_when_config_missing() -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let (input, in_dur_ms) = common::gen_odd_width_input(&tmp);
    let output_default = tmp.path().join("out_default.mp4");

    run_no_config_conversion(
        &input,
        &output_default,
        &["--hw-accel", "none"],
        tmp.path(),
    )?;
    assert_no_config_output_sane(&output_default, in_dur_ms)?;

    let output_all = tmp.path().join("out_all.mp4");
    let stderr = run_no_config_conversion(
        &input,
        &output_all,
        &["-s", "all", "--hw-accel", "none"],
        tmp.path(),
    )?;
    assert!(
        stderr.contains("Target streaming devices"),
        "expected device resolution logs in stderr, got:\n{}",
        stderr
    );
    assert_no_config_output_sane(&output_all, in_dur_ms)?;

    Ok(())
}
