mod common;

use assert_cmd::Command as AssertCommand;
use common::{ffprobe_avg_frame_rate, ffprobe_duration, nv_library_env_value};
use std::error::Error;
use std::process::Command;
use tempfile::tempdir;

fn set_nv_env(cmd: &mut Command) {
    if let Some(ld_path) = nv_library_env_value() {
        cmd.env("LD_LIBRARY_PATH", ld_path);
    }
}

#[test]
fn nvenc_pipeline_preserves_duration_when_available() -> Result<(), Box<dyn Error>> {
    common::ensure_ffmpeg_present();

    let tmp = tempdir()?;
    let input = tmp.path().join("nvenc_input.mp4");
    let output = tmp.path().join("nvenc_output.mp4");

    let mut gen_cmd = Command::new("ffmpeg");
    set_nv_env(&mut gen_cmd);
    let gen_status = gen_cmd
        .args([
            "-hide_banner",
            "-loglevel",
            "error",
            "-y",
            "-f",
            "lavfi",
            "-i",
            "testsrc2=size=1920x1080:rate=30000/1001:duration=4",
            "-f",
            "lavfi",
            "-i",
            "sine=frequency=523.25:sample_rate=48000:duration=4",
            "-c:v",
            "h264_nvenc",
            "-preset",
            "p5",
            "-profile:v",
            "high",
            "-level:v",
            "4.1",
            "-pix_fmt",
            "yuv420p",
            "-c:a",
            "aac",
            "-b:a",
            "192k",
            "-movflags",
            "+faststart",
            input.to_str().expect("path utf8"),
        ])
        .status()?;

    if !gen_status.success() {
        eprintln!(
            "Skipping nvenc pipeline regression test: ffmpeg h264_nvenc unavailable (status: {})",
            gen_status
        );
        return Ok(());
    }

    let mut cmd = AssertCommand::cargo_bin("direct_play_nice")?;
    if let Some(ld_path) = nv_library_env_value() {
        cmd.env("LD_LIBRARY_PATH", ld_path);
    }
    let output_status = cmd
        .args([
            "--hw-accel",
            "nvenc",
            "--video-quality",
            "720p",
            input.to_str().expect("path utf8"),
            output.to_str().expect("path utf8"),
        ])
        .output()?;

    if !output_status.status.success() {
        let stderr = String::from_utf8_lossy(&output_status.stderr);
        if stderr.contains("CUDA_ERROR")
            || stderr.contains("No hardware decoder")
            || stderr.contains("No hardware encoder")
        {
            eprintln!(
                "Skipping nvenc pipeline regression test: hardware unavailable ({})",
                stderr.trim()
            );
            return Ok(());
        }
        panic!("direct_play_nice failed: {}", stderr);
    }

    let input_duration = ffprobe_duration(&input)?;
    let output_duration = ffprobe_duration(&output)?;
    let duration_delta = (input_duration - output_duration).abs();
    assert!(
        duration_delta < 0.25,
        "nvenc duration drift too large: input={} output={} delta={}",
        input_duration,
        output_duration,
        duration_delta
    );

    let output_rate = ffprobe_avg_frame_rate(&output)?;
    assert!(
        (output_rate - 29.97).abs() < 0.5,
        "nvenc unexpected avg_frame_rate {}",
        output_rate
    );

    Ok(())
}
