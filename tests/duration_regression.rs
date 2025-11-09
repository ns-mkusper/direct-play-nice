mod common;

use assert_cmd::Command;
use common::{ffprobe_avg_frame_rate, ffprobe_duration};
use std::error::Error;
use std::process::Command as StdCommand;
use tempfile::tempdir;

#[test]
fn preserves_duration_and_fps_after_transcode() -> Result<(), Box<dyn Error>> {
    common::ensure_ffmpeg_present();

    let tmp = tempdir()?;
    let input = tmp.path().join("kaiji_like.mkv");
    let output = tmp.path().join("kaiji_like.fixed.mp4");

    let status = StdCommand::new("ffmpeg")
        .args([
            "-hide_banner",
            "-loglevel",
            "error",
            "-y",
            "-f",
            "lavfi",
            "-i",
            "testsrc2=size=1920x1080:rate=30000/1001",
            "-f",
            "lavfi",
            "-i",
            "sine=frequency=440:sample_rate=48000",
            "-shortest",
            "-t",
            "4",
            "-pix_fmt",
            "yuv420p",
            "-c:v",
            "libx264",
            "-c:a",
            "aac",
            input.to_str().expect("path utf8"),
        ])
        .status()?;
    assert!(status.success(), "ffmpeg sample generation failed");

    Command::cargo_bin("direct_play_nice")?
        .args([
            "--hw-accel",
            "none",
            "--video-quality",
            "720p",
            input.to_str().expect("path utf8"),
            output.to_str().expect("path utf8"),
        ])
        .assert()
        .success();

    let input_duration = ffprobe_duration(&input)?;
    let output_duration = ffprobe_duration(&output)?;
    let duration_delta = (input_duration - output_duration).abs();
    assert!(
        duration_delta < 0.25,
        "duration drift too large: input={} output={} delta={}",
        input_duration,
        output_duration,
        duration_delta
    );

    let output_rate = ffprobe_avg_frame_rate(&output)?;
    assert!(
        (output_rate - 29.97).abs() < 0.5,
        "unexpected avg_frame_rate {}",
        output_rate
    );

    Ok(())
}
