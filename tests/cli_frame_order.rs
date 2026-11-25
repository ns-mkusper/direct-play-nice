#![cfg(feature = "ffmpeg-cli-tests")]

#[path = "common/mod.rs"]
mod common;

use common::frame_order::{
    check_scene_frames, create_scene_cut_clip, decoded_scene_frames, SceneFrame, SCENE_COUNT,
    SCENE_FRAMES,
};
use std::error::Error;
use std::path::Path;
use std::process::Command;
use tempfile::tempdir;

fn probe_video(path: &Path) -> Result<serde_json::Value, Box<dyn Error>> {
    let output = Command::new("ffprobe")
        .args([
            "-v",
            "error",
            "-select_streams",
            "v:0",
            "-count_frames",
            "-show_entries",
            "stream=codec_name,width,height,has_b_frames,nb_read_frames:frame=pict_type",
            "-show_frames",
            "-of",
            "json",
        ])
        .arg(path)
        .output()?;
    if !output.status.success() {
        return Err(format!(
            "ffprobe failed: {}",
            String::from_utf8_lossy(&output.stderr)
        )
        .into());
    }
    Ok(serde_json::from_slice(&output.stdout)?)
}

#[test]
fn cpu_transcode_preserves_frame_identity_and_timeline() -> Result<(), Box<dyn Error>> {
    common::ensure_ffmpeg_present();
    let dir = tempdir()?;
    let input = dir.path().join("scene-cuts.mkv");
    let output = dir.path().join("scene-cuts.mp4");
    create_scene_cut_clip(&input)?;

    let input_probe = probe_video(&input)?;
    let video = &input_probe["streams"][0];
    assert_eq!(video["codec_name"], "h264");
    assert!(video["has_b_frames"].as_u64().unwrap_or(0) > 0);
    assert!(
        input_probe["frames"]
            .as_array()
            .expect("input frames")
            .iter()
            .any(|frame| frame["pict_type"] == "B"),
        "fixture must contain B-frames"
    );
    assert_eq!(
        video["nb_read_frames"]
            .as_str()
            .and_then(|value| value.parse::<usize>().ok()),
        Some(SCENE_FRAMES * SCENE_COUNT)
    );

    let result = Command::new(env!("CARGO_BIN_EXE_direct_play_nice"))
        .env("HOME", dir.path())
        .env("XDG_CONFIG_HOME", dir.path().join("config"))
        .env_remove("DIRECT_PLAY_NICE_CONFIG")
        .args([
            "--device",
            "chromecast_2nd_gen",
            "--hw-accel",
            "none",
            "--video-quality",
            "360p",
            "--resize-backend",
            "software",
            "--delete-source",
            "false",
        ])
        .arg(&input)
        .arg(&output)
        .output()?;
    assert!(
        result.status.success(),
        "conversion failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&result.stdout),
        String::from_utf8_lossy(&result.stderr)
    );
    assert!(input.exists(), "conversion deleted the source");
    let output_probe = probe_video(&output)?;
    let video = &output_probe["streams"][0];
    assert_eq!(video["width"], 640, "expected forced resize");
    assert_eq!(video["height"], 360, "expected forced resize");
    assert_eq!(
        video["nb_read_frames"]
            .as_str()
            .and_then(|value| value.parse::<usize>().ok()),
        Some(SCENE_FRAMES * SCENE_COUNT)
    );

    let source = decoded_scene_frames(&input)?;
    let converted = decoded_scene_frames(&output)?;
    check_scene_frames(&source, &converted)
        .map_err(|message| format!("CPU transcode: {message}"))?;
    common::assert_video_timestamps_match(&input, &output, 0.025);
    common::assert_audio_packet_range_matches(&input, &output, 0.1);
    Ok(())
}

#[test]
fn marker_decoder_rejects_ambiguous_blocks() -> Result<(), Box<dyn Error>> {
    common::ensure_ffmpeg_present();
    let dir = tempdir()?;
    let input = dir.path().join("scene-cuts.mkv");
    let ambiguous = dir.path().join("ambiguous.mkv");
    create_scene_cut_clip(&input)?;
    let output = Command::new("ffmpeg")
        .args(["-hide_banner", "-loglevel", "error", "-y", "-i"])
        .arg(&input)
        .args([
            "-vf",
            "drawbox=x=24:y=24:w=48:h=48:color=gray:t=fill",
            "-c:v",
            "libx264",
            "-c:a",
            "copy",
        ])
        .arg(&ambiguous)
        .output()?;
    assert!(
        output.status.success(),
        "could not create ambiguous marker: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let error = decoded_scene_frames(&ambiguous).unwrap_err().to_string();
    assert!(
        error.contains("frame 0 marker bit 0 is ambiguous"),
        "unexpected error: {error}"
    );
    Ok(())
}

#[test]
fn identity_oracle_rejects_future_frame_at_unchanged_timestamp() {
    let colors = [
        [255.0, 0.0, 0.0],
        [0.0, 0.0, 255.0],
        [0.0, 128.0, 0.0],
        [255.0, 255.0, 0.0],
    ];
    let source: Vec<_> = (0..SCENE_FRAMES * SCENE_COUNT)
        .map(|index| {
            (
                index,
                SceneFrame {
                    mean: colors[index / SCENE_FRAMES],
                    marker: index,
                },
            )
        })
        .collect();
    let source_frames: Vec<_> = source.iter().map(|(_, frame)| frame.clone()).collect();
    assert!(check_scene_frames(&source_frames, &source_frames).is_ok());

    // The timestamps and frame count are unchanged; only the image is replaced.
    // This tests the oracle, not whether CUVID exhibits the fault on this fixture.
    for (index, future) in [(5, 7), (11, 25)] {
        let mut output = source.clone();
        output[index].1 = source[future].1.clone();
        assert_eq!(output.len(), source.len());
        assert!(output
            .iter()
            .zip(&source)
            .all(|(actual, expected)| actual.0 == expected.0));
        let output_frames: Vec<_> = output.iter().map(|(_, frame)| frame.clone()).collect();
        let error = check_scene_frames(&source_frames, &output_frames).unwrap_err();
        assert_eq!(
            error,
            format!("frame {index}: expected marker {index}, got {future}")
        );
    }

    let mut wrong_scene = source_frames.clone();
    wrong_scene[11].mean = source_frames[25].mean;
    assert!(check_scene_frames(&source_frames, &wrong_scene)
        .unwrap_err()
        .contains("frame 11: expected scene 0, got 2"));

    let mut invalid_source = source_frames.clone();
    invalid_source[3].marker = 4;
    assert!(check_scene_frames(&invalid_source, &source_frames)
        .unwrap_err()
        .contains("fixture frame 3: expected marker 3, got 4"));
}
