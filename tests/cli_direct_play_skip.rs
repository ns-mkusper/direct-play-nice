#![cfg(feature = "ffmpeg-cli-tests")]

//! Integration tests covering the direct-play compatibility preflight.

use assert_cmd::prelude::*;
use predicates::prelude::PredicateBooleanExt;
use predicates::str;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

use rsmpeg::avformat::AVFormatContextInput;
use rsmpeg::ffi;

fn ensure_ffmpeg_present() {
    let out = Command::new("ffmpeg").arg("-version").output();
    match out {
        Ok(o) if o.status.success() => (),
        _ => panic!("ffmpeg CLI not found. Install ffmpeg and ensure it is on PATH."),
    }
}

fn new_temp_mp4(path: &std::path::Path) {
    // Generates a 720p H.264 High@4.1 video with AAC audio that's within Chromecast limits.
    assert!(
        Command::new("ffmpeg")
            .args([
                "-y",
                "-f",
                "lavfi",
                "-i",
                "testsrc=size=1280x720:rate=30:duration=2",
                "-f",
                "lavfi",
                "-i",
                "sine=frequency=1000:sample_rate=48000:duration=2",
                "-c:v",
                "libx264",
                "-b:v",
                "4M",
                "-minrate",
                "4M",
                "-maxrate",
                "4M",
                "-bufsize",
                "8M",
                "-profile:v",
                "high",
                "-level:v",
                "4.1",
                "-pix_fmt",
                "yuv420p",
                "-c:a",
                "aac",
                "-b:a",
                "128k",
                "-movflags",
                "+faststart",
                path.to_string_lossy().as_ref(),
            ])
            .status()
            .expect("invoke ffmpeg")
            .success(),
        "ffmpeg failed to generate direct-play sample"
    );
}

fn probe_stream_counts(path: &std::path::Path) -> (usize, usize, usize) {
    let ictx = AVFormatContextInput::open(
        std::ffi::CString::new(path.to_string_lossy().to_string())
            .unwrap()
            .as_c_str(),
    )
    .expect("open media for probe");
    let mut video = 0;
    let mut audio = 0;
    let mut subtitles = 0;
    for st in ictx.streams() {
        match st.codecpar().codec_type {
            t if t == ffi::AVMEDIA_TYPE_VIDEO => video += 1,
            t if t == ffi::AVMEDIA_TYPE_AUDIO => audio += 1,
            t if t == ffi::AVMEDIA_TYPE_SUBTITLE => subtitles += 1,
            _ => {}
        }
    }
    (video, audio, subtitles)
}

fn probe_audio_codec(path: &std::path::Path) -> ffi::AVCodecID {
    let ictx = AVFormatContextInput::open(
        std::ffi::CString::new(path.to_string_lossy().to_string())
            .unwrap()
            .as_c_str(),
    )
    .expect("open media for probe");
    for st in ictx.streams() {
        let par = st.codecpar();
        if par.codec_type == ffi::AVMEDIA_TYPE_AUDIO {
            return par.codec_id;
        }
    }
    ffi::AV_CODEC_ID_NONE
}

#[test]
fn cli_skips_when_input_already_direct_play() -> Result<(), Box<dyn std::error::Error>> {
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let input = tmp.path().join("already_direct_play.mp4");
    let output = tmp.path().join("should_not_exist.mp4");

    new_temp_mp4(&input);

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.arg("-s")
        .arg("chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra")
        .arg(&input)
        .arg(&output);

    cmd.assert()
        .success()
        .stderr(str::contains("Input is direct-play compatible"));

    assert!(
        !output.exists(),
        "skip path should not create an output file"
    );

    Ok(())
}

#[test]
fn cli_remuxes_track_bloated_direct_play_file() -> Result<(), Box<dyn std::error::Error>> {
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let input = tmp.path().join("bloated.mp4");
    let output = tmp.path().join("clean.mp4");

    let mut subtitle_paths = Vec::new();
    for index in 0..9 {
        let path = tmp.path().join(format!("sub-{index}.srt"));
        fs::write(&path, "1\n00:00:00,000 --> 00:00:01,000\nhello\n")?;
        subtitle_paths.push(path);
    }

    let mut args = vec![
        "-hide_banner".to_string(),
        "-loglevel".to_string(),
        "error".to_string(),
        "-y".to_string(),
        "-f".to_string(),
        "lavfi".to_string(),
        "-i".to_string(),
        "testsrc=size=640x360:rate=30:duration=2".to_string(),
    ];
    for freq in [440, 550, 660, 770, 880] {
        args.extend([
            "-f".to_string(),
            "lavfi".to_string(),
            "-i".to_string(),
            format!("sine=frequency={freq}:sample_rate=48000:duration=2"),
        ]);
    }
    for path in &subtitle_paths {
        args.extend(["-i".to_string(), path.to_string_lossy().to_string()]);
    }
    args.extend([
        "-map".to_string(),
        "0:v:0".to_string(),
        "-map".to_string(),
        "1:a:0".to_string(),
        "-map".to_string(),
        "2:a:0".to_string(),
        "-map".to_string(),
        "3:a:0".to_string(),
        "-map".to_string(),
        "4:a:0".to_string(),
        "-map".to_string(),
        "5:a:0".to_string(),
    ]);
    for idx in 6..15 {
        args.extend(["-map".to_string(), format!("{idx}:s:0")]);
    }
    args.extend([
        "-c:v".to_string(),
        "libx264".to_string(),
        "-b:v".to_string(),
        "700k".to_string(),
        "-profile:v".to_string(),
        "high".to_string(),
        "-level:v".to_string(),
        "4.1".to_string(),
        "-pix_fmt".to_string(),
        "yuv420p".to_string(),
        "-c:a".to_string(),
        "aac".to_string(),
        "-b:a".to_string(),
        "96k".to_string(),
        "-c:s".to_string(),
        "mov_text".to_string(),
        "-metadata:s:a:0".to_string(),
        "language=jpn".to_string(),
        "-metadata:s:a:1".to_string(),
        "language=eng".to_string(),
        "-metadata:s:s:0".to_string(),
        "language=eng".to_string(),
        input.to_string_lossy().to_string(),
    ]);
    assert!(
        Command::new("ffmpeg").args(args).status()?.success(),
        "ffmpeg failed to generate bloated direct-play sample"
    );
    assert_eq!(probe_stream_counts(&input), (1, 5, 9));

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.arg("-s")
        .arg("chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra")
        .arg(&input)
        .arg(&output);

    cmd.assert()
        .success()
        .stderr(str::contains("bloated audio/subtitle track layout"));
    assert_eq!(probe_stream_counts(&output), (1, 2, 1));

    Ok(())
}

#[test]
fn cli_transcodes_when_audio_incompatible() -> Result<(), Box<dyn std::error::Error>> {
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let baseline = tmp.path().join("baseline_direct_play.mp4");
    let input = tmp.path().join("direct_play_video_mp3_audio.mp4");
    let output = tmp.path().join("transcoded.mp4");

    // Start from a known good direct-play file then swap audio to MP3 to trigger transcode.
    new_temp_mp4(&baseline);
    // Remux with MP3 audio only.
    assert!(
        Command::new("ffmpeg")
            .args([
                "-y",
                "-i",
                baseline.to_string_lossy().as_ref(),
                "-f",
                "lavfi",
                "-i",
                "sine=frequency=500:sample_rate=44100:duration=2",
                "-map",
                "0:v:0",
                "-map",
                "1:a:0",
                "-c:v",
                "copy",
                "-c:a",
                "libmp3lame",
                input.to_string_lossy().as_ref(),
            ])
            .status()
            .expect("ffmpeg remux")
            .success(),
        "ffmpeg failed to replace audio with MP3"
    );

    // Sanity check: ensure audio codec really is MP3 pre-conversion.
    assert_eq!(
        probe_audio_codec(&input),
        ffi::AV_CODEC_ID_MP3,
        "precondition: source audio must be MP3"
    );

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.arg("-s")
        .arg("chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra")
        .arg(&input)
        .arg(&output);

    cmd.assert().success().stderr(str::contains(
        "no audio stream with compatible codec AAC found",
    ));

    assert!(output.exists(), "output should exist after transcode");
    assert_eq!(
        probe_audio_codec(&output),
        ffi::AV_CODEC_ID_AAC,
        "audio should be transcoded to AAC"
    );

    Ok(())
}

#[test]
fn cli_skips_when_quality_caps_are_met() -> Result<(), Box<dyn std::error::Error>> {
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let input = tmp.path().join("quality_ok.mp4");
    let output = tmp.path().join("should_not_exist.mp4");

    new_temp_mp4(&input);

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.arg("-s")
        .arg("chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra")
        .arg("--video-quality")
        .arg("720p")
        .arg("--audio-quality")
        .arg("128k")
        .arg("--max-video-bitrate")
        .arg("4.5M")
        .arg(&input)
        .arg(&output);

    cmd.assert()
        .success()
        .stderr(str::contains("Input is direct-play compatible"));

    assert!(!output.exists(), "skip path should not emit an output file");

    Ok(())
}

#[test]
fn cli_transcodes_when_video_bitrate_exceeds_cap() -> Result<(), Box<dyn std::error::Error>> {
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let input = tmp.path().join("bitrate_too_high.mp4");
    let output = tmp.path().join("transcoded.mp4");

    new_temp_mp4(&input);

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.arg("-s")
        .arg("chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra")
        .arg("--max-video-bitrate")
        .arg("500k")
        .arg(&input)
        .arg(&output);

    cmd.assert()
        .success()
        .stderr(str::contains("video bitrate").and(str::contains("requested limit")));

    assert!(output.exists(), "output should exist after transcode");

    Ok(())
}

#[test]
fn cli_transcodes_when_video_resolution_exceeds_quality() -> Result<(), Box<dyn std::error::Error>>
{
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let input = tmp.path().join("resolution_too_high.mp4");
    let output = tmp.path().join("resized.mp4");

    new_temp_mp4(&input);

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.arg("-s")
        .arg("chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra")
        .arg("--video-quality")
        .arg("480p")
        .arg(&input)
        .arg(&output);

    cmd.assert()
        .success()
        .stderr(str::contains("video resolution").and(str::contains("requested quality limit")));

    assert!(output.exists(), "output should exist after transcode");

    Ok(())
}
