//! Integration tests for skipping non-supported/auxiliary stream types
//!
//! Validates that the CLI handles containers with attachments and
//! attached picture image streams without failing and still produces a
//! valid Chromecast-direct-play MP4.

use assert_cmd::prelude::*;
use predicates::str;
use std::ffi::CString;
use std::path::PathBuf;
use std::process::Command;
use tempfile::TempDir;

use rsmpeg::avformat::AVFormatContextInput;
use rsmpeg::ffi;

fn ensure_ffmpeg_present() {
    let out = Command::new("ffmpeg").arg("-version").output();
    match out {
        Ok(o) if o.status.success() => return,
        _ => panic!("ffmpeg CLI not found. Install ffmpeg and ensure it is on PATH."),
    }
}

fn probe_duration_ms(path: &PathBuf) -> u64 {
    let path_cstr = CString::new(path.to_string_lossy().to_string()).unwrap();
    let ictx = AVFormatContextInput::open(path_cstr.as_c_str()).unwrap();
    (ictx.duration as i64 / 1000).max(0) as u64
}

#[test]
fn cli_skips_mkv_attachment_streams() -> Result<(), Box<dyn std::error::Error>> {
    ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let dir = tmp.path();
    let video = dir.join("v.mkv");
    let audio = dir.join("a.mp2");
    let attach = dir.join("note.txt");
    let input = dir.join("input_with_attach.mkv");

    // Create tiny assets
    std::fs::write(&attach, b"hello attachment")?;
    assert!(
        Command::new("ffmpeg")
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
            .status()?
            .success(),
        "ffmpeg video generation failed"
    );
    assert!(
        Command::new("ffmpeg")
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
            .status()?
            .success(),
        "ffmpeg audio generation failed"
    );

    // Mux MKV with an attachment stream
    assert!(
        Command::new("ffmpeg")
            .args([
                "-y",
                "-i",
                &video.to_string_lossy(),
                "-i",
                &audio.to_string_lossy(),
                "-attach",
                &attach.to_string_lossy(),
                "-metadata:s:t",
                "mimetype=text/plain",
                "-c:v",
                "copy",
                "-c:a",
                "copy",
                &input.to_string_lossy(),
            ])
            .status()?
            .success(),
        "ffmpeg mux with attachment failed"
    );

    let in_ms = probe_duration_ms(&input);
    let output = dir.join("out_attach.mp4");

    // Run CLI
    let mut cmd = Command::cargo_bin("direct_play_nice")?;
    cmd.arg("-s")
        .arg("chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra")
        .arg(&input)
        .arg(&output);
    cmd.assert().success().stdout(str::is_empty());

    // Validate
    assert!(output.exists(), "output file was not created");
    let output_cstr = CString::new(output.to_string_lossy().to_string()).unwrap();
    let octx = AVFormatContextInput::open(output_cstr.as_c_str())?;
    let mut saw_v = false;
    let mut saw_a = false;
    for st in octx.streams() {
        let par = st.codecpar();
        if par.codec_type == ffi::AVMEDIA_TYPE_VIDEO {
            saw_v = true;
        }
        if par.codec_type == ffi::AVMEDIA_TYPE_AUDIO {
            saw_a = true;
        }
        // ensure no attachments made it into output
        assert_ne!(
            par.codec_type,
            ffi::AVMEDIA_TYPE_ATTACHMENT,
            "attachment leaked to output"
        );
    }
    assert!(saw_v && saw_a, "missing A/V streams in output");

    let out_ms = probe_duration_ms(&output);
    let diff = if out_ms > in_ms {
        out_ms - in_ms
    } else {
        in_ms - out_ms
    };
    assert!(
        diff <= 200,
        "duration drift too large: in={}ms out={}ms",
        in_ms,
        out_ms
    );
    Ok(())
}

#[test]
fn cli_skips_mkv_font_attachment_streams() -> Result<(), Box<dyn std::error::Error>> {
    ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let dir = tmp.path();
    let video = dir.join("vf.mkv");
    let audio = dir.join("af.mp2");
    let font = dir.join("fakefont.ttf");
    let input = dir.join("input_with_font.mkv");

    // Minimal fake font payload (extension and mimetype drive codec detection).
    std::fs::write(&font, vec![0u8; 1024])?;

    // Generate short video/audio assets if needed.
    assert!(
        Command::new("ffmpeg")
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
            .status()?
            .success(),
        "ffmpeg video generation failed"
    );
    assert!(
        Command::new("ffmpeg")
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
            .status()?
            .success(),
        "ffmpeg audio generation failed"
    );

    assert!(
        Command::new("ffmpeg")
            .args([
                "-y",
                "-i",
                &video.to_string_lossy(),
                "-i",
                &audio.to_string_lossy(),
                "-attach",
                &font.to_string_lossy(),
                "-metadata:s:t",
                "mimetype=application/x-truetype-font",
                "-metadata:s:t",
                "filename=fakefont.ttf",
                "-c:v",
                "copy",
                "-c:a",
                "copy",
                &input.to_string_lossy(),
            ])
            .status()?
            .success(),
        "ffmpeg mux with font attachment failed"
    );

    let output = dir.join("out_font.mp4");

    let mut cmd = Command::cargo_bin("direct_play_nice")?;
    cmd.arg("-s")
        .arg("chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra")
        .arg(&input)
        .arg(&output);
    cmd.assert().success().stdout(str::is_empty());

    assert!(output.exists(), "output file was not created");
    let output_cstr = CString::new(output.to_string_lossy().to_string()).unwrap();
    let octx = AVFormatContextInput::open(output_cstr.as_c_str())?;
    for st in octx.streams() {
        let par = st.codecpar();
        assert_ne!(
            par.codec_type,
            ffi::AVMEDIA_TYPE_ATTACHMENT,
            "font attachment leaked to output"
        );
    }

    Ok(())
}

#[test]
fn cli_skips_webvtt_subtitles_but_keeps_text_streams() -> Result<(), Box<dyn std::error::Error>> {
    ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let dir = tmp.path();
    let video = dir.join("vwv.mkv");
    let audio = dir.join("awv.mp2");
    let ass_file = dir.join("sample.ass");
    let srt_file = dir.join("sample.srt");
    let vtt_file = dir.join("sample.vtt");
    let input = dir.join("input_with_vtt.mkv");

    // Prepare subtitle sources
    std::fs::write(
        &ass_file,
        "[Script Info]\nScriptType: v4.00+\n\n[V4+ Styles]\nFormat: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding\nStyle: Default,Arial,20,&H00FFFFFF,&H000000FF,&H00000000,&H00000000,0,0,0,0,100,100,0,0,1,1,0,2,10,10,10,1\n\n[Events]\nFormat: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text\nDialogue: 0,0:00:00.00,0:00:01.00,Default,,0,0,0,,Hello ASS\n",
    )?;
    std::fs::write(
        &srt_file,
        "1\n00:00:00,000 --> 00:00:01,000\nBonjour SRT\n\n2\n00:00:01,200 --> 00:00:02,000\nEncore SRT\n",
    )?;
    std::fs::write(
        &vtt_file,
        "WEBVTT\n\n00:00:00.000 --> 00:00:01.000\nBonjour VTT\n\n00:00:01.200 --> 00:00:02.000\nEncore VTT\n",
    )?;

    // Base video and audio assets
    assert!(
        Command::new("ffmpeg")
            .args([
                "-y",
                "-f",
                "lavfi",
                "-i",
                "testsrc=size=160x120:rate=25:duration=3",
                "-pix_fmt",
                "yuv420p",
                "-c:v",
                "mpeg4",
                &video.to_string_lossy(),
            ])
            .status()?
            .success(),
        "ffmpeg video generation failed"
    );
    assert!(
        Command::new("ffmpeg")
            .args([
                "-y",
                "-f",
                "lavfi",
                "-i",
                "sine=frequency=500:sample_rate=44100:duration=3",
                "-c:a",
                "mp2",
                &audio.to_string_lossy(),
            ])
            .status()?
            .success(),
        "ffmpeg audio generation failed"
    );

    assert!(
        Command::new("ffmpeg")
            .args([
                "-y",
                "-i",
                &video.to_string_lossy(),
                "-i",
                &audio.to_string_lossy(),
                "-i",
                &ass_file.to_string_lossy(),
                "-i",
                &srt_file.to_string_lossy(),
                "-i",
                &vtt_file.to_string_lossy(),
                "-map",
                "0:v",
                "-map",
                "1:a",
                "-map",
                "2:0",
                "-map",
                "3:0",
                "-map",
                "4:0",
                "-c:v",
                "copy",
                "-c:a",
                "copy",
                "-c:s:0",
                "ass",
                "-c:s:1",
                "srt",
                "-c:s:2",
                "webvtt",
                &input.to_string_lossy(),
            ])
            .status()?
            .success(),
        "ffmpeg mux with VTT failed"
    );

    let output = dir.join("out_vtt.mp4");

    let mut cmd = Command::cargo_bin("direct_play_nice")?;
    cmd.arg("-s")
        .arg("chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra")
        .arg(&input)
        .arg(&output);
    cmd.assert().success().stdout(str::is_empty());

    let output_cstr = CString::new(output.to_string_lossy().to_string()).unwrap();
    let octx = AVFormatContextInput::open(output_cstr.as_c_str())?;
    let mut subtitle_count = 0usize;
    for st in octx.streams() {
        let par = st.codecpar();
        if par.codec_type == ffi::AVMEDIA_TYPE_SUBTITLE {
            subtitle_count += 1;
            assert_eq!(par.codec_id, ffi::AV_CODEC_ID_MOV_TEXT);
        }
    }
    assert!(
        subtitle_count >= 2,
        "Unexpected subtitle count in output (found {})",
        subtitle_count
    );

    Ok(())
}

#[test]
fn cli_skips_mp4_attached_picture_streams() -> Result<(), Box<dyn std::error::Error>> {
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let dir = tmp.path();
    let video = dir.join("v.mp4");
    let audio = dir.join("a.aac");
    let cover = dir.join("cover.png");
    let input = dir.join("input_with_cover.mp4");

    // Base video
    assert!(
        Command::new("ffmpeg")
            .args([
                "-y",
                "-f",
                "lavfi",
                "-i",
                "testsrc=size=160x120:rate=25:duration=2",
                "-pix_fmt",
                "yuv420p",
                "-c:v",
                "libx264",
                "-t",
                "2",
                &video.to_string_lossy(),
            ])
            .status()?
            .success(),
        "ffmpeg video generation failed"
    );
    // AAC audio
    assert!(
        Command::new("ffmpeg")
            .args([
                "-y",
                "-f",
                "lavfi",
                "-i",
                "sine=frequency=1000:sample_rate=44100:duration=2",
                "-c:a",
                "aac",
                &audio.to_string_lossy(),
            ])
            .status()?
            .success(),
        "ffmpeg audio generation failed"
    );
    // Small PNG cover
    assert!(
        Command::new("ffmpeg")
            .args([
                "-y",
                "-f",
                "lavfi",
                "-i",
                "color=c=red:s=64x64:d=0.1",
                "-frames:v",
                "1",
                &cover.to_string_lossy(),
            ])
            .status()?
            .success(),
        "ffmpeg cover generation failed"
    );

    // Mux MP4 with attached picture stream
    assert!(
        Command::new("ffmpeg")
            .args([
                "-y",
                "-i",
                &video.to_string_lossy(),
                "-i",
                &audio.to_string_lossy(),
                "-i",
                &cover.to_string_lossy(),
                "-map",
                "0:v:0",
                "-map",
                "1:a:0",
                "-map",
                "2:v:0",
                "-c:v:0",
                "copy",
                "-c:a",
                "copy",
                "-c:v:1",
                "mjpeg",
                "-disposition:v:1",
                "attached_pic",
                &input.to_string_lossy(),
            ])
            .status()?
            .success(),
        "ffmpeg mux with attached picture failed"
    );

    let output = dir.join("out_cover.mp4");
    let mut cmd = Command::cargo_bin("direct_play_nice")?;
    cmd.arg("-s")
        .arg("chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra")
        .arg(&input)
        .arg(&output);
    cmd.assert().success().stdout(str::is_empty());

    assert!(output.exists(), "output file was not created");
    // Minimal validation that we have a playable A/V MP4 and no attachments
    let final_cstr = CString::new(output.to_string_lossy().to_string()).unwrap();
    let octx = AVFormatContextInput::open(final_cstr.as_c_str())?;
    for st in octx.streams() {
        let par = st.codecpar();
        assert_ne!(
            par.codec_type,
            ffi::AVMEDIA_TYPE_ATTACHMENT,
            "attachment leaked to output"
        );
    }
    Ok(())
}
