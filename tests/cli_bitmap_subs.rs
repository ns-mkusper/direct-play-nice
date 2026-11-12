#![cfg(feature = "ffmpeg-cli-tests")]

//! Integration test: converts an input with bitmap subtitles
//! and verifies the output is Chromecast direct‑play compatible with
//! text subs (MOV_TEXT) and intact timing. We prefer PGS, but try other
//! bitmap subtitle codecs when the local ffmpeg build lacks a PGS encoder.

use assert_cmd::prelude::*;
use predicates::str;
use std::env;
use std::ffi::CString;
use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};
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

fn mk_subs_file(path: &Path) {
    let mut f = File::create(path).expect("create srt");
    writeln!(
        f,
        "1\n00:00:00,000 --> 00:00:00,800\nhello bitmap\n\n2\n00:00:01,000 --> 00:00:01,600\nsecond line\n"
    )
    .unwrap();
}

fn mk_subs_file_with_text(path: &Path, first: &str, second: &str) {
    let mut f = File::create(path).expect("create srt");
    writeln!(
        f,
        "1\n00:00:00,000 --> 00:00:00,800\n{}\n\n2\n00:00:01,000 --> 00:00:01,600\n{}\n",
        first, second
    )
    .unwrap();
}

fn gen_problem_input_with_bitmap_subs(tmp: &TempDir) -> Option<(PathBuf, u64)> {
    let dir = tmp.path();
    let video = dir.join("v.mkv");
    let audio = dir.join("a.mp2");
    let subs = dir.join("subs.srt");
    let input = dir.join("input_bitmap.mkv");

    mk_subs_file(&subs);

    // Tiny source: 2s MPEG4 yuv420p
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

    for codec in ["hdmv_pgs_subtitle", "dvdsub", "dvb_subtitle"] {
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
        if !status_mux.success() {
            continue;
        }

        let input_cstr = CString::new(input.to_string_lossy().to_string()).unwrap();
        let ictx = AVFormatContextInput::open(input_cstr.as_c_str()).unwrap();
        let dur_ms = (ictx.duration / 1000).max(0) as u64;
        return Some((input, dur_ms));
    }

    None
}

fn gen_multi_stream_bitmap_input(tmp: &TempDir) -> Option<PathBuf> {
    let dir = tmp.path();
    let video = dir.join("v_multi.mkv");
    let audio = dir.join("a_multi.mp2");
    let subs_eng = dir.join("subs_eng.srt");
    let subs_spa = dir.join("subs_spa.srt");
    let input = dir.join("input_multi_bitmap.mkv");

    mk_subs_file_with_text(&subs_eng, "hello world", "second english line");
    mk_subs_file_with_text(&subs_spa, "hola mundo", "segunda linea");

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

    let candidates = ["hdmv_pgs_subtitle", "dvdsub", "dvb_subtitle"];
    for codec in candidates {
        let status_mux = Command::new("ffmpeg")
            .args([
                "-y",
                "-i",
                &video.to_string_lossy(),
                "-i",
                &audio.to_string_lossy(),
                "-i",
                &subs_eng.to_string_lossy(),
                "-i",
                &subs_spa.to_string_lossy(),
                "-c:v",
                "copy",
                "-c:a",
                "copy",
                "-c:s",
                codec,
                "-metadata:s:s:0",
                "language=eng",
                "-metadata:s:s:1",
                "language=spa",
                "-map",
                "0:v:0",
                "-map",
                "1:a:0",
                "-map",
                "2:0",
                "-map",
                "3:0",
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

fn is_bitmap_subtitle_codec(codec_id: ffi::AVCodecID) -> bool {
    matches!(
        codec_id,
        ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE
            | ffi::AV_CODEC_ID_DVD_SUBTITLE
            | ffi::AV_CODEC_ID_DVB_SUBTITLE
            | ffi::AV_CODEC_ID_XSUB
    )
}

fn count_bitmap_subtitle_streams(path: &Path) -> Result<usize, Box<dyn std::error::Error>> {
    let path_cstr = CString::new(path.to_string_lossy().to_string())?;
    let ictx = AVFormatContextInput::open(path_cstr.as_c_str())?;
    Ok(ictx
        .streams()
        .iter()
        .filter(|st| {
            let cp = st.codecpar();
            cp.codec_type == ffi::AVMEDIA_TYPE_SUBTITLE && is_bitmap_subtitle_codec(cp.codec_id)
        })
        .count())
}

fn count_mov_text_streams(path: &Path) -> Result<usize, Box<dyn std::error::Error>> {
    let path_cstr = CString::new(path.to_string_lossy().to_string())?;
    let ictx = AVFormatContextInput::open(path_cstr.as_c_str())?;
    Ok(ictx
        .streams()
        .iter()
        .filter(|st| {
            let cp = st.codecpar();
            cp.codec_type == ffi::AVMEDIA_TYPE_SUBTITLE && cp.codec_id == ffi::AV_CODEC_ID_MOV_TEXT
        })
        .count())
}

fn probe_duration_ms(path: &Path) -> u64 {
    let path_cstr = CString::new(path.to_string_lossy().to_string()).unwrap();
    let ictx = AVFormatContextInput::open(path_cstr.as_c_str()).unwrap();
    (ictx.duration / 1000).max(0) as u64
}

// Construct a small SUP stream directly: FFmpeg's bitmap encoders cannot turn
// SRT into pictures. Each visible display set carries a real PGS RLE object.
fn synthetic_pgs() -> Vec<u8> {
    const PALETTE: u8 = 0x14;
    const OBJECT: u8 = 0x15;
    const PRESENTATION: u8 = 0x16;
    const WINDOW: u8 = 0x17;
    const END: u8 = 0x80;

    fn segment(out: &mut Vec<u8>, ms: u32, kind: u8, data: &[u8]) {
        out.extend_from_slice(b"PG");
        out.extend_from_slice(&(ms * 90).to_be_bytes());
        out.extend_from_slice(&(ms * 90).to_be_bytes());
        out.push(kind);
        out.extend_from_slice(&(data.len() as u16).to_be_bytes());
        out.extend_from_slice(data);
    }
    fn display(out: &mut Vec<u8>, ms: u32, number: u16, visible: bool) {
        // 160x120 canvas, composition state = epoch start.
        let mut pcs = vec![0, 160, 0, 120, 0x10];
        pcs.extend_from_slice(&number.to_be_bytes());
        pcs.extend_from_slice(&[0x80, 0, 0, u8::from(visible)]);
        if visible {
            // Object 0 in window 0, positioned at (48, 80).
            pcs.extend_from_slice(&[0, 0, 0, 0, 0, 48, 0, 80]);
        }
        segment(out, ms, PRESENTATION, &pcs);
        if visible {
            // Distinct rectangle widths make replacement events visibly different.
            let width = 32 - (number as u8 * 2);
            segment(out, ms, WINDOW, &[1, 0, 0, 48, 0, 80, 0, width, 0, 16]);
            segment(
                out,
                ms,
                PALETTE,
                &[0, 0, 0, 16, 128, 128, 0, 1, 235, 128, 128, 255],
            );
            let mut rle = Vec::new();
            for _ in 0..16 {
                rle.extend_from_slice(&[0, 0x80 | width, 1, 0, 0]);
            }
            // Object 0, version 0, first and last fragment, 24-bit data length.
            let len = (rle.len() + 4) as u32;
            let mut object = vec![0, 0, 0, 0xc0];
            object.extend_from_slice(&len.to_be_bytes()[1..]);
            object.extend_from_slice(&[0, width, 0, 16]);
            object.extend_from_slice(&rle);
            segment(out, ms, OBJECT, &object);
        }
        segment(out, ms, END, &[]);
    }
    let mut data = Vec::new();
    for (n, (ms, visible)) in [
        (500, true),
        (1000, false),
        (1500, true),
        (2000, true),
        (2600, false),
        (3000, true),
    ]
    .into_iter()
    .enumerate()
    {
        display(&mut data, ms, n as u16, visible);
    }
    data
}

/// Parses an SRT `HH:MM:SS,mmm` timestamp into milliseconds.
fn srt_timestamp_ms(timestamp: &str) -> i64 {
    let parts: Vec<i64> = timestamp
        .split([':', ','])
        .map(|part| part.parse().expect("timestamp component"))
        .collect();
    assert_eq!(parts.len(), 4, "malformed SRT timestamp {timestamp:?}");
    parts[0] * 3_600_000 + parts[1] * 60_000 + parts[2] * 1_000 + parts[3]
}

#[test]
fn cli_synthetic_pgs_preserves_event_timing() -> Result<(), Box<dyn std::error::Error>> {
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let sup = tmp.path().join("synthetic.sup");
    let input = tmp.path().join("input.mkv");
    let output = tmp.path().join("output.mp4");
    let extracted = tmp.path().join("output.srt");
    let config = tmp.path().join("config.toml");
    fs::write(&sup, synthetic_pgs())?;
    fs::write(&config, "")?;

    let mux = Command::new("ffmpeg")
        .args([
            "-v",
            "error",
            "-y",
            "-copyts",
            "-f",
            "lavfi",
            "-i",
            "color=size=160x120:rate=25:duration=5",
            "-f",
            "lavfi",
            "-i",
            "anullsrc=sample_rate=44100:channel_layout=mono",
            "-i",
        ])
        .arg(&sup)
        .args([
            "-map", "0:v:0", "-map", "1:a:0", "-map", "2:s:0", "-t", "5", "-c:v", "mpeg4", "-c:a",
            "aac", "-c:s", "copy",
        ])
        .arg(&input)
        .output()?;
    assert!(
        mux.status.success(),
        "synthetic input mux failed: {}",
        String::from_utf8_lossy(&mux.stderr)
    );
    assert_eq!(count_bitmap_subtitle_streams(&input)?, 1);

    // The external engine is a deterministic stub, not an OCR accuracy test.
    // It lets the real decoded bitmap events traverse cue assembly and muxing.
    let external_ocr = if cfg!(windows) {
        "cmd /C echo SYNTHETIC SAMPLE"
    } else {
        "echo SYNTHETIC SAMPLE"
    };
    let run = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
        .arg("--config-file")
        .arg(&config)
        .args([
            "--hw-accel",
            "none",
            "--sub-mode",
            "force",
            "--ocr-engine",
            "external",
            "--ocr-format",
            "srt",
            "--ocr-external-command",
            external_ocr,
        ])
        .arg(&input)
        .arg(&output)
        .output()?;
    assert!(
        run.status.success(),
        "conversion failed:\n{}",
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(count_mov_text_streams(&output)?, 1);
    let extract = Command::new("ffmpeg")
        .args(["-v", "error", "-y", "-i"])
        .arg(&output)
        .args(["-map", "0:s:0"])
        .arg(&extracted)
        .output()?;
    assert!(
        extract.status.success(),
        "subtitle extraction failed: {}",
        String::from_utf8_lossy(&extract.stderr)
    );
    let srt = fs::read_to_string(extracted)?.replace('\r', "");
    let cues: Vec<_> = srt.trim().split("\n\n").collect();
    // Includes a clear at 1s, a gap to 1.5s, visible replacement at 2s,
    // another clear at 2.6s, and an unclosed final event with a 1s fallback.
    let expected = [(500, 1000), (1500, 2000), (2000, 2600), (3000, 4000)];
    assert_eq!(cues.len(), expected.len(), "unexpected cues:\n{srt}");
    for (index, (cue, (start, end))) in cues.iter().zip(expected).enumerate() {
        let mut lines = cue.lines();
        assert_eq!(lines.next(), Some((index + 1).to_string().as_str()));
        let times = lines.next().expect("cue timestamp");
        let (actual_start, actual_end) = times.split_once(" --> ").expect("SRT timing");
        let actual = (srt_timestamp_ms(actual_start), srt_timestamp_ms(actual_end));
        assert!(
            (actual.0 - start).abs() <= 30 && (actual.1 - end).abs() <= 30,
            "cue {}: expected {start}-{end}ms, got {}-{}ms",
            index + 1,
            actual.0,
            actual.1
        );
        assert_eq!(lines.collect::<Vec<_>>(), ["SYNTHETIC SAMPLE"]);
    }
    Ok(())
}

#[test]
fn cli_converts_bitmap_subs_to_mov_text_and_direct_play() -> Result<(), Box<dyn std::error::Error>>
{
    ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let Some((input, in_dur_ms)) = gen_problem_input_with_bitmap_subs(&tmp) else {
        eprintln!("No bitmap subtitle encoder available; skipping bitmap direct-play test.");
        return Ok(());
    };
    let output = tmp.path().join("out_bitmap.mp4");

    // Run the CLI for all Chromecast models
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.arg("-s")
        .arg("chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra")
        .arg("--audio-quality")
        .arg("192k")
        .arg(&input)
        .arg(&output);
    cmd.assert().success().stdout(str::is_empty());

    assert!(output.exists(), "output file was not created");

    // Validate via rsmpeg
    let output_cstr = CString::new(output.to_string_lossy().to_string()).unwrap();
    let octx = AVFormatContextInput::open(output_cstr.as_c_str())?;

    let mut saw_v = false;
    let mut saw_a = false;
    let mut saw_s = false;
    let mut width = 0i32;
    let mut height = 0i32;
    let mut fps_num = 0i32;
    let mut fps_den = 1i32;
    let mut level = 0i32;
    let mut pix_fmt = -1i32;

    for st in octx.streams() {
        let par = st.codecpar();
        match par.codec_type {
            t if t == ffi::AVMEDIA_TYPE_VIDEO => {
                saw_v = true;
                assert_eq!(par.codec_id, ffi::AV_CODEC_ID_H264, "video must be H.264");
                width = par.width;
                height = par.height;
                level = par.level;
                pix_fmt = par.format;
                let rate = st.avg_frame_rate;
                fps_num = rate.num;
                fps_den = rate.den;
            }
            t if t == ffi::AVMEDIA_TYPE_AUDIO => {
                saw_a = true;
                assert_eq!(par.codec_id, ffi::AV_CODEC_ID_AAC, "audio must be AAC");
            }
            t if t == ffi::AVMEDIA_TYPE_SUBTITLE => {
                saw_s = true;
                assert_eq!(
                    par.codec_id,
                    ffi::AV_CODEC_ID_MOV_TEXT,
                    "subs must be MOV_TEXT"
                );
            }
            _ => {}
        }
    }

    assert!(
        saw_v && saw_a && saw_s,
        "missing one or more required streams"
    );

    assert!(
        width as u32 <= 1920 && height as u32 <= 1080,
        "resolution too high"
    );
    assert!(level <= 41, "H.264 level too high: {}", level);
    assert_eq!(pix_fmt, ffi::AV_PIX_FMT_YUV420P, "pix fmt must be yuv420p");
    if fps_den != 0 {
        let fps = (fps_num as f64) / (fps_den as f64);
        assert!(fps <= 30.01, "fps too high: {}", fps);
    }

    let out_dur_ms = probe_duration_ms(&output);
    let diff = out_dur_ms.abs_diff(in_dur_ms);
    assert!(
        diff <= 200,
        "duration drift too large: in={}ms out={}ms",
        in_dur_ms,
        out_dur_ms
    );

    Ok(())
}

#[test]
fn cli_ai_ocr_processes_all_bitmap_subtitle_streams() -> Result<(), Box<dyn std::error::Error>> {
    ensure_ffmpeg_present();

    if env::var("DPN_OCR_AI_E2E").ok().as_deref() != Some("1") {
        eprintln!("Skipping AI all-stream OCR test (set DPN_OCR_AI_E2E=1 to enable).");
        return Ok(());
    }

    let tmp = TempDir::new()?;
    let Some(input) = gen_multi_stream_bitmap_input(&tmp) else {
        eprintln!("No bitmap subtitle encoder available; skipping AI all-stream OCR test.");
        return Ok(());
    };
    let output = tmp.path().join("out_ai_all_streams.mp4");

    let input_bitmap_count = count_bitmap_subtitle_streams(&input)?;
    assert!(
        input_bitmap_count >= 2,
        "expected at least two bitmap subtitle streams, got {}",
        input_bitmap_count
    );

    let run = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
        .env("DPN_OCR_FORCE_CPU", "1")
        .env("DPN_OCR_SKIP_CLS", "1")
        .arg("--sub-mode")
        .arg("force")
        .arg("--ocr-engine")
        .arg("pp-ocr-v3")
        .arg("--skip-codec-check")
        .arg(&input)
        .arg(&output)
        .output()?;

    assert!(
        run.status.success(),
        "AI OCR run failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );

    let output_sub_count = count_mov_text_streams(&output)?;
    assert_eq!(
        output_sub_count, input_bitmap_count,
        "expected all bitmap subtitle streams to be OCR-converted ({}), got {}",
        input_bitmap_count, output_sub_count
    );

    Ok(())
}

#[test]
fn cli_ai_ocr_gpu_processes_all_bitmap_subtitle_streams() -> Result<(), Box<dyn std::error::Error>>
{
    ensure_ffmpeg_present();

    if env::var("DPN_OCR_GPU_E2E").ok().as_deref() != Some("1") {
        eprintln!("Skipping GPU OCR test (set DPN_OCR_GPU_E2E=1 to enable).");
        return Ok(());
    }

    let tmp = TempDir::new()?;
    let Some(input) = gen_multi_stream_bitmap_input(&tmp) else {
        eprintln!("No bitmap subtitle encoder available; skipping GPU OCR test.");
        return Ok(());
    };
    let output = tmp.path().join("out_ai_gpu_all_streams.mp4");

    let input_bitmap_count = count_bitmap_subtitle_streams(&input)?;
    assert!(
        input_bitmap_count >= 2,
        "expected at least two bitmap subtitle streams, got {}",
        input_bitmap_count
    );

    let run = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
        .env("DPN_OCR_REQUIRE_GPU", "1")
        .env("DPN_OCR_SKIP_CLS", "1")
        .arg("--sub-mode")
        .arg("force")
        .arg("--ocr-engine")
        .arg("auto")
        .arg("--skip-codec-check")
        .arg(&input)
        .arg(&output)
        .output()?;

    assert!(
        run.status.success(),
        "GPU OCR run failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );

    let output_sub_count = count_mov_text_streams(&output)?;
    assert_eq!(
        output_sub_count, input_bitmap_count,
        "expected all bitmap subtitle streams to be OCR-converted ({}), got {}",
        input_bitmap_count, output_sub_count
    );

    Ok(())
}
