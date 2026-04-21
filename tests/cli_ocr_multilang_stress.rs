#![cfg(feature = "ffmpeg-cli-tests")]

#[path = "common/mod.rs"]
mod common;

use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Instant;
use strsim::normalized_levenshtein;
use tempfile::TempDir;

use rsmpeg::avformat::AVFormatContextInput;
use rsmpeg::ffi;

fn write_srt(path: &Path, first: &str, second: &str) {
    let mut file = fs::File::create(path).expect("create srt");
    writeln!(
        file,
        "1\n00:00:00,300 --> 00:00:01,100\n{}\n\n2\n00:00:01,400 --> 00:00:02,100\n{}\n",
        first, second
    )
    .unwrap();
}

fn generate_multilang_bitmap_input(tmp: &TempDir) -> Option<PathBuf> {
    let dir = tmp.path();
    let video = dir.join("video.mkv");
    let audio = dir.join("audio.mp2");
    let input = dir.join("input_multilang_bitmap.mkv");

    let srt_eng = dir.join("subs_eng.srt");
    let srt_spa = dir.join("subs_spa.srt");
    let srt_fra = dir.join("subs_fra.srt");
    let srt_kor = dir.join("subs_kor.srt");
    let srt_jpn = dir.join("subs_jpn.srt");

    write_srt(&srt_eng, "We should leave now.", "It is getting dark.");
    write_srt(&srt_spa, "Debemos irnos ahora.", "Se está haciendo tarde.");
    write_srt(&srt_fra, "Nous devons partir.", "Il se fait tard.");
    write_srt(&srt_kor, "지금 떠나야 해.", "점점 어두워지고 있어.");
    write_srt(&srt_jpn, "今出発しよう。", "だんだん暗くなってきた。");

    let status_video = Command::new("ffmpeg")
        .args([
            "-y",
            "-f",
            "lavfi",
            "-i",
            "testsrc=size=640x360:rate=24:duration=3",
            "-pix_fmt",
            "yuv420p",
            "-c:v",
            "mpeg4",
            &video.to_string_lossy(),
        ])
        .status()
        .expect("run ffmpeg video generation");
    assert!(status_video.success(), "ffmpeg video generation failed");

    let status_audio = Command::new("ffmpeg")
        .args([
            "-y",
            "-f",
            "lavfi",
            "-i",
            "sine=frequency=880:sample_rate=48000:duration=3",
            "-c:a",
            "mp2",
            &audio.to_string_lossy(),
        ])
        .status()
        .expect("run ffmpeg audio generation");
    assert!(status_audio.success(), "ffmpeg audio generation failed");

    let candidates = ["hdmv_pgs_subtitle", "dvdsub", "dvb_subtitle"];
    for codec in candidates {
        let status = Command::new("ffmpeg")
            .args([
                "-y",
                "-i",
                &video.to_string_lossy(),
                "-i",
                &audio.to_string_lossy(),
                "-i",
                &srt_eng.to_string_lossy(),
                "-i",
                &srt_spa.to_string_lossy(),
                "-i",
                &srt_fra.to_string_lossy(),
                "-i",
                &srt_kor.to_string_lossy(),
                "-i",
                &srt_jpn.to_string_lossy(),
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
                "-metadata:s:s:2",
                "language=fra",
                "-metadata:s:s:3",
                "language=kor",
                "-metadata:s:s:4",
                "language=jpn",
                "-map",
                "0:v:0",
                "-map",
                "1:a:0",
                "-map",
                "2:0",
                "-map",
                "3:0",
                "-map",
                "4:0",
                "-map",
                "5:0",
                "-map",
                "6:0",
                &input.to_string_lossy(),
            ])
            .status()
            .expect("run ffmpeg mux for multilingual bitmap subs");
        if status.success() {
            return Some(input);
        }
    }

    None
}

fn count_bitmap_subtitle_streams(input: &Path) -> usize {
    let cstr = std::ffi::CString::new(input.to_string_lossy().to_string()).unwrap();
    let ictx = AVFormatContextInput::open(cstr.as_c_str()).expect("open generated input");
    ictx.streams()
        .iter()
        .filter(|stream| {
            let cp = stream.codecpar();
            cp.codec_type == ffi::AVMEDIA_TYPE_SUBTITLE
                && matches!(
                    cp.codec_id,
                    ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE
                        | ffi::AV_CODEC_ID_DVD_SUBTITLE
                        | ffi::AV_CODEC_ID_DVB_SUBTITLE
                        | ffi::AV_CODEC_ID_XSUB
                )
        })
        .count()
}

fn extract_srt_text_lines(srt_path: &Path) -> Vec<String> {
    let raw = fs::read_to_string(srt_path).unwrap_or_default();
    raw.lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .filter(|line| !line.chars().all(|ch| ch.is_ascii_digit()))
        .filter(|line| !line.contains("-->"))
        .map(|line| line.to_string())
        .collect()
}

fn best_similarity(lines: &[String], expected: &str) -> f64 {
    let expected_norm = expected.to_lowercase();
    lines
        .iter()
        .map(|line| normalized_levenshtein(&line.to_lowercase(), &expected_norm))
        .fold(0.0, f64::max)
}

#[test]
fn cli_multilang_generator_creates_bitmap_streams() -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let Some(input) = generate_multilang_bitmap_input(&tmp) else {
        eprintln!(
            "No bitmap subtitle encoder available; skipping multilingual bitmap generator test."
        );
        return Ok(());
    };
    let bitmap_count = count_bitmap_subtitle_streams(&input);
    assert!(
        bitmap_count >= 5,
        "expected >=5 bitmap subtitle streams, got {}",
        bitmap_count
    );
    Ok(())
}

#[test]
#[ignore]
fn cli_multilang_ocr_accuracy_and_performance() -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();
    if std::env::var("DPN_OCR_MULTILANG_STRESS").ok().as_deref() != Some("1") {
        eprintln!(
            "Skipping multilingual OCR stress test (set DPN_OCR_MULTILANG_STRESS=1 to enable)."
        );
        return Ok(());
    }

    let tmp = TempDir::new()?;
    let Some(input) = generate_multilang_bitmap_input(&tmp) else {
        eprintln!("No bitmap subtitle encoder available; skipping multilingual OCR stress test.");
        return Ok(());
    };

    let output = tmp.path().join("out_multilang.mp4");
    let start = Instant::now();

    let cmd_out = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
        .arg("--sub-mode")
        .arg("force")
        .arg("--ocr-engine")
        .arg("pp-ocr-v3")
        .arg("--ocr-format")
        .arg("srt")
        .arg("--ocr-write-srt-sidecar")
        .arg("--skip-codec-check")
        .arg(&input)
        .arg(&output)
        .env("RUST_LOG", "info")
        .output()?;

    let elapsed = start.elapsed().as_secs_f64();
    if !cmd_out.status.success() {
        let stderr = String::from_utf8_lossy(&cmd_out.stderr);
        if stderr.contains("PP-OCRv3 failed to initialize")
            || stderr.contains("ONNX Runtime")
            || stderr.contains("No GPU execution providers available")
        {
            eprintln!(
                "Skipping multilingual OCR stress test due to local ORT/GPU runtime setup: {}",
                stderr
            );
            return Ok(());
        }
        panic!(
            "direct_play_nice failed: status={} stderr={}",
            cmd_out.status, stderr
        );
    }

    let max_sec = std::env::var("DPN_OCR_MULTILANG_MAX_SEC")
        .ok()
        .and_then(|v| v.parse::<f64>().ok())
        .unwrap_or(180.0);
    assert!(
        elapsed <= max_sec,
        "OCR runtime exceeded budget: {:.2}s > {:.2}s",
        elapsed,
        max_sec
    );

    let logs = format!(
        "{}\n{}",
        String::from_utf8_lossy(&cmd_out.stdout),
        String::from_utf8_lossy(&cmd_out.stderr)
    );
    assert!(
        logs.contains("OCR progress:"),
        "expected OCR progress logging in command output"
    );

    let checks = [
        ("eng", "we should leave now", 0.45),
        ("spa", "debemos irnos ahora", 0.40),
        ("fra", "nous devons partir", 0.40),
        ("kor", "지금 떠나야 해", 0.15),
        ("jpn", "今出発しよう", 0.15),
    ];
    let stem = output
        .file_stem()
        .and_then(|x| x.to_str())
        .expect("output stem");
    let parent = output.parent().expect("output parent");
    for (lang, expected, min_similarity) in checks {
        let sidecar = parent.join(format!("{stem}.{lang}.srt"));
        assert!(
            sidecar.exists(),
            "expected sidecar for language '{}' at {}",
            lang,
            sidecar.display()
        );
        let lines = extract_srt_text_lines(&sidecar);
        assert!(
            !lines.is_empty(),
            "sidecar '{}' had no cues",
            sidecar.display()
        );
        let similarity = best_similarity(&lines, expected);
        assert!(
            similarity >= min_similarity,
            "low OCR similarity for '{}': got {:.3}, expected at least {:.3} ({})",
            lang,
            similarity,
            min_similarity,
            sidecar.display()
        );
    }

    Ok(())
}
