#![cfg(feature = "ffmpeg-cli-tests")]

use serde_json::Value;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

fn run(command: &mut Command) -> Vec<u8> {
    let output = command.output().expect("run command");
    assert!(
        output.status.success(),
        "{command:?}: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    output.stdout
}

fn fixture() -> (TempDir, PathBuf) {
    fixture_with_timing("0.2", "0.6")
}

fn fixture_with_timing(video_offset: &str, audio_offset: &str) -> (TempDir, PathBuf) {
    let dir = TempDir::new().unwrap();
    let ass = dir.path().join("input.ass");
    let font = dir.path().join("font.ttf");
    let input = dir.path().join("input.mkv");
    // Use an actual font where available. The fallback still exercises binary attachment copying.
    let bytes = std::fs::read("/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf")
        .unwrap_or_else(|_| b"synthetic font attachment\0\x01".to_vec());
    std::fs::write(&font, bytes).unwrap();
    std::fs::write(&ass, "[Script Info]\nScriptType: v4.00+\nPlayResX: 640\nPlayResY: 360\n\n[V4+ Styles]\nFormat: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding\nStyle: Default,DejaVu Sans,24,&H00FFFFFF,&H000000FF,&H00000000,&H00000000,0,0,0,0,100,100,0,0,1,1,0,2,10,10,10,1\n\n[Events]\nFormat: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text\nDialogue: 0,0:00:01.00,0:00:04.00,Default,,0,0,0,,Hello there\nDialogue: 1,0:00:01.00,0:00:02.00,Default,,0,0,0,,{\\pos(100,100)}FIRST SIGN\nDialogue: 1,0:00:01.01,0:00:03.00,Default,,0,0,0,,{\\pos(500,100)}SECOND SIGN\n").unwrap();
    run(Command::new("ffmpeg")
        .args([
            "-hide_banner",
            "-loglevel",
            "error",
            "-y",
            "-f",
            "lavfi",
            "-itsoffset",
            video_offset,
            "-i",
            "testsrc=size=160x120:rate=10:duration=5",
            "-f",
            "lavfi",
            "-itsoffset",
            audio_offset,
            "-i",
            "sine=duration=5",
            "-i",
        ])
        .arg(&ass)
        .args([
            "-map",
            "0:v",
            "-map",
            "1:a",
            "-map",
            "2:s",
            "-c:v",
            "mpeg4",
            "-c:a",
            "aac",
            "-c:s",
            "ass",
            "-metadata:s:s:0",
            "language=eng",
            "-metadata:s:s:0",
            "title=English Signs and Dialogue",
            "-disposition:s:0",
            "default+forced",
            "-attach",
        ])
        .arg(&font)
        .args([
            "-metadata:s:t",
            "mimetype=application/x-truetype-font",
            "-avoid_negative_ts",
            "disabled",
        ])
        .arg(&input));
    (dir, input)
}

fn probe(path: &Path) -> Value {
    let output = run(Command::new("ffprobe")
        .args([
            "-v",
            "error",
            "-show_streams",
            "-show_packets",
            "-show_chapters",
            "-show_format",
            "-show_data_hash",
            "sha256",
            "-of",
            "json",
        ])
        .arg(path));
    serde_json::from_slice(&output).unwrap()
}

fn stream<'a>(probe: &'a Value, kind: &str) -> &'a Value {
    probe["streams"]
        .as_array()
        .unwrap()
        .iter()
        .find(|s| s["codec_type"] == kind)
        .unwrap()
}

fn packets(probe: &Value, index: &Value) -> Vec<(Value, Value, Value)> {
    probe["packets"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|p| &p["stream_index"] == index)
        .map(|p| {
            (
                p["pts_time"].clone(),
                p["duration_time"].clone(),
                p["data_hash"].clone(),
            )
        })
        .collect()
}

fn audible_start(path: &Path) -> f64 {
    let bytes = run(Command::new("ffmpeg")
        .args(["-hide_banner", "-loglevel", "error", "-copyts", "-i"])
        .arg(path)
        .args([
            "-map",
            "0:a:0",
            "-af",
            "aresample=async=1:first_pts=0",
            "-ar",
            "44100",
            "-ac",
            "1",
            "-f",
            "f32le",
            "-",
        ]));
    let index = bytes
        .as_chunks::<4>()
        .0
        .iter()
        .position(|bytes| f32::from_le_bytes(*bytes).abs() > 0.01)
        .expect("audible audio");
    index as f64 / 44_100.0
}

fn synthetic_pgs() -> Vec<u8> {
    fn segment(out: &mut Vec<u8>, ms: u32, kind: u8, payload: &[u8]) {
        out.extend_from_slice(b"PG");
        out.extend_from_slice(&(ms * 90).to_be_bytes());
        out.extend_from_slice(&(ms * 90).to_be_bytes());
        out.push(kind);
        out.extend_from_slice(&(payload.len() as u16).to_be_bytes());
        out.extend_from_slice(payload);
    }
    let mut out = Vec::new();
    for (number, (ms, visible)) in [(500, true), (1500, false)].into_iter().enumerate() {
        let mut pcs = vec![0, 160, 0, 120, 0x10];
        pcs.extend_from_slice(&(number as u16).to_be_bytes());
        pcs.extend_from_slice(&[0x80, 0, 0, u8::from(visible)]);
        if visible {
            pcs.extend_from_slice(&[0, 0, 0, 0, 0, 48, 0, 80]);
        }
        segment(&mut out, ms, 0x16, &pcs);
        if visible {
            segment(&mut out, ms, 0x17, &[1, 0, 0, 48, 0, 80, 0, 32, 0, 16]);
            segment(
                &mut out,
                ms,
                0x14,
                &[0, 0, 0, 16, 128, 128, 0, 1, 235, 128, 128, 255],
            );
            let mut rle = Vec::new();
            for _ in 0..16 {
                rle.extend_from_slice(&[0, 0x80 | 32, 1, 0, 0]);
            }
            let len = (rle.len() + 4) as u32;
            let mut object = vec![0, 0, 0, 0xc0];
            object.extend_from_slice(&len.to_be_bytes()[1..]);
            object.extend_from_slice(&[0, 32, 0, 16]);
            object.extend_from_slice(&rle);
            segment(&mut out, ms, 0x15, &object);
        }
        segment(&mut out, ms, 0x80, &[]);
    }
    out
}

fn add_bitmap_streams(input: &Path, sup: &Path, output: &Path, languages: &[&str]) {
    let mut command = Command::new("ffmpeg");
    command
        .args(["-hide_banner", "-loglevel", "error", "-y", "-copyts", "-i"])
        .arg(input);
    for _ in languages {
        command.arg("-i").arg(sup);
    }
    command.args(["-map", "0"]);
    for i in 0..languages.len() {
        command.args(["-map", &format!("{}:s", i + 1)]);
    }
    command.args(["-c", "copy"]);
    for (i, language) in languages.iter().enumerate() {
        command.args([
            &format!("-metadata:s:s:{}", i + 1),
            &format!("language={language}"),
        ]);
    }
    command.args(["-avoid_negative_ts", "disabled"]).arg(output);
    run(&mut command);
}

fn sonarr_command(dir: &TempDir, input: &Path, suffix: &str) -> Command {
    let config = dir.path().join("sonarr-test.toml");
    std::fs::write(&config, "").unwrap();
    let mut command = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    for (key, _) in std::env::vars_os() {
        let name = key.to_string_lossy().to_ascii_lowercase();
        if name.starts_with("sonarr_")
            || name.starts_with("radarr_")
            || name.starts_with("direct_play_nice_")
            || name == "plex_url"
            || name == "plex_token"
        {
            command.env_remove(key);
        }
    }
    command
        .env("HOME", dir.path())
        .env("XDG_CONFIG_HOME", dir.path())
        .env("XDG_CACHE_HOME", dir.path())
        .env("DIRECT_PLAY_NICE_LOCK_DIR", dir.path().join("locks"))
        .env("sonarr_eventtype", "Download")
        .env("sonarr_episodefile_path", input)
        .env("sonarr_series_title", "Synthetic")
        .arg("--config-file")
        .arg(config)
        .args([
            "--hw-accel",
            "none",
            "--device",
            "fire_tv_stick_4k",
            "--servarr-output-extension",
            "mkv",
            "--servarr-output-suffix",
            suffix,
        ]);
    command
}

fn assert_no_private_staging(directory: &Path) {
    let leftovers: Vec<_> = std::fs::read_dir(directory)
        .unwrap()
        .map(|item| item.unwrap().path())
        .filter(|path| {
            path.file_name()
                .unwrap()
                .to_string_lossy()
                .starts_with(".direct-play-nice-")
        })
        .collect();
    assert!(leftovers.is_empty(), "staging leftovers: {leftovers:?}");
}

#[test]
fn matroska_ocr_remux_preserves_forced_ass_and_attached_font() {
    let (dir, input) = fixture();
    let sup = dir.path().join("bitmap.sup");
    let combined = dir.path().join("mixed.mkv");
    let output = dir.path().join("mixed-out.mkv");
    std::fs::write(&sup, synthetic_pgs()).unwrap();
    run(Command::new("ffmpeg")
        .args(["-hide_banner", "-loglevel", "error", "-y", "-copyts", "-i"])
        .arg(&input)
        .arg("-i")
        .arg(&sup)
        .args([
            "-map",
            "0",
            "-map",
            "1:s",
            "-c",
            "copy",
            "-metadata:s:s:1",
            "language=spa",
        ])
        .arg(&combined));
    let original = std::fs::read(&combined).unwrap();
    let mixed_probe = probe(&combined);
    assert_eq!(stream(&mixed_probe, "subtitle")["disposition"]["forced"], 1);
    let result = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
        .args([
            "--hw-accel",
            "none",
            "--delete-source",
            "false",
            "--device",
            "fire_tv_stick_4k",
            "--ocr-format",
            "ass",
            "--ocr-engine",
            "external",
            "--ocr-external-command",
            "echo OCR TEXT",
        ])
        .arg(&combined)
        .arg(&output)
        .output()
        .unwrap();
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    let converted = probe(&output);
    let before = stream(&mixed_probe, "subtitle");
    let after = stream(&converted, "subtitle");
    assert_eq!(before["disposition"], after["disposition"]);
    assert_eq!(before["tags"]["language"], after["tags"]["language"]);
    assert_eq!(before["tags"]["title"], after["tags"]["title"]);
    assert_eq!(before["extradata_hash"], after["extradata_hash"]);
    assert_eq!(
        packets(&mixed_probe, &before["index"]),
        packets(&converted, &after["index"])
    );
    let font_before = stream(&mixed_probe, "attachment");
    let font_after = stream(&converted, "attachment");
    assert_eq!(font_before["extradata_hash"], font_after["extradata_hash"]);
    assert_eq!(
        font_before["tags"]["filename"],
        font_after["tags"]["filename"]
    );
    assert_eq!(
        font_before["tags"]["mimetype"],
        font_after["tags"]["mimetype"]
    );
    let subtitles: Vec<_> = converted["streams"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|s| s["codec_type"] == "subtitle")
        .collect();
    assert_eq!(subtitles.len(), 3);
    let ocr = subtitles
        .iter()
        .find(|s| s["codec_name"] == "ass" && s["tags"]["language"] == "spa")
        .unwrap();
    let copied_pgs = subtitles
        .iter()
        .find(|s| s["codec_name"] == "hdmv_pgs_subtitle")
        .unwrap();
    let ocr_packets = packets(&converted, &ocr["index"]);
    assert_eq!(ocr_packets.len(), 1);
    let start: f64 = ocr_packets[0].0.as_str().unwrap().parse().unwrap();
    let duration: f64 = ocr_packets[0].1.as_str().unwrap().parse().unwrap();
    let source_pgs = mixed_probe["streams"]
        .as_array()
        .unwrap()
        .iter()
        .find(|s| s["codec_name"] == "hdmv_pgs_subtitle")
        .unwrap();
    let source_packets = packets(&mixed_probe, &source_pgs["index"]);
    assert_eq!(packets(&converted, &copied_pgs["index"]), source_packets);
    let source_start: f64 = source_packets[0].0.as_str().unwrap().parse().unwrap();
    let source_end: f64 = source_packets[1].0.as_str().unwrap().parse().unwrap();
    assert!(
        source_start >= 0.4,
        "fixture did not retain its non-zero subtitle start"
    );
    assert!(
        (start - source_start).abs() < 0.02,
        "OCR start changed: {source_start} -> {start}"
    );
    assert!(
        (duration - (source_end - source_start)).abs() < 0.02,
        "OCR duration changed: {duration}"
    );
    let a = audible_start(&combined);
    let b = audible_start(&output);
    assert!((a - b).abs() < 0.015, "OCR remux shifted audio: {a} -> {b}");
    assert_eq!(std::fs::read(combined).unwrap(), original);
}

#[test]
fn matroska_ocr_remux_keeps_ass_packets_and_audio_when_flags_are_unset() {
    let (dir, input) = fixture();
    let sup = dir.path().join("bitmap.sup");
    let combined = dir.path().join("mixed.mkv");
    let output = dir.path().join("mixed-out.mkv");
    std::fs::write(&sup, synthetic_pgs()).unwrap();
    run(Command::new("ffmpeg")
        .args(["-hide_banner", "-loglevel", "error", "-y", "-copyts", "-i"])
        .arg(&input)
        .arg("-i")
        .arg(&sup)
        .args([
            "-map",
            "0",
            "-map",
            "1:s",
            "-c",
            "copy",
            "-metadata:s:s:1",
            "language=spa",
            "-disposition:s:0",
            "0",
        ])
        .arg(&combined));
    let source = probe(&combined);
    run(
        Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
            .args([
                "--hw-accel",
                "none",
                "--delete-source",
                "false",
                "--device",
                "fire_tv_stick_4k",
                "--ocr-format",
                "ass",
                "--ocr-engine",
                "external",
                "--ocr-external-command",
                "echo OCR TEXT",
            ])
            .arg(&combined)
            .arg(&output),
    );
    let converted = probe(&output);
    let subtitle_streams: Vec<_> = converted["streams"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|item| item["codec_type"] == "subtitle")
        .collect();
    assert_eq!(
        subtitle_streams.len(),
        3,
        "original bitmap or OCR track was lost"
    );
    let ocr = subtitle_streams
        .iter()
        .find(|s| s["codec_name"] == "ass" && s["tags"]["language"] == "spa")
        .unwrap();
    assert!(
        !packets(&converted, &ocr["index"]).is_empty(),
        "OCR track is empty"
    );
    let before = stream(&source, "subtitle");
    let after = stream(&converted, "subtitle");
    assert_eq!(
        packets(&source, &before["index"]),
        packets(&converted, &after["index"])
    );
    assert_eq!(before["disposition"], after["disposition"]);
    assert_eq!(
        stream(&source, "attachment")["extradata_hash"],
        stream(&converted, "attachment")["extradata_hash"]
    );
    let a = audible_start(&combined);
    let b = audible_start(&output);
    assert!((a - b).abs() < 0.015, "OCR remux shifted audio: {a} -> {b}");
}

#[test]
fn matroska_is_not_silently_selected_for_an_mp4_only_device() {
    let (dir, input) = fixture();
    let output = dir.path().join("output.mkv");
    let original = std::fs::read(&input).unwrap();
    let result = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
        .args([
            "--hw-accel",
            "none",
            "--delete-source",
            "true",
            "--device",
            "chromecast_1st_gen",
        ])
        .arg(&input)
        .arg(&output)
        .output()
        .unwrap();
    assert!(!result.status.success());
    assert!(String::from_utf8_lossy(&result.stderr)
        .contains("not compatible with all selected devices"));
    assert_eq!(std::fs::read(input).unwrap(), original);
    assert!(!output.exists());
}

#[test]
fn private_staging_never_overwrites_aliases_or_existing_files() {
    let (dir, input) = fixture();
    let original = std::fs::read(&input).unwrap();
    let output = dir.path().join("target.mkv");
    let old_conv = dir.path().join("target.direct-play-nice.conv.mkv");
    std::fs::copy(&input, &old_conv).unwrap();
    let copied = std::fs::read(&old_conv).unwrap();
    run(
        Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
            .args([
                "--hw-accel",
                "none",
                "--delete-source",
                "false",
                "--device",
                "fire_tv_stick_4k",
                "--ocr-format",
                "ass",
            ])
            .arg(&old_conv)
            .arg(&output),
    );
    assert_eq!(std::fs::read(&old_conv).unwrap(), copied);
    assert_eq!(std::fs::read(&input).unwrap(), original);
    assert_no_private_staging(dir.path());
    let old_tmp = dir.path().join("tmp-target.direct-play-nice.tmp.mkv");
    let tmp_target = dir.path().join("tmp-target.mkv");
    std::fs::copy(&input, &old_tmp).unwrap();
    let original_tmp = std::fs::read(&old_tmp).unwrap();
    run(
        Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
            .args([
                "--hw-accel",
                "none",
                "--delete-source",
                "false",
                "--device",
                "fire_tv_stick_4k",
                "--ocr-format",
                "ass",
            ])
            .arg(&old_tmp)
            .arg(&tmp_target),
    );
    assert_eq!(std::fs::read(old_tmp).unwrap(), original_tmp);
    assert_no_private_staging(dir.path());

    let next = dir.path().join("other.mkv");
    let stale_conv = dir.path().join("other.direct-play-nice.conv.mkv");
    std::fs::write(&stale_conv, b"unrelated existing file").unwrap();
    run(
        Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
            .args([
                "--hw-accel",
                "none",
                "--delete-source",
                "false",
                "--device",
                "fire_tv_stick_4k",
                "--ocr-format",
                "ass",
            ])
            .arg(&input)
            .arg(&next),
    );
    assert_eq!(
        std::fs::read(stale_conv).unwrap(),
        b"unrelated existing file"
    );
    assert_no_private_staging(dir.path());

    let existing = dir.path().join("existing.mkv");
    std::fs::write(&existing, b"destination survives").unwrap();
    let failed = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
        .args([
            "--hw-accel",
            "none",
            "--delete-source",
            "true",
            "--device",
            "fire_tv_stick_4k",
            "--ocr-format",
            "ass",
        ])
        .arg(&input)
        .arg(&existing)
        .output()
        .unwrap();
    assert!(!failed.status.success());
    assert_eq!(std::fs::read(existing).unwrap(), b"destination survives");
    assert_eq!(std::fs::read(&input).unwrap(), original);
    let failed = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
        .args([
            "--hw-accel",
            "none",
            "--delete-source",
            "true",
            "--device",
            "fire_tv_stick_4k",
            "--ocr-format",
            "ass",
        ])
        .arg(&input)
        .arg(&input)
        .output()
        .unwrap();
    assert!(!failed.status.success());
    assert_eq!(std::fs::read(&input).unwrap(), original);
    let hard_alias = dir.path().join("hardlink.mkv");
    std::fs::hard_link(&input, &hard_alias).unwrap();
    let failed = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
        .args([
            "--hw-accel",
            "none",
            "--delete-source",
            "true",
            "--device",
            "fire_tv_stick_4k",
            "--ocr-format",
            "ass",
        ])
        .arg(&input)
        .arg(&hard_alias)
        .output()
        .unwrap();
    assert!(!failed.status.success());
    assert_eq!(std::fs::read(&hard_alias).unwrap(), original);
    assert_eq!(std::fs::read(&input).unwrap(), original);
    assert_no_private_staging(dir.path());
}

#[test]
fn failed_ocr_cleans_private_staging_without_touching_source() {
    let (dir, input) = fixture();
    let sup = dir.path().join("bitmap.sup");
    let mixed = dir.path().join("mixed.mkv");
    let output = dir.path().join("fail.mkv");
    let stale = dir.path().join("fail.direct-play-nice.conv.mkv");
    std::fs::write(&sup, synthetic_pgs()).unwrap();
    std::fs::write(&stale, b"unrelated file").unwrap();
    add_bitmap_streams(&input, &sup, &mixed, &["spa"]);
    let original = std::fs::read(&mixed).unwrap();
    let result = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
        .args([
            "--hw-accel",
            "none",
            "--delete-source",
            "true",
            "--device",
            "fire_tv_stick_4k",
            "--ocr-format",
            "ass",
            "--ocr-engine",
            "external",
            "--ocr-external-command",
            "/not-a-real-ocr-command",
            "--subtitle-failure-policy",
            "fail",
        ])
        .arg(&mixed)
        .arg(&output)
        .output()
        .unwrap();
    assert!(
        !result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    assert!(!output.exists());
    assert_eq!(std::fs::read(mixed).unwrap(), original);
    assert_eq!(std::fs::read(stale).unwrap(), b"unrelated file");
    assert_no_private_staging(dir.path());
}

#[test]
fn ocr_language_filter_keeps_excluded_bitmap_tracks() {
    let (dir, input) = fixture();
    let sup = dir.path().join("bitmap.sup");
    let mixed = dir.path().join("mixed.mkv");
    let output = dir.path().join("filtered.mkv");
    std::fs::write(&sup, synthetic_pgs()).unwrap();
    add_bitmap_streams(&input, &sup, &mixed, &["eng", "jpn"]);
    let before = probe(&mixed);
    run(
        Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
            .args([
                "--hw-accel",
                "none",
                "--delete-source",
                "true",
                "--device",
                "fire_tv_stick_4k",
                "--ocr-format",
                "ass",
                "--ocr-engine",
                "external",
                "--ocr-external-command",
                "echo OCR TEXT",
                "--ocr-languages",
                "eng",
            ])
            .arg(&mixed)
            .arg(&output),
    );
    let after = probe(&output);
    let text_streams = after["streams"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|s| s["codec_name"] == "ass" && s["tags"]["language"] == "eng")
        .count();
    assert_eq!(
        text_streams, 2,
        "expected original ASS and English OCR track"
    );
    assert!(!after["streams"]
        .as_array()
        .unwrap()
        .iter()
        .any(|s| s["codec_name"] == "ass" && s["tags"]["language"] == "jpn"));
    for language in ["eng", "jpn"] {
        let select = |probe: &Value| {
            probe["streams"]
                .as_array()
                .unwrap()
                .iter()
                .find(|s| {
                    s["codec_name"] == "hdmv_pgs_subtitle" && s["tags"]["language"] == language
                })
                .unwrap()["index"]
                .clone()
        };
        assert_eq!(
            packets(&before, &select(&before)),
            packets(&after, &select(&after)),
            "bitmap {language} changed"
        );
    }
    assert!(
        !mixed.exists(),
        "source deletion should only happen after successful preservation"
    );
    assert_no_private_staging(dir.path());
}

#[test]
fn sonarr_matroska_preserves_unselected_bitmap_and_old_staging_files() {
    let (dir, input) = fixture();
    let sup = dir.path().join("bitmap.sup");
    let mixed = dir.path().join("mixed.mkv");
    std::fs::write(&sup, synthetic_pgs()).unwrap();
    add_bitmap_streams(&input, &sup, &mixed, &["eng", "jpn"]);
    let before = probe(&mixed);
    let output = dir.path().join("mixed.fixed.mkv");
    let old_conv = dir.path().join("mixed.fixed.direct-play-nice.conv.mkv");
    let old_backup = dir.path().join("mixed.direct-play-nice.bak.mkv");
    std::fs::write(&old_conv, b"unrelated intermediate").unwrap();
    std::fs::write(&old_backup, b"unrelated backup").unwrap();
    let config = dir.path().join("config.toml");
    std::fs::write(&config, "").unwrap();
    let mut command = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    for (key, _) in std::env::vars_os() {
        let name = key.to_string_lossy().to_ascii_lowercase();
        if name.starts_with("sonarr_")
            || name.starts_with("radarr_")
            || name.starts_with("direct_play_nice_")
            || name == "plex_url"
            || name == "plex_token"
        {
            command.env_remove(key);
        }
    }
    command
        .env("HOME", dir.path())
        .env("XDG_CONFIG_HOME", dir.path())
        .env("XDG_CACHE_HOME", dir.path())
        .env("DIRECT_PLAY_NICE_LOCK_DIR", dir.path().join("locks"))
        .env("sonarr_eventtype", "Download")
        .env("sonarr_episodefile_path", &mixed)
        .env("sonarr_series_title", "Synthetic")
        .arg("--config-file")
        .arg(&config)
        .args([
            "--hw-accel",
            "none",
            "--device",
            "fire_tv_stick_4k",
            "--servarr-output-extension",
            "mkv",
            "--servarr-output-suffix",
            ".fixed",
            "--ocr-format",
            "ass",
            "--ocr-engine",
            "external",
            "--ocr-external-command",
            "echo OCR TEXT",
            "--ocr-languages",
            "eng",
        ]);
    run(&mut command);
    let after = probe(&output);
    let find_jpn = |data: &Value| {
        data["streams"]
            .as_array()
            .unwrap()
            .iter()
            .find(|s| s["codec_name"] == "hdmv_pgs_subtitle" && s["tags"]["language"] == "jpn")
            .unwrap()["index"]
            .clone()
    };
    assert_eq!(
        packets(&before, &find_jpn(&before)),
        packets(&after, &find_jpn(&after))
    );
    assert!(!mixed.exists());
    assert_eq!(std::fs::read(old_conv).unwrap(), b"unrelated intermediate");
    assert_eq!(std::fs::read(old_backup).unwrap(), b"unrelated backup");
    assert_no_private_staging(dir.path());
}

#[test]
fn compatible_direct_input_with_in_place_or_existing_destination_is_unchanged() {
    let dir = TempDir::new().unwrap();
    let input = dir.path().join("compatible.mp4");
    run(Command::new("ffmpeg")
        .args([
            "-hide_banner",
            "-loglevel",
            "error",
            "-y",
            "-f",
            "lavfi",
            "-i",
            "testsrc=size=160x120:rate=25:duration=2",
            "-f",
            "lavfi",
            "-i",
            "sine=duration=2",
            "-pix_fmt",
            "yuv420p",
            "-c:v",
            "libx264",
            "-profile:v",
            "high",
            "-level:v",
            "4.1",
            "-b:v",
            "4M",
            "-c:a",
            "aac",
            "-b:a",
            "128k",
            "-movflags",
            "+faststart",
        ])
        .arg(&input));
    let source = std::fs::read(&input).unwrap();
    let output = dir.path().join("existing.mp4");
    let missing_parent = dir.path().join("missing-parent").join("output.mp4");
    std::fs::write(&output, b"unrelated destination").unwrap();
    for destination in [&input, &output, &missing_parent] {
        let result = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
            .env("HOME", dir.path())
            .env("XDG_CONFIG_HOME", dir.path())
            .env_remove("DIRECT_PLAY_NICE_CONFIG")
            .env_remove("DIRECT_PLAY_NICE_CONFIG_FILE")
            .args([
                "--hw-accel",
                "none",
                "--delete-source",
                "true",
                "--device",
                "chromecast_1st_gen",
            ])
            .arg(&input)
            .arg(destination)
            .output()
            .unwrap();
        assert!(
            result.status.success(),
            "{}",
            String::from_utf8_lossy(&result.stderr)
        );
        assert!(String::from_utf8_lossy(&result.stderr).contains("Input is direct-play compatible"));
        assert_eq!(std::fs::read(&input).unwrap(), source);
        assert_eq!(std::fs::read(&output).unwrap(), b"unrelated destination");
        assert!(!missing_parent.exists());
        assert_no_private_staging(dir.path());
    }
    let fixed = dir.path().join("compatible.fixed.mp4");
    std::fs::write(&fixed, b"existing Servarr destination").unwrap();
    let config = dir.path().join("servarr-compatible.toml");
    std::fs::write(&config, "").unwrap();
    for (kind, suffix) in [("sonarr", ".fixed"), ("radarr", "")] {
        let mut command = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
        for (key, _) in std::env::vars_os() {
            let name = key.to_string_lossy().to_ascii_lowercase();
            if name.starts_with("sonarr_")
                || name.starts_with("radarr_")
                || name.starts_with("direct_play_nice_")
                || name == "plex_url"
                || name == "plex_token"
            {
                command.env_remove(key);
            }
        }
        command
            .env("HOME", dir.path())
            .env("XDG_CONFIG_HOME", dir.path())
            .env("XDG_CACHE_HOME", dir.path())
            .env("DIRECT_PLAY_NICE_LOCK_DIR", dir.path().join("locks"))
            .env(format!("{kind}_eventtype"), "Download")
            .env(
                if kind == "sonarr" {
                    "sonarr_episodefile_path"
                } else {
                    "radarr_moviefile_path"
                },
                &input,
            )
            .env(
                if kind == "sonarr" {
                    "sonarr_series_title"
                } else {
                    "radarr_movie_title"
                },
                "Synthetic",
            )
            .arg("--config-file")
            .arg(&config)
            .args([
                "--hw-accel",
                "none",
                "--device",
                "chromecast_1st_gen",
                "--servarr-output-extension",
                "mp4",
                "--servarr-output-suffix",
                suffix,
            ]);
        let result = command.output().unwrap();
        assert!(
            result.status.success(),
            "{}",
            String::from_utf8_lossy(&result.stderr)
        );
        assert!(String::from_utf8_lossy(&result.stderr).contains("Input is direct-play compatible"));
        assert_eq!(std::fs::read(&input).unwrap(), source);
        assert_eq!(
            std::fs::read(&fixed).unwrap(),
            b"existing Servarr destination"
        );
        assert_no_private_staging(dir.path());
    }
}

#[test]
fn radarr_in_place_replacement_keeps_subtitles_and_external_hardlink() {
    let (dir, input) = fixture();
    let original = std::fs::read(&input).unwrap();
    let before = probe(&input);
    let link = dir.path().join("source-hardlink.mkv");
    std::fs::hard_link(&input, &link).unwrap();
    let mut command = sonarr_command(&dir, &input, "");
    command
        .env_remove("sonarr_eventtype")
        .env_remove("sonarr_episodefile_path")
        .env_remove("sonarr_series_title")
        .env("radarr_eventtype", "Download")
        .env("radarr_moviefile_path", &input)
        .env("radarr_movie_title", "Synthetic");
    run(&mut command);
    let after = probe(&input);
    assert_eq!(stream(&after, "video")["codec_name"], "h264");
    assert_ne!(std::fs::read(&input).unwrap(), original);
    assert_eq!(std::fs::read(link).unwrap(), original);
    let old_sub = stream(&before, "subtitle");
    let new_sub = stream(&after, "subtitle");
    assert_eq!(
        packets(&before, &old_sub["index"]),
        packets(&after, &new_sub["index"])
    );
    assert_eq!(old_sub["disposition"], new_sub["disposition"]);
    assert_eq!(
        stream(&before, "attachment")["extradata_hash"],
        stream(&after, "attachment")["extradata_hash"]
    );
    assert_no_private_staging(dir.path());
}

#[cfg(unix)]
#[test]
fn sonarr_existing_regular_destination_is_not_overwritten_and_source_is_restored() {
    let (dir, input) = fixture();
    let original = std::fs::read(&input).unwrap();
    let destination = dir.path().join("input.fixed.mkv");
    std::fs::write(&destination, b"unrelated destination").unwrap();
    let result = sonarr_command(&dir, &input, ".fixed").output().unwrap();
    assert!(!result.status.success());
    assert!(
        String::from_utf8_lossy(&result.stderr).contains("could not promote"),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    assert_eq!(std::fs::read(&input).unwrap(), original);
    assert_eq!(
        std::fs::read(destination).unwrap(),
        b"unrelated destination"
    );
    assert_no_private_staging(dir.path());
}

#[cfg(unix)]
#[test]
fn sonarr_replaced_source_during_conversion_is_not_removed_or_published() {
    use std::process::Stdio;
    use std::time::{Duration, Instant};

    for suffix in ["", ".fixed"] {
        let (dir, input) = fixture();
        let sup = dir.path().join("bitmap.sup");
        let mixed = dir.path().join("mixed.mkv");
        std::fs::write(&sup, synthetic_pgs()).unwrap();
        add_bitmap_streams(&input, &sup, &mixed, &["eng"]);
        let ready = dir.path().join("ocr-ready");
        let release = dir.path().join("ocr-release");
        let script = dir.path().join("block-ocr.sh");
        std::fs::write(
            &script,
            "touch \"$DPN_TEST_READY\"\ni=0\nwhile [ ! -e \"$DPN_TEST_RELEASE\" ]; do\n  i=$((i+1))\n  [ \"$i\" -lt 300 ] || exit 1\n  sleep 0.1\ndone\necho OCR TEXT\n",
        )
        .unwrap();
        let mut command = sonarr_command(&dir, &mixed, suffix);
        command
            .env("DPN_TEST_READY", &ready)
            .env("DPN_TEST_RELEASE", &release)
            .args([
                "--ocr-format",
                "ass",
                "--ocr-engine",
                "external",
                "--ocr-external-command",
            ])
            .arg(format!("sh {}", script.display()))
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
        let mut child = command.spawn().unwrap();
        let deadline = Instant::now() + Duration::from_secs(30);
        while !ready.exists() && Instant::now() < deadline {
            if child.try_wait().unwrap().is_some() {
                break;
            }
            std::thread::sleep(Duration::from_millis(50));
        }
        if !ready.exists() {
            let _ = child.kill();
            let result = child.wait_with_output().unwrap();
            panic!(
                "conversion did not reach OCR: {}",
                String::from_utf8_lossy(&result.stderr)
            );
        }
        let replacement = dir.path().join("replacement.mkv");
        std::fs::write(&replacement, b"newly imported episode").unwrap();
        std::fs::rename(&replacement, &mixed).unwrap();
        std::fs::write(&release, b"continue").unwrap();
        let result = child.wait_with_output().unwrap();
        assert!(!result.status.success(), "stale conversion was published");
        assert!(
            String::from_utf8_lossy(&result.stderr).contains("changed during conversion"),
            "{}",
            String::from_utf8_lossy(&result.stderr)
        );
        assert_eq!(std::fs::read(&mixed).unwrap(), b"newly imported episode");
        if !suffix.is_empty() {
            assert!(!dir.path().join("mixed.fixed.mkv").exists());
        }
        assert_no_private_staging(dir.path());
    }
}

#[test]
fn ocr_sidecars_are_published_without_overwriting_existing_files() {
    let (dir, input) = fixture();
    let sup = dir.path().join("bitmap.sup");
    let mixed = dir.path().join("mixed.mkv");
    let bitmap_only = dir.path().join("bitmap-only.mkv");
    std::fs::write(&sup, synthetic_pgs()).unwrap();
    add_bitmap_streams(&input, &sup, &mixed, &["spa"]);
    run(Command::new("ffmpeg")
        .args(["-hide_banner", "-loglevel", "error", "-y", "-copyts", "-i"])
        .arg(&mixed)
        .args([
            "-map",
            "0:v",
            "-map",
            "0:a",
            "-map",
            "0:s:1",
            "-c",
            "copy",
            "-avoid_negative_ts",
            "disabled",
        ])
        .arg(&bitmap_only));
    let original = std::fs::read(&bitmap_only).unwrap();
    let output = dir.path().join("sidecar.mp4");
    let result = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
        .args([
            "--hw-accel",
            "none",
            "--delete-source",
            "false",
            "--device",
            "chromecast_1st_gen",
            "--ocr-engine",
            "external",
            "--ocr-external-command",
            "echo OCR TEXT",
            "--ocr-write-srt-sidecar",
            "--subtitle-failure-policy",
            "fail",
        ])
        .arg(&bitmap_only)
        .arg(&output)
        .output()
        .unwrap();
    #[cfg(unix)]
    {
        assert!(
            result.status.success(),
            "{}",
            String::from_utf8_lossy(&result.stderr)
        );
        assert!(output.exists());
        assert!(std::fs::read_to_string(dir.path().join("sidecar.spa.srt"))
            .unwrap()
            .contains("OCR TEXT"));
    }
    #[cfg(not(unix))]
    {
        assert!(!result.status.success());
        assert!(String::from_utf8_lossy(&result.stderr)
            .contains("OCR sidecars cannot be safely rolled back on this platform"));
        assert!(!output.exists());
        assert!(!dir.path().join("sidecar.spa.srt").exists());
    }
    assert_eq!(std::fs::read(&bitmap_only).unwrap(), original);
    assert_no_private_staging(dir.path());

    let blocked = dir.path().join("blocked.mp4");
    let existing = dir.path().join("blocked.spa.srt");
    std::fs::write(&existing, b"existing sidecar").unwrap();
    let result = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
        .args([
            "--hw-accel",
            "none",
            "--delete-source",
            "true",
            "--device",
            "chromecast_1st_gen",
            "--ocr-engine",
            "external",
            "--ocr-external-command",
            "echo OCR TEXT",
            "--ocr-write-srt-sidecar",
            "--subtitle-failure-policy",
            "fail",
        ])
        .arg(&bitmap_only)
        .arg(&blocked)
        .output()
        .unwrap();
    assert!(
        !result.status.success(),
        "existing sidecar must block publication"
    );
    assert!(!blocked.exists());
    assert_eq!(std::fs::read(existing).unwrap(), b"existing sidecar");
    assert_eq!(std::fs::read(&bitmap_only).unwrap(), original);
    assert_no_private_staging(dir.path());
}

#[test]
fn negative_aac_priming_keeps_ocr_and_copied_subtitles_on_source_timeline() {
    let (dir, input) = fixture_with_timing("0", "0");
    let sup = dir.path().join("bitmap.sup");
    let mixed = dir.path().join("mixed.mkv");
    let output = dir.path().join("negative.mkv");
    std::fs::write(&sup, synthetic_pgs()).unwrap();
    add_bitmap_streams(&input, &sup, &mixed, &["spa"]);
    let before = probe(&mixed);
    let audio = stream(&before, "audio");
    let min_audio = packets(&before, &audio["index"])
        .iter()
        .filter_map(|p| p.0.as_str()?.parse::<f64>().ok())
        .fold(f64::INFINITY, f64::min);
    assert!(
        min_audio < 0.0,
        "fixture did not contain a negative AAC packet: {min_audio}"
    );
    run(
        Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
            .args([
                "--hw-accel",
                "none",
                "--delete-source",
                "false",
                "--device",
                "fire_tv_stick_4k",
                "--ocr-format",
                "ass",
                "--ocr-engine",
                "external",
                "--ocr-external-command",
                "echo OCR TEXT",
            ])
            .arg(&mixed)
            .arg(&output),
    );
    let after = probe(&output);
    let source_ass = stream(&before, "subtitle");
    let output_ass = stream(&after, "subtitle");
    assert_eq!(
        packets(&before, &source_ass["index"]),
        packets(&after, &output_ass["index"])
    );
    let pgs = before["streams"]
        .as_array()
        .unwrap()
        .iter()
        .find(|s| s["codec_name"] == "hdmv_pgs_subtitle")
        .unwrap();
    let ocr = after["streams"]
        .as_array()
        .unwrap()
        .iter()
        .find(|s| s["codec_name"] == "ass" && s["tags"]["language"] == "spa")
        .unwrap();
    let start: f64 = packets(&before, &pgs["index"])[0]
        .0
        .as_str()
        .unwrap()
        .parse()
        .unwrap();
    let ocr_start: f64 = packets(&after, &ocr["index"])[0]
        .0
        .as_str()
        .unwrap()
        .parse()
        .unwrap();
    assert!(
        (start - ocr_start).abs() < 0.01,
        "OCR shifted from {start} to {ocr_start}"
    );
    let video_start = |data: &Value| {
        packets(data, &stream(data, "video")["index"])
            .iter()
            .filter_map(|p| p.0.as_str()?.parse::<f64>().ok())
            .fold(f64::INFINITY, f64::min)
    };
    assert!(
        (video_start(&before) - video_start(&after)).abs() < 0.01,
        "video timeline shifted"
    );
    let a = audible_start(&mixed);
    let b = audible_start(&output);
    assert!(
        (a - b).abs() < 0.015,
        "negative priming shifted audio from {a} to {b}"
    );
    assert_no_private_staging(dir.path());
}

#[test]
fn matroska_final_outputs_preserve_six_chapters_and_format_metadata() {
    let (dir, input) = fixture();
    let sup = dir.path().join("bitmap.sup");
    let mixed = dir.path().join("mixed.mkv");
    let chapters = dir.path().join("chapters.ffmeta");
    let source = dir.path().join("chaptered.mkv");
    std::fs::write(&sup, synthetic_pgs()).unwrap();
    add_bitmap_streams(&input, &sup, &mixed, &["eng"]);
    let mut metadata = String::from(
        ";FFMETADATA1\ntitle=Cross Game Fixture\nartist=Fixture Artist\ncomment=Format note\n",
    );
    for (index, title) in [
        "Prologue", "Opening", "Part A", "Part B", "Ending", "Preview",
    ]
    .iter()
    .enumerate()
    {
        let start = index * 700 + 125;
        metadata.push_str(&format!("[CHAPTER]\nTIMEBASE=1/1000\nSTART={start}\nEND={}\ntitle={title}\ncomment=Section {}\n", start + 600, index + 1));
    }
    std::fs::write(&chapters, metadata).unwrap();
    run(Command::new("ffmpeg")
        .args(["-hide_banner", "-loglevel", "error", "-y", "-copyts", "-i"])
        .arg(&mixed)
        .arg("-i")
        .arg(&chapters)
        .args([
            "-map",
            "0",
            "-map_metadata",
            "1",
            "-map_chapters",
            "1",
            "-c",
            "copy",
        ])
        .arg(&source));
    let before = probe(&source);
    assert_eq!(before["chapters"].as_array().unwrap().len(), 6);
    for (key, value) in [
        ("title", "Cross Game Fixture"),
        ("ARTIST", "Fixture Artist"),
        ("COMMENT", "Format note"),
    ] {
        assert_eq!(before["format"]["tags"][key], value);
    }
    let direct = dir.path().join("direct.mkv");
    run(
        Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
            .args([
                "--hw-accel",
                "none",
                "--delete-source",
                "false",
                "--device",
                "fire_tv_stick_4k",
                "--ocr-format",
                "srt",
            ])
            .arg(&source)
            .arg(&direct),
    );
    let direct_probe = probe(&direct);
    assert_eq!(direct_probe["chapters"], before["chapters"]);
    for key in ["title", "ARTIST", "COMMENT"] {
        assert_eq!(
            direct_probe["format"]["tags"][key],
            before["format"]["tags"][key]
        );
    }
    let mut command = sonarr_command(&dir, &source, ".fixed");
    command.args([
        "--ocr-format",
        "ass",
        "--ocr-engine",
        "external",
        "--ocr-external-command",
        "echo OCR TEXT",
    ]);
    run(&mut command);
    let final_path = dir.path().join("chaptered.fixed.mkv");
    let final_probe = probe(&final_path);
    assert_eq!(final_probe["chapters"], before["chapters"]);
    for key in ["title", "ARTIST", "COMMENT"] {
        assert_eq!(
            final_probe["format"]["tags"][key],
            before["format"]["tags"][key]
        );
    }
    assert!(!source.exists());
    assert_no_private_staging(dir.path());
}

#[test]
fn matroska_transcode_preserves_overlapping_ass_fonts_metadata_and_sync() {
    let (dir, input) = fixture();
    let source_bytes = std::fs::read(&input).unwrap();
    let source = probe(&input);
    for ocr_format in ["srt", "ass"] {
        let output = dir.path().join(format!("output-{ocr_format}.mkv"));
        run(
            Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
                .env_remove("DIRECT_PLAY_NICE_CONFIG")
                .env_remove("DIRECT_PLAY_NICE_CONFIG_FILE")
                .args([
                    "--hw-accel",
                    "none",
                    "--delete-source",
                    "false",
                    "--device",
                    "fire_tv_stick_4k",
                    "--ocr-format",
                    ocr_format,
                ])
                .arg(&input)
                .arg(&output),
        );
        let converted = probe(&output);
        let sub_in = stream(&source, "subtitle");
        let sub_out = stream(&converted, "subtitle");
        assert_eq!(sub_out["codec_name"], "ass");
        assert_eq!(sub_out["extradata_hash"], sub_in["extradata_hash"]);
        assert_eq!(sub_out["disposition"], sub_in["disposition"]);
        assert_eq!(sub_out["tags"]["language"], "eng");
        assert_eq!(sub_out["tags"]["title"], "English Signs and Dialogue");
        let expected = packets(&source, &sub_in["index"]);
        assert_eq!(expected.len(), 3);
        assert_eq!(packets(&converted, &sub_out["index"]), expected);
        let font_in = stream(&source, "attachment");
        let font_out = stream(&converted, "attachment");
        assert_eq!(font_in["extradata_hash"], font_out["extradata_hash"]);
        assert_eq!(font_in["tags"]["filename"], font_out["tags"]["filename"]);
        assert_eq!(font_in["tags"]["mimetype"], font_out["tags"]["mimetype"]);
        assert_eq!(stream(&converted, "video")["codec_name"], "h264");
        assert_eq!(stream(&converted, "audio")["codec_name"], "aac");
        let first_video_pts = |file: &Value| {
            let s = stream(file, "video");
            packets(file, &s["index"])
                .iter()
                .filter_map(|(pts, _, _)| pts.as_str()?.parse::<f64>().ok())
                .fold(f64::INFINITY, f64::min)
        };
        let video_shift = first_video_pts(&converted) - first_video_pts(&source);
        assert!(
            video_shift.abs() < 0.015,
            "video timestamp shifted by {video_shift}s"
        );
        let a = audible_start(&input);
        let b = audible_start(&output);
        assert!((a - b).abs() < 0.015, "audio onset shifted: {a}s -> {b}s");
        assert_eq!(std::fs::read(&input).unwrap(), source_bytes);
    }
}
