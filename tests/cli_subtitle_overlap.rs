#![cfg(feature = "ffmpeg-cli-tests")]

use std::process::Command;
use tempfile::TempDir;

fn fixture(second_start: &str) -> (TempDir, std::path::PathBuf) {
    let dir = TempDir::new().unwrap();
    let ass = dir.path().join("input.ass");
    let input = dir.path().join("input.mkv");
    std::fs::write(&ass, format!("[Script Info]\nScriptType: v4.00+\n\n[V4+ Styles]\nFormat: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding\nStyle: Default,Arial,20,&H00FFFFFF,&H000000FF,&H00000000,&H00000000,0,0,0,0,100,100,0,0,1,1,0,2,10,10,10,1\n\n[Events]\nFormat: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text\nDialogue: 0,0:00:01.00,0:00:04.00,Default,,0,0,0,,Hello there\nDialogue: 0,{second_start},0:00:05.00,Default,,0,0,0,,Sign text\n")).unwrap();
    let output = Command::new("ffmpeg")
        .args([
            "-hide_banner",
            "-loglevel",
            "error",
            "-y",
            "-f",
            "lavfi",
            "-i",
            "testsrc=size=160x120:rate=10:duration=6",
            "-f",
            "lavfi",
            "-i",
            "sine=duration=6",
            "-i",
        ])
        .arg(&ass)
        .args([
            "-map", "0:v", "-map", "1:a", "-map", "2:s", "-c:v", "mpeg4", "-c:a", "aac", "-c:s",
            "ass",
        ])
        .arg(&input)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    (dir, input)
}

fn convert(input: &std::path::Path, output: &std::path::Path) -> std::process::Output {
    Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
        .args([
            "--hw-accel",
            "none",
            "--delete-source",
            "false",
            "-s",
            "chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra",
        ])
        .arg(input)
        .arg(output)
        .output()
        .unwrap()
}

#[test]
fn overlapping_ass_cues_must_not_produce_a_successful_truncated_output() {
    for second_start in ["0:00:01.00", "0:00:01.01", "0:00:02.00"] {
        let (dir, input) = fixture(second_start);
        let output = dir.path().join("output.mp4");
        let result = convert(&input, &output);
        assert!(
            !result.status.success(),
            "overlapping ASS must fail closed: {}",
            String::from_utf8_lossy(&result.stderr)
        );
        assert!(
            !output.exists(),
            "failed conversion must not publish an output"
        );
        assert!(
            input.exists(),
            "failed conversion must not delete the source"
        );
        assert!(
            String::from_utf8_lossy(&result.stderr).contains("overlapping subtitle"),
            "{}",
            String::from_utf8_lossy(&result.stderr)
        );
    }
}

#[test]
fn failed_mp4_conversion_keeps_source_and_existing_destination() {
    let (dir, input) = fixture("0:00:01.01");
    let output = dir.path().join("existing.mp4");
    let source = std::fs::read(&input).unwrap();
    std::fs::write(&output, b"existing destination").unwrap();
    let result = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
        .args([
            "--hw-accel",
            "none",
            "--delete-source",
            "true",
            "-s",
            "chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra",
        ])
        .arg(&input)
        .arg(&output)
        .output()
        .unwrap();
    assert!(
        !result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    assert_eq!(std::fs::read(input).unwrap(), source);
    assert_eq!(std::fs::read(output).unwrap(), b"existing destination");
}

#[test]
fn non_overlapping_ass_cues_retain_their_timing() {
    let (dir, input) = fixture("0:00:04.00");
    let output = dir.path().join("output.mp4");
    let result = convert(&input, &output);
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    let extracted = Command::new("ffmpeg")
        .args(["-hide_banner", "-loglevel", "error", "-i"])
        .arg(&output)
        .args(["-map", "0:s:0", "-f", "srt", "-"])
        .output()
        .unwrap();
    assert!(
        extracted.status.success(),
        "{}",
        String::from_utf8_lossy(&extracted.stderr)
    );
    let srt = String::from_utf8(extracted.stdout).unwrap();
    assert!(srt.contains("00:00:01,000 --> 00:00:04,000"), "{srt}");
    assert!(srt.contains("Hello there"), "{srt}");
    assert!(srt.contains("00:00:04,000 --> 00:00:05,000"), "{srt}");
    assert!(srt.contains("Sign text"), "{srt}");
}
