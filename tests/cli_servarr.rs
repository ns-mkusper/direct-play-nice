#[path = "common/mod.rs"]
mod common;

use assert_cmd::prelude::*;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

fn append_suffix(path: &Path, suffix: &str) -> PathBuf {
    let filename = path
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_else(|| String::from("file"));
    let new_name = match filename.rfind('.') {
        Some(idx) => {
            let (stem, ext) = filename.split_at(idx);
            format!("{}{}{}", stem, suffix, ext)
        }
        None => format!("{}{}", filename, suffix),
    };
    match path.parent() {
        Some(parent) => parent.join(new_name),
        None => PathBuf::from(new_name),
    }
}

#[test]
fn sonarr_test_event_short_circuits() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("direct_play_nice")?;
    cmd.env("sonarr_eventtype", "Test");
    common::assert_cli_success(cmd);
    Ok(())
}

#[test]
fn sonarr_grab_event_skips_conversion() -> Result<(), Box<dyn std::error::Error>> {
    let tmp = TempDir::new()?;
    let input = tmp.path().join("sample.mkv");
    fs::write(&input, b"dummy")?;
    let before_len = fs::metadata(&input)?.len();
    let temp_path = append_suffix(&input, ".direct-play-nice.tmp");
    let backup_path = append_suffix(&input, ".direct-play-nice.bak");

    let mut cmd = Command::cargo_bin("direct_play_nice")?;
    cmd.env("sonarr_eventtype", "Grab")
        .env("sonarr_episodefile_path", &input);

    let assert = cmd.assert().success();
    let stdout = String::from_utf8_lossy(&assert.get_output().stdout);
    assert!(
        stdout.contains("Sonarr event 'Grab'"),
        "expected informational log about skipping Grab event"
    );

    assert!(input.exists(), "original file should remain in place");
    assert_eq!(before_len, fs::metadata(&input)?.len(), "file size changed");
    assert!(
        !temp_path.exists(),
        "no temporary output should be produced for Grab events"
    );
    assert!(
        !backup_path.exists(),
        "no backup file should be produced for Grab events"
    );

    Ok(())
}

#[test]
fn sonarr_download_converts_and_replaces() -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let (input, original_duration) = common::gen_problem_input(&tmp);
    let final_path = input.with_extension("mp4");
    let backup_path = append_suffix(&input, ".direct-play-nice.bak");

    if final_path.exists() {
        fs::remove_file(&final_path)?;
    }
    if backup_path.exists() {
        fs::remove_file(&backup_path)?;
    }

    let mut cmd = Command::cargo_bin("direct_play_nice")?;
    cmd.env("sonarr_eventtype", "Download")
        .env("sonarr_episodefile_path", &input)
        .env("sonarr_series_title", "Example Series");

    common::assert_cli_success(cmd);

    assert!(final_path.exists(), "converted file was not promoted");
    assert!(
        !input.exists(),
        "original file still present after conversion"
    );
    assert!(!backup_path.exists(), "backup file should be cleaned up");

    let final_duration = common::probe_duration_ms(&final_path);
    let delta = (final_duration as i64 - original_duration as i64).abs();
    assert!(delta <= 100, "unexpected duration delta: {} ms", delta);

    Ok(())
}

#[test]
fn sonarr_upgrade_download_flag_converts() -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let (input, original_duration) = common::gen_problem_input(&tmp);
    let final_path = input.with_extension("mp4");
    let backup_path = append_suffix(&input, ".direct-play-nice.bak");

    if final_path.exists() {
        fs::remove_file(&final_path)?;
    }
    if backup_path.exists() {
        fs::remove_file(&backup_path)?;
    }

    let mut cmd = Command::cargo_bin("direct_play_nice")?;
    cmd.env("sonarr_eventtype", "Download")
        .env("sonarr_episodefile_path", &input)
        .env("sonarr_series_title", "Example Series")
        .env("sonarr_isupgrade", "True");

    common::assert_cli_success(cmd);

    assert!(final_path.exists(), "converted file was not promoted");
    assert!(
        !input.exists(),
        "original file still present after conversion"
    );
    assert!(!backup_path.exists(), "backup file should be cleaned up");

    let final_duration = common::probe_duration_ms(&final_path);
    let delta = (final_duration as i64 - original_duration as i64).abs();
    assert!(delta <= 100, "unexpected duration delta: {} ms", delta);

    Ok(())
}

#[test]
fn radarr_download_with_match_input_extension_replaces_in_place(
) -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let (input, original_duration) = common::gen_problem_input(&tmp);
    let backup_path = append_suffix(&input, ".direct-play-nice.bak");

    if backup_path.exists() {
        fs::remove_file(&backup_path)?;
    }

    let mut cmd = Command::cargo_bin("direct_play_nice")?;
    cmd.env("radarr_eventtype", "Download")
        .env("radarr_moviefile_path", &input)
        .env("radarr_movie_title", "Example Movie")
        .arg("--servarr-output-extension")
        .arg("match-input");

    common::assert_cli_success(cmd);

    assert!(input.exists(), "file should remain at original path");
    assert!(!backup_path.exists(), "backup file should not persist");

    let final_duration = common::probe_duration_ms(&input);
    let delta = (final_duration as i64 - original_duration as i64).abs();
    assert!(delta <= 100, "unexpected duration delta: {} ms", delta);

    Ok(())
}

#[test]
fn radarr_test_event_short_circuits() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("direct_play_nice")?;
    cmd.env("radarr_eventtype", "Test");
    common::assert_cli_success(cmd);
    Ok(())
}
