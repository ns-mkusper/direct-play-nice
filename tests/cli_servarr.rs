#[path = "common/mod.rs"]
mod common;

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
    let tmp = TempDir::new()?;
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env("DIRECT_PLAY_NICE_LOCK_DIR", tmp.path())
        .env("sonarr_eventtype", "Test");
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

    let lock_dir = tmp.path().join("locks");
    fs::create_dir_all(&lock_dir)?;

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env("DIRECT_PLAY_NICE_LOCK_DIR", &lock_dir)
        .env("sonarr_eventtype", "Grab")
        .env("sonarr_episodefile_path", &input);

    common::assert_cli_success(cmd);

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
    let final_path = input.with_file_name(format!(
        "{}.fixed.mp4",
        input.file_stem().and_then(|s| s.to_str()).unwrap()
    ));
    let backup_path = append_suffix(&input, ".direct-play-nice.bak");

    if final_path.exists() {
        fs::remove_file(&final_path)?;
    }
    if backup_path.exists() {
        fs::remove_file(&backup_path)?;
    }

    let lock_dir = tmp.path().join("locks");
    fs::create_dir_all(&lock_dir)?;

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env("DIRECT_PLAY_NICE_LOCK_DIR", &lock_dir)
        .env("sonarr_eventtype", "Download")
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
    let final_path = input.with_file_name(format!(
        "{}.fixed.mp4",
        input.file_stem().and_then(|s| s.to_str()).unwrap()
    ));
    let backup_path = append_suffix(&input, ".direct-play-nice.bak");

    if final_path.exists() {
        fs::remove_file(&final_path)?;
    }
    if backup_path.exists() {
        fs::remove_file(&backup_path)?;
    }

    let lock_dir = tmp.path().join("locks");
    fs::create_dir_all(&lock_dir)?;

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env("DIRECT_PLAY_NICE_LOCK_DIR", &lock_dir)
        .env("sonarr_eventtype", "Download")
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
fn sonarr_download_uses_config_for_plex_refresh() -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let (input, original_duration) = common::gen_problem_input(&tmp);
    let final_path = input.with_file_name(format!(
        "{}.fixed.mp4",
        input.file_stem().and_then(|s| s.to_str()).unwrap()
    ));
    let backup_path = append_suffix(&input, ".direct-play-nice.bak");

    if final_path.exists() {
        fs::remove_file(&final_path)?;
    }
    if backup_path.exists() {
        fs::remove_file(&backup_path)?;
    }

    let config_path = tmp.path().join("direct-play-nice.toml");
    fs::write(
        &config_path,
        r#"[plex]
refresh = true
url = "http://127.0.0.1:9"
token = "test-token"
"#,
    )?;

    let lock_dir = tmp.path().join("locks");
    fs::create_dir_all(&lock_dir)?;

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env("DIRECT_PLAY_NICE_LOCK_DIR", &lock_dir)
        .env("sonarr_eventtype", "Download")
        .env("sonarr_episodefile_path", &input)
        .env("sonarr_series_title", "Example Series")
        .arg("--config-file")
        .arg(&config_path);

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
fn sonarr_download_handles_multiple_paths() -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;

    let (input, original_duration) = common::gen_problem_input(&tmp);
    let ep1 = input.with_file_name("episode1.mkv");
    fs::rename(&input, &ep1)?;

    let (second_input, original_duration2) = common::gen_problem_input(&tmp);
    let ep2 = second_input.with_file_name("episode2.mkv");
    fs::rename(&second_input, &ep2)?;

    let out1 = ep1.with_file_name("episode1.fixed.mp4");
    let out2 = ep2.with_file_name("episode2.fixed.mp4");
    let backup1 = append_suffix(&ep1, ".direct-play-nice.bak");
    let backup2 = append_suffix(&ep2, ".direct-play-nice.bak");

    for path in [&out1, &out2, &backup1, &backup2] {
        if path.exists() {
            fs::remove_file(path)?;
        }
    }

    let lock_dir = tmp.path().join("locks");
    fs::create_dir_all(&lock_dir)?;

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env("DIRECT_PLAY_NICE_LOCK_DIR", &lock_dir)
        .env("sonarr_eventtype", "Download")
        .env("sonarr_episodefile_path", "")
        .env(
            "sonarr_episodefile_paths",
            format!("{}|{}", ep1.to_string_lossy(), ep2.to_string_lossy()),
        )
        .env("sonarr_series_title", "Example Series Batch");

    common::assert_cli_success(cmd);

    assert!(out1.exists(), "first episode output missing");
    assert!(out2.exists(), "second episode output missing");
    assert!(!ep1.exists(), "first source should be removed");
    assert!(!ep2.exists(), "second source should be removed");
    assert!(!backup1.exists(), "first backup should be cleaned up");
    assert!(!backup2.exists(), "second backup should be cleaned up");

    let final_duration1 = common::probe_duration_ms(&out1);
    let final_duration2 = common::probe_duration_ms(&out2);
    let delta1 = (final_duration1 as i64 - original_duration as i64).abs();
    let delta2 = (final_duration2 as i64 - original_duration2 as i64).abs();
    assert!(
        delta1 <= 100,
        "unexpected duration delta for ep1: {} ms",
        delta1
    );
    assert!(
        delta2 <= 100,
        "unexpected duration delta for ep2: {} ms",
        delta2
    );

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

    let lock_dir = tmp.path().join("locks");
    fs::create_dir_all(&lock_dir)?;

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env("DIRECT_PLAY_NICE_LOCK_DIR", &lock_dir)
        .env("radarr_eventtype", "Download")
        .env("radarr_moviefile_path", &input)
        .env("radarr_movie_title", "Example Movie");

    common::assert_cli_success(cmd);

    let final_path = input.with_extension("mp4");

    assert!(final_path.exists(), "converted file was not promoted");
    assert!(
        !input.exists(),
        "original file should be removed after conversion"
    );
    assert!(!backup_path.exists(), "backup file should not persist");

    let final_duration = common::probe_duration_ms(&final_path);
    let delta = (final_duration as i64 - original_duration as i64).abs();
    assert!(delta <= 100, "unexpected duration delta: {} ms", delta);

    Ok(())
}

#[test]
fn radarr_test_event_short_circuits() -> Result<(), Box<dyn std::error::Error>> {
    let tmp = TempDir::new()?;
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env("DIRECT_PLAY_NICE_LOCK_DIR", tmp.path())
        .env("radarr_eventtype", "Test");
    common::assert_cli_success(cmd);
    Ok(())
}
