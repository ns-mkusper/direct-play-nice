#![cfg(feature = "ffmpeg-cli-tests")]

#[path = "common/mod.rs"]
mod common;

use std::fs;
use std::fs::OpenOptions;
use std::io::{Seek, SeekFrom, Write};
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

fn gen_direct_play_mp4(path: &Path) {
    assert!(
        Command::new("ffmpeg")
            .args([
                "-hide_banner",
                "-loglevel",
                "error",
                "-y",
                "-f",
                "lavfi",
                "-i",
                "testsrc=size=640x360:rate=30:duration=2",
                "-f",
                "lavfi",
                "-i",
                "sine=frequency=1000:sample_rate=48000:duration=2",
                "-c:v",
                "libx264",
                "-b:v",
                "700k",
                "-minrate",
                "700k",
                "-maxrate",
                "700k",
                "-bufsize",
                "1400k",
                "-profile:v",
                "high",
                "-level:v",
                "4.1",
                "-pix_fmt",
                "yuv420p",
                "-c:a",
                "aac",
                "-b:a",
                "96k",
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
fn sonarr_download_skips_already_direct_play_without_replacement(
) -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let input = tmp.path().join("already_direct_play.mp4");
    gen_direct_play_mp4(&input);

    let final_path = input.with_file_name("already_direct_play.fixed.mp4");
    let temp_path = append_suffix(&final_path, ".direct-play-nice.tmp");
    let backup_path = append_suffix(&input, ".direct-play-nice.bak");

    let config_path = tmp.path().join("direct-play-nice.toml");
    fs::write(&config_path, "")?;

    let lock_dir = tmp.path().join("locks");
    fs::create_dir_all(&lock_dir)?;

    let output = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
        .env("DIRECT_PLAY_NICE_LOCK_DIR", &lock_dir)
        .env("sonarr_eventtype", "Download")
        .env("sonarr_episodefile_path", &input)
        .env("sonarr_series_title", "Compat Test")
        .arg("--config-file")
        .arg(&config_path)
        .arg("-s")
        .arg("chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra")
        .arg("--video-quality")
        .arg("480p")
        .arg("--audio-quality")
        .arg("128k")
        .arg("--max-video-bitrate")
        .arg("1M")
        .output()?;

    assert!(
        output.status.success(),
        "expected already-compatible Sonarr file to be skipped successfully; stderr:
{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Input is direct-play compatible"),
        "stderr did not include direct-play skip message:
{stderr}"
    );

    assert!(input.exists(), "original file should remain in place");
    assert!(
        !final_path.exists(),
        "skip path should not promote a replacement output"
    );
    assert!(
        !temp_path.exists(),
        "skip path should not leave a temporary output"
    );
    assert!(
        !backup_path.exists(),
        "skip path should not create a source backup"
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

// Use an explicit config and isolated environment: these tests must never pick
// up a real service's Plex token, language policy, or imported media paths.
fn radarr_upgrade_command(tmp: &TempDir, input: &Path) -> Command {
    let config = tmp.path().join("upgrade.toml");
    fs::write(
        &config,
        r#"streaming_devices = "all"
hw_accel = "none"
video_quality = "480p"
max_video_bitrate = "1M"
servarr_output_extension = "mp4"
servarr_output_suffix = ".fixed"
delete_source = true
"#,
    )
    .unwrap();
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    for (key, _) in std::env::vars_os() {
        let name = key.to_string_lossy().to_ascii_lowercase();
        if name.starts_with("sonarr_")
            || name.starts_with("radarr_")
            || name.starts_with("direct_play_nice_")
            || matches!(name.as_str(), "plex_url" | "plex_token")
        {
            cmd.env_remove(key);
        }
    }
    cmd.env("HOME", tmp.path())
        .env("XDG_CONFIG_HOME", tmp.path())
        .env("XDG_CACHE_HOME", tmp.path())
        .env("DIRECT_PLAY_NICE_LOCK_DIR", tmp.path().join("locks"))
        .env("radarr_eventtype", "Download")
        .env("radarr_isupgrade", "True")
        .env("radarr_moviefile_path", input)
        .env("radarr_movie_title", "Synthetic upgrade")
        .args(["--config-file", config.to_str().unwrap()]);
    cmd
}

#[test]
fn radarr_upgrade_download_converts_and_removes_library_source(
) -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let (input, duration) = common::gen_problem_input(&tmp);
    let output = tmp.path().join("input.fixed.mp4");
    let backup = append_suffix(&input, ".direct-play-nice.bak");
    let temp = append_suffix(&output, ".direct-play-nice.tmp");
    // A separate download-client copy must not be removed by library replacement.
    let download = tmp.path().join("download.mkv");
    fs::copy(&input, &download)?;
    let download_bytes = fs::read(&download)?;
    let mut cmd = radarr_upgrade_command(&tmp, &input);
    cmd.env("radarr_moviefile_sourcepath", &download);
    common::assert_cli_success(cmd);

    assert!(output.is_file(), "upgrade output should be promoted");
    assert!(!input.exists(), "replaced library original must be removed");
    assert!(!backup.exists(), "original backup must be removed");
    assert!(!temp.exists(), "conversion temp file must be promoted");
    assert_eq!(fs::read(&download)?, download_bytes);
    assert!(common::probe_duration_ms(&output).abs_diff(duration) <= 100);
    // Decode the complete short synthetic output, not just its container header.
    assert!(Command::new("ffmpeg")
        .args(["-v", "error", "-xerror", "-i"])
        .arg(&output)
        .args(["-f", "null", "-"])
        .status()?
        .success());
    Ok(())
}

#[test]
fn radarr_upgrade_invalid_input_preserves_source() -> Result<(), Box<dyn std::error::Error>> {
    let tmp = TempDir::new()?;
    let input = tmp.path().join("input.mkv");
    let bytes = b"not a media file";
    fs::write(&input, bytes)?;
    let output = tmp.path().join("input.fixed.mp4");
    let result = radarr_upgrade_command(&tmp, &input).output()?;
    assert!(!result.status.success(), "invalid input must not succeed");
    assert_eq!(fs::read(&input)?, bytes);
    assert!(!output.exists());
    assert!(!append_suffix(&input, ".direct-play-nice.bak").exists());
    assert!(!append_suffix(&output, ".direct-play-nice.tmp").exists());
    Ok(())
}

#[test]
fn radarr_upgrade_promotion_failure_restores_source() -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let (input, _) = common::gen_problem_input(&tmp);
    let original_bytes = fs::read(&input)?;
    let output = tmp.path().join("input.fixed.mp4");
    // Make promotion fail after conversion without relying on Unix permissions.
    fs::create_dir(&output)?;
    let sentinel = output.join("keep.txt");
    fs::write(&sentinel, b"untouched")?;
    let result = radarr_upgrade_command(&tmp, &input).output()?;
    assert!(!result.status.success(), "blocked promotion must fail");
    assert!(
        String::from_utf8_lossy(&result.stderr).contains("could not promote"),
        "must exercise promotion rollback, not an earlier failure: {}",
        String::from_utf8_lossy(&result.stderr)
    );
    assert_eq!(fs::read(&input)?, original_bytes);
    assert_eq!(fs::read(&sentinel)?, b"untouched");
    assert!(!append_suffix(&input, ".direct-play-nice.bak").exists());
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

#[test]
fn sonarr_download_failure_restores_original_and_cleans_temp_files(
) -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let input = tmp.path().join("episode_corrupt.mkv");
    let temp_path = append_suffix(&input, ".direct-play-nice.tmp");
    let backup_path = append_suffix(&input, ".direct-play-nice.bak");
    let final_path = input.with_file_name("episode_corrupt.fixed.mp4");

    assert!(
        Command::new("ffmpeg")
            .args([
                "-y",
                "-f",
                "lavfi",
                "-i",
                "testsrc2=size=1280x720:rate=24:duration=12",
                "-f",
                "lavfi",
                "-i",
                "sine=frequency=1000:sample_rate=48000:duration=12",
                "-c:v",
                "libx265",
                "-preset",
                "medium",
                "-x265-params",
                "log-level=error",
                "-c:a",
                "aac",
                input.to_string_lossy().as_ref(),
            ])
            .status()
            .expect("generate hevc source")
            .success(),
        "ffmpeg failed to generate source"
    );

    let mut f = OpenOptions::new().read(true).write(true).open(&input)?;
    let len = f.metadata()?.len();
    f.seek(SeekFrom::Start(len / 2))?;
    f.write_all(&vec![0xAAu8; 64 * 1024])?;
    f.flush()?;

    let lock_dir = tmp.path().join("locks");
    fs::create_dir_all(&lock_dir)?;

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    let status = cmd
        .env("DIRECT_PLAY_NICE_LOCK_DIR", &lock_dir)
        .env("sonarr_eventtype", "Download")
        .env("sonarr_episodefile_path", &input)
        .env("sonarr_series_title", "Example Series")
        .status()?;

    assert!(
        !status.success(),
        "expected conversion to fail on corrupt input"
    );
    assert!(
        input.exists(),
        "original source should remain after failure"
    );
    assert!(
        !temp_path.exists(),
        "temporary conversion file should be removed after failure"
    );
    assert!(
        !backup_path.exists(),
        "backup should not remain after failure"
    );
    assert!(
        !final_path.exists(),
        "final promoted output should not exist after failure"
    );

    Ok(())
}
