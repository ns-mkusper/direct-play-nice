#![cfg(feature = "ffmpeg-cli-tests")]

//! `--dry-run` must report the plan and leave the filesystem exactly as it found it.
//!
//! Every test snapshots the whole temp directory (paths, sizes, mtimes) before and
//! after the run, with the lock dir, language cache, and config home all pointed
//! inside that directory, so any write DPN attempted would show up in the diff.

mod common;

use common::{ensure_ffmpeg_present, gen_problem_input};
use std::collections::BTreeMap;
use std::error::Error;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::SystemTime;
use tempfile::TempDir;

type Snapshot = BTreeMap<PathBuf, (u64, SystemTime)>;

fn snapshot(root: &Path) -> Snapshot {
    fn walk(dir: &Path, root: &Path, out: &mut Snapshot) {
        for entry in fs::read_dir(dir).expect("read dir") {
            let entry = entry.expect("dir entry");
            let path = entry.path();
            let meta = fs::metadata(&path).expect("metadata");
            let rel = path.strip_prefix(root).unwrap().to_path_buf();
            if meta.is_dir() {
                out.insert(rel, (0, SystemTime::UNIX_EPOCH));
                walk(&path, root, out);
            } else {
                out.insert(rel, (meta.len(), meta.modified().expect("mtime")));
            }
        }
    }
    let mut out = Snapshot::new();
    walk(root, root, &mut out);
    out
}

fn assert_unchanged(before: &Snapshot, after: &Snapshot) {
    let added: Vec<_> = after.keys().filter(|k| !before.contains_key(*k)).collect();
    let removed: Vec<_> = before.keys().filter(|k| !after.contains_key(*k)).collect();
    let modified: Vec<_> = before
        .iter()
        .filter(|(k, v)| after.get(*k).is_some_and(|a| a != *v))
        .map(|(k, _)| k)
        .collect();
    assert!(
        added.is_empty() && removed.is_empty() && modified.is_empty(),
        "dry-run touched the filesystem: added={added:?} removed={removed:?} modified={modified:?}"
    );
}

/// Builds a command whose every side-channel write would land inside `tmp`.
fn isolated_cmd(tmp: &TempDir) -> Command {
    let lock_dir = tmp.path().join("locks");
    fs::create_dir_all(&lock_dir).expect("create lock dir");
    let xdg = tmp.path().join("xdg");
    fs::create_dir_all(&xdg).expect("create xdg dir");
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env("DIRECT_PLAY_NICE_LOCK_DIR", &lock_dir)
        .env(
            "DIRECT_PLAY_NICE_LANGUAGE_CACHE",
            tmp.path().join("language-cache.json"),
        )
        .env("XDG_CONFIG_HOME", &xdg)
        .env("XDG_CACHE_HOME", &xdg)
        .env_remove("DIRECT_PLAY_NICE_CONFIG")
        .env_remove("sonarr_eventtype")
        .env_remove("radarr_eventtype");
    cmd
}

fn run_and_capture(mut cmd: Command) -> String {
    let output = cmd.output().expect("run direct_play_nice");
    assert!(
        output.status.success(),
        "dry-run exited with {:?}\nstderr:\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout).expect("utf-8 stdout")
}

#[test]
fn direct_dry_run_reports_transcode_and_writes_nothing() -> Result<(), Box<dyn Error>> {
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let (input, _) = gen_problem_input(&tmp);
    let output = tmp.path().join("out.mp4");

    let mut cmd = isolated_cmd(&tmp);
    cmd.arg("-s")
        .arg("chromecast_1st_gen")
        .arg("--sub-mode")
        .arg("skip")
        .arg("--delete-source")
        .arg("--dry-run")
        .arg(&input)
        .arg(&output);

    let before = snapshot(tmp.path());
    let stdout = run_and_capture(cmd);
    let after = snapshot(tmp.path());

    assert_unchanged(&before, &after);
    assert!(!output.exists(), "dry-run must not create the output file");
    assert!(input.exists(), "dry-run must not delete the source");
    assert!(
        stdout.starts_with("Dry run: no file will be written"),
        "{stdout}"
    );
    assert!(
        stdout.contains("Mode:            direct conversion"),
        "{stdout}"
    );
    assert!(stdout.contains("Action:          transcode"), "{stdout}");
    assert!(
        stdout.contains(&format!("Output:          {}", output.display())),
        "{stdout}"
    );
    assert!(
        stdout.contains("deleted after the output passes verification"),
        "{stdout}"
    );
    Ok(())
}

#[test]
fn direct_dry_run_json_is_machine_readable() -> Result<(), Box<dyn Error>> {
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let (input, _) = gen_problem_input(&tmp);
    let output = tmp.path().join("out.mkv");

    let mut cmd = isolated_cmd(&tmp);
    cmd.arg("-s")
        .arg("roku")
        .arg("--sub-mode")
        .arg("skip")
        .arg("--dry-run")
        .arg("--output")
        .arg("json")
        .arg(&input)
        .arg(&output);

    let before = snapshot(tmp.path());
    let stdout = run_and_capture(cmd);
    let after = snapshot(tmp.path());
    assert_unchanged(&before, &after);

    let report: serde_json::Value = serde_json::from_str(&stdout)?;
    assert_eq!(report["action"], "transcode");
    assert_eq!(report["source_handling"], "keep");
    assert_eq!(report["input"], input.to_string_lossy().as_ref());
    assert_eq!(report["output"], output.to_string_lossy().as_ref());
    let temps = report["temp_outputs"]
        .as_array()
        .expect("temp_outputs array");
    assert_eq!(
        temps.len(),
        2,
        "direct MKV output stages through the promoted temp file plus the MP4 intermediate"
    );
    assert_eq!(
        temps[0],
        tmp.path()
            .join("out.direct-play-nice.tmp.mkv")
            .to_string_lossy()
            .as_ref()
    );
    assert_eq!(
        temps[1],
        tmp.path()
            .join("out.direct-play-nice.tmp.conv.mp4")
            .to_string_lossy()
            .as_ref()
    );
    assert!(!report["reasons"].as_array().unwrap().is_empty());
    Ok(())
}

#[test]
fn sonarr_dry_run_reports_replacement_paths_and_writes_nothing() -> Result<(), Box<dyn Error>> {
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let (input, _) = gen_problem_input(&tmp);
    let final_path = tmp.path().join("input.fixed.mp4");
    let temp_path = tmp.path().join("input.fixed.direct-play-nice.tmp.mp4");
    let backup_path = tmp.path().join("input.direct-play-nice.bak.mkv");

    let mut cmd = isolated_cmd(&tmp);
    cmd.env("sonarr_eventtype", "Download")
        .env("sonarr_episodefile_path", &input)
        .env("sonarr_series_title", "Example Series")
        .arg("-s")
        .arg("chromecast_1st_gen")
        .arg("--sub-mode")
        .arg("skip")
        .arg("--dry-run");

    let before = snapshot(tmp.path());
    let stdout = run_and_capture(cmd);
    let after = snapshot(tmp.path());

    assert_unchanged(&before, &after);
    assert!(input.exists(), "original must stay in place");
    assert!(!final_path.exists(), "no replacement must be written");
    assert!(!temp_path.exists(), "no temp output must be written");
    assert!(!backup_path.exists(), "no backup must be created");
    assert!(
        stdout.contains("Mode:            Sonarr Download event"),
        "{stdout}"
    );
    assert!(
        stdout.contains(&format!("Output:          {}", final_path.display())),
        "{stdout}"
    );
    assert!(
        stdout.contains(&format!("Temp output:     {}", temp_path.display())),
        "{stdout}"
    );
    assert!(
        stdout.contains(&format!("Backup:          {}", backup_path.display())),
        "{stdout}"
    );
    assert!(stdout.contains("moved to the backup path"), "{stdout}");
    Ok(())
}

#[test]
fn dry_run_from_config_file_is_honored() -> Result<(), Box<dyn Error>> {
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let (input, _) = gen_problem_input(&tmp);
    let output = tmp.path().join("out.mp4");
    let config_path = tmp.path().join("config.toml");
    fs::write(&config_path, "dry_run = true\nsub_mode = \"skip\"\n")?;

    let mut cmd = isolated_cmd(&tmp);
    cmd.arg("--config-file")
        .arg(&config_path)
        .arg("-s")
        .arg("chromecast_1st_gen")
        .arg(&input)
        .arg(&output);

    let before = snapshot(tmp.path());
    let stdout = run_and_capture(cmd);
    let after = snapshot(tmp.path());

    assert_unchanged(&before, &after);
    assert!(!output.exists(), "config dry_run=true must prevent output");
    assert!(stdout.starts_with("Dry run:"), "{stdout}");
    Ok(())
}

#[test]
fn direct_conversion_promotes_staged_output_and_leaves_no_temp() -> Result<(), Box<dyn Error>> {
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let (input, _) = gen_problem_input(&tmp);
    let output = tmp.path().join("out.mp4");
    let staged = tmp.path().join("out.direct-play-nice.tmp.mp4");

    let mut cmd = isolated_cmd(&tmp);
    cmd.arg("-s")
        .arg("chromecast_1st_gen")
        .arg("--sub-mode")
        .arg("skip")
        .arg("--audio-quality")
        .arg("192k")
        .arg(&input)
        .arg(&output);
    let out = cmd.output()?;
    assert!(
        out.status.success(),
        "conversion failed:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );

    assert!(output.exists(), "final output must be promoted into place");
    assert!(
        !staged.exists(),
        "staged temp file must not remain after promotion"
    );
    assert!(
        input.exists(),
        "input stays untouched without --delete-source"
    );
    Ok(())
}

#[test]
fn dry_run_reports_ai_upscale_without_loading_a_model() -> Result<(), Box<dyn Error>> {
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let (input, _) = gen_problem_input(&tmp);
    let output = tmp.path().join("out.mp4");

    let mut cmd = isolated_cmd(&tmp);
    cmd.env("DPN_UPSCALE_MODEL_DIR", tmp.path().join("models"))
        .arg("-s")
        .arg("roku")
        .arg("--video-quality")
        .arg("1080p")
        .arg("--sub-mode")
        .arg("skip")
        .arg("--ai-upscale-model")
        .arg("realesr-animevideov3")
        .arg("--dry-run")
        .arg("--output")
        .arg("json")
        .arg(&input)
        .arg(&output);

    let before = snapshot(tmp.path());
    let stdout = run_and_capture(cmd);
    let after = snapshot(tmp.path());
    assert_unchanged(&before, &after);

    let report: serde_json::Value = serde_json::from_str(&stdout)?;
    assert_eq!(report["ai_upscale"], "realesr-animevideov3");
    let reasons = report["reasons"].as_array().unwrap();
    assert!(
        reasons.iter().any(|r| r
            .as_str()
            .unwrap_or("")
            .contains("AI upscale (realesr-animevideov3) requested")),
        "reasons: {reasons:?}"
    );
    Ok(())
}

#[test]
fn custom_ai_upscale_model_requires_a_path() -> Result<(), Box<dyn Error>> {
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let (input, _) = gen_problem_input(&tmp);
    let output = tmp.path().join("out.mp4");

    let mut cmd = isolated_cmd(&tmp);
    cmd.arg("-s")
        .arg("roku")
        .arg("--video-quality")
        .arg("1080p")
        .arg("--sub-mode")
        .arg("skip")
        .arg("--ai-upscale-model")
        .arg("custom")
        .arg(&input)
        .arg(&output);
    let out = cmd.output()?;
    assert!(
        !out.status.success(),
        "custom model without a path must fail"
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("--ai-upscale-model-path"),
        "error should name the missing flag:\n{stderr}"
    );
    assert!(!output.exists(), "no output must be written");
    assert!(
        !tmp.path().join("out.direct-play-nice.tmp.mp4").exists(),
        "staged temp must be cleaned up"
    );
    Ok(())
}
