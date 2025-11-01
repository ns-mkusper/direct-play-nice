mod common;

use assert_cmd::prelude::*;
use common::{ensure_ffmpeg_present, gen_problem_input};
use predicates::str;
use std::error::Error;
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use tempfile::TempDir;

fn make_input(tmp: &TempDir) -> Result<PathBuf, Box<dyn Error>> {
    let (source, _) = gen_problem_input(tmp);
    Ok(source)
}

fn run_cli(input: &PathBuf, output: &PathBuf, extra_args: &[&str]) -> Result<(), Box<dyn Error>> {
    let mut cmd = Command::cargo_bin("direct_play_nice")?;
    cmd.arg("-s")
        .arg("chromecast_1st_gen")
        .arg(input)
        .arg(output);
    for arg in extra_args {
        cmd.arg(arg);
    }
    cmd.assert().success().stdout(str::is_empty());
    Ok(())
}

#[test]
fn delete_source_defaults_to_false() -> Result<(), Box<dyn Error>> {
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let input = make_input(&tmp)?;
    let output = tmp.path().join("out.mp4");

    run_cli(&input, &output, &[])?;

    assert!(input.exists(), "input file should remain by default");
    Ok(())
}

#[test]
fn delete_source_config_true_overridden_by_cli_false() -> Result<(), Box<dyn Error>> {
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let config_path = tmp.path().join("config.toml");
    fs::write(&config_path, "delete_source = true\n")?;

    let input = make_input(&tmp)?;
    let output = tmp.path().join("out.mp4");

    let mut cmd = Command::cargo_bin("direct_play_nice")?;
    cmd.arg("--config-file")
        .arg(&config_path)
        .arg("-s")
        .arg("chromecast_1st_gen")
        .arg(&input)
        .arg(&output)
        .arg("--delete-source=false");
    cmd.assert().success().stdout(str::is_empty());

    assert!(
        input.exists(),
        "CLI should override config delete_source=true"
    );
    Ok(())
}

#[test]
fn delete_source_flag_deletes_input() -> Result<(), Box<dyn Error>> {
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let input = make_input(&tmp)?;
    let output = tmp.path().join("out.mp4");

    run_cli(&input, &output, &["--delete-source"])?;

    assert!(!input.exists(), "--delete-source should remove input file");
    Ok(())
}

#[test]
fn delete_source_config_true_respected_without_cli_override() -> Result<(), Box<dyn Error>> {
    ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let config_path = tmp.path().join("config.toml");
    fs::write(&config_path, "delete_source = true\n")?;

    let input = make_input(&tmp)?;
    let output = tmp.path().join("out.mp4");

    let mut cmd = Command::cargo_bin("direct_play_nice")?;
    cmd.arg("--config-file")
        .arg(&config_path)
        .arg("-s")
        .arg("chromecast_1st_gen")
        .arg(&input)
        .arg(&output);
    cmd.assert().success().stdout(str::is_empty());

    assert!(
        !input.exists(),
        "config delete_source=true should remove input when not overridden"
    );
    Ok(())
}
