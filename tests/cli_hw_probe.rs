//! GPU acceleration: probe-mode smoke tests
//!
//! These tests validate the new hardware probing CLI surfaces. They don't
//! require a GPU; they only check that JSON output is well-formed and
//! contains expected top-level keys. On machines with GPUs, the content
//! will include available devices/encoders; on CPU-only hosts, fields may
//! be empty or marked unavailable, which is fine.

use assert_cmd::prelude::*;
use predicates::prelude::*;
use std::process::Command;

#[test]
fn probe_hw_json_smoke() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("direct_play_nice")?;
    cmd.arg("--probe-hw").arg("--probe-json");
    let output = cmd.assert().success().get_output().stdout.clone();
    let v: serde_json::Value = serde_json::from_slice(&output)?;
    // Check basic structure
    assert!(v.get("ffmpeg").is_some(), "missing ffmpeg section");
    assert!(v.get("devices").is_some(), "missing devices section");
    assert!(
        v.get("hw_encoders").is_some(),
        "missing hw_encoders section"
    );
    Ok(())
}

#[test]
fn probe_codecs_hw_json_smoke() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("direct_play_nice")?;
    // Include codec lists; narrow to video and hardware-only for brevity
    cmd.args([
        "--probe-hw",
        "--probe-codecs",
        "--only-video",
        "--only-hw",
        "--probe-json",
    ]);
    let output = cmd.assert().success().get_output().stdout.clone();
    let v: serde_json::Value = serde_json::from_slice(&output)?;
    // Encoders/decoders arrays should exist even if empty
    assert!(v.get("encoders").is_some(), "missing encoders array");
    assert!(v.get("decoders").is_some(), "missing decoders array");
    Ok(())
}
