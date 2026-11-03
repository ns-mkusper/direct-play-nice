#![cfg(feature = "ffmpeg-cli-tests")]

//! Direct-play contract, checked on outputs only, whatever path made them.
//!
//! Every input in the matrix is converted for each device on the software
//! path and on `--hw-accel auto` (hardware when the host has it). Each output
//! is then judged with ffprobe, independently of DPN's own code:
//!
//! 1. DPN's own assessment of the output finds nothing left to convert.
//! 2. An 8-bit video stream never signals HDR (PQ/HLG transfer, BT.2020).
//! 3. The display aspect ratio matches the source's.
//! 4. Video is limited range, and full-range black really lands at code 16.
//!
//! Add a fixture here whenever a new kind of source needs coverage.

#[path = "common/mod.rs"]
mod common;

use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

const DEVICES: &[&str] = &["chromecast", "roku"];
const HW_PATHS: &[&str] = &["none", "auto"];

/// Extra conversion modes. AI upscaling needs ONNX Runtime and downloads a
/// model, so it only joins the matrix when `DPN_CONTRACT_AI_UPSCALE` is set.
fn modes() -> Vec<(&'static str, Vec<&'static str>)> {
    let mut modes = vec![("default", vec![])];
    if std::env::var_os("DPN_CONTRACT_AI_UPSCALE").is_some() {
        let ai = vec![
            "--ai-upscale-model",
            "realesr-animevideov3",
            "--ai-upscale-tile",
            "256",
            "--video-quality",
            "1080p",
        ];
        let mut restore = ai.clone();
        restore.push("--ai-upscale-restore");
        modes.push(("ai-upscale", ai));
        modes.push(("ai-restore", restore));
    }
    modes
}

struct Fixture {
    name: &'static str,
    file: &'static str,
    video: &'static str,
    opts: &'static [&'static str],
    /// Stored as full-range black, so limited-range output must measure ~16.
    full_range_black: bool,
}

const FIXTURES: &[Fixture] = &[
    Fixture {
        name: "NTSC DVD 4:3, SAR only in the container",
        file: "dvd.mkv",
        video: "testsrc2=size=720x480:rate=24000/1001:duration=1",
        opts: &["-vf", "setsar=8/9", "-c:v", "ffv1", "-pix_fmt", "yuv420p"],
        full_range_black: false,
    },
    Fixture {
        name: "untagged SD MPEG-2",
        file: "sd.mkv",
        video: "testsrc2=size=640x480:rate=24:duration=1",
        opts: &["-c:v", "mpeg2video"],
        full_range_black: false,
    },
    Fixture {
        name: "tagged BT.709 HD MPEG-2",
        file: "hd709.mkv",
        video: "testsrc2=size=1920x1080:rate=24:duration=1",
        opts: &[
            "-vf",
            "setparams=range=tv:colorspace=bt709:color_primaries=bt709:color_trc=bt709",
            "-c:v",
            "mpeg2video",
        ],
        full_range_black: false,
    },
    Fixture {
        name: "full-range yuv420p tagged pc",
        file: "full.mkv",
        video: "color=c=black:size=640x360:rate=24:duration=1",
        opts: &[
            "-vf",
            "scale=out_range=full,format=yuv420p",
            "-c:v",
            "ffv1",
            "-color_range",
            "pc",
        ],
        full_range_black: true,
    },
    Fixture {
        name: "10-bit HDR10 (PQ, BT.2020)",
        file: "pq.mkv",
        video: "testsrc2=size=1280x720:rate=24:duration=1",
        opts: &[
            "-vf",
            "format=yuv420p10le,setparams=range=tv:colorspace=bt2020nc:color_primaries=bt2020:color_trc=smpte2084",
            "-c:v",
            "ffv1",
        ],
        full_range_black: false,
    },
    Fixture {
        name: "odd-size HD",
        file: "odd.mkv",
        video: "testsrc2=size=1281x721:rate=24:duration=1",
        opts: &["-c:v", "ffv1", "-pix_fmt", "yuv420p"],
        full_range_black: false,
    },
];

fn make_fixture(fixture: &Fixture, dir: &Path) -> PathBuf {
    let path = dir.join(fixture.file);
    let status = Command::new("ffmpeg")
        .args([
            "-hide_banner",
            "-loglevel",
            "error",
            "-y",
            "-f",
            "lavfi",
            "-i",
        ])
        .arg(fixture.video)
        .args([
            "-f",
            "lavfi",
            "-i",
            "sine=frequency=1000:sample_rate=48000:duration=1",
        ])
        .args(fixture.opts)
        .args(["-c:a", "aac", "-shortest"])
        .arg(&path)
        .status()
        .expect("run ffmpeg");
    assert!(
        status.success(),
        "fixture `{}` failed to generate",
        fixture.name
    );
    path
}

/// An isolated DPN invocation: empty config, private lock and cache dirs.
fn dpn(tmp: &Path) -> Command {
    let config = tmp.join("empty.toml");
    fs::write(&config, "").unwrap();
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env("DIRECT_PLAY_NICE_LOCK_DIR", tmp.join("locks"))
        .env("XDG_CONFIG_HOME", tmp.join("xdg"))
        .env("XDG_CACHE_HOME", tmp.join("xdg"))
        .env_remove("DIRECT_PLAY_NICE_CONFIG")
        .env_remove("sonarr_eventtype")
        .env_remove("radarr_eventtype")
        .arg("--config-file")
        .arg(&config);
    cmd
}

fn video_props(path: &Path) -> HashMap<String, String> {
    let out = Command::new("ffprobe")
        .args([
            "-v",
            "error",
            "-select_streams",
            "v:0",
            "-show_entries",
            "stream=width,height,sample_aspect_ratio,pix_fmt,color_range,color_transfer,color_primaries",
            "-of",
            "default=noprint_wrappers=1",
        ])
        .arg(path)
        .output()
        .expect("run ffprobe");
    String::from_utf8_lossy(&out.stdout)
        .lines()
        .filter_map(|l| l.split_once('='))
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect()
}

fn display_aspect(p: &HashMap<String, String>) -> f64 {
    let w: f64 = p["width"].parse().unwrap();
    let h: f64 = p["height"].parse().unwrap();
    let sar = p
        .get("sample_aspect_ratio")
        .and_then(|s| s.split_once(':'))
        .and_then(|(n, d)| Some((n.parse::<f64>().ok()?, d.parse::<f64>().ok()?)))
        .filter(|(n, d)| *n > 0.0 && *d > 0.0)
        .map_or(1.0, |(n, d)| n / d);
    w * sar / h
}

fn is_8bit(pix_fmt: &str) -> bool {
    !["9", "10", "12", "14", "16"]
        .iter()
        .any(|depth| pix_fmt.contains(&format!("p{depth}")) || pix_fmt.ends_with(depth))
}

fn mean_luma(path: &Path) -> f64 {
    let out = Command::new("ffmpeg")
        .args(["-v", "error", "-i"])
        .arg(path)
        .args([
            "-frames:v",
            "1",
            "-vf",
            "extractplanes=y",
            "-f",
            "rawvideo",
            "-pix_fmt",
            "gray",
            "-",
        ])
        .output()
        .expect("decode first frame");
    out.stdout.iter().map(|&b| b as f64).sum::<f64>() / out.stdout.len().max(1) as f64
}

/// Every contract breach for one output; empty when it holds.
fn contract_violations(
    fixture: &Fixture,
    source: &Path,
    output: &Path,
    device: &str,
    tmp: &Path,
) -> Vec<String> {
    let mut v = Vec::new();
    let out = video_props(output);
    let src = video_props(source);

    let dry_run = dpn(tmp)
        .args([
            "--device",
            device,
            "--sub-mode",
            "skip",
            "--dry-run",
            "--output",
            "json",
        ])
        .arg(output)
        .arg(tmp.join("again.mp4"))
        .output()
        .expect("run dry-run");
    match serde_json::from_slice::<serde_json::Value>(&dry_run.stdout) {
        Ok(report) if report["reasons"] == serde_json::json!([]) => {}
        Ok(report) => v.push(format!(
            "output still needs conversion: {}",
            report["reasons"]
        )),
        Err(err) => v.push(format!(
            "dry-run on output failed ({err}): {}",
            String::from_utf8_lossy(&dry_run.stderr)
        )),
    }

    let pix_fmt = out.get("pix_fmt").map(String::as_str).unwrap_or("");
    if is_8bit(pix_fmt) {
        let trc = out.get("color_transfer").map(String::as_str).unwrap_or("");
        let prim = out.get("color_primaries").map(String::as_str).unwrap_or("");
        if matches!(trc, "smpte2084" | "arib-std-b67") || prim == "bt2020" {
            v.push(format!(
                "8-bit {pix_fmt} signals HDR (transfer {trc}, primaries {prim})"
            ));
        }
    }

    let (sd, od) = (display_aspect(&src), display_aspect(&out));
    if (od / sd - 1.0).abs() > 0.01 {
        v.push(format!("display aspect {sd:.4} became {od:.4}"));
    }

    if out.get("color_range").map(String::as_str) == Some("pc") {
        v.push("video is tagged full range".to_string());
    }
    if fixture.full_range_black {
        let y = mean_luma(output);
        if !(14.0..=18.0).contains(&y) {
            v.push(format!(
                "full-range black stored at {y:.1}, expected limited-range 16"
            ));
        }
    }
    v
}

#[test]
fn every_output_honours_the_direct_play_contract() {
    common::ensure_ffmpeg_present();
    let tmp = TempDir::new().unwrap();
    let mut failures = Vec::new();
    for fixture in FIXTURES {
        let source = make_fixture(fixture, tmp.path());
        for device in DEVICES {
            for hw in HW_PATHS {
                for (mode, extra) in modes() {
                    let case = format!("{} / {device} / --hw-accel {hw} / {mode}", fixture.name);
                    let work = tmp
                        .path()
                        .join(format!("{}-{device}-{hw}-{mode}", fixture.file));
                    fs::create_dir_all(&work).unwrap();
                    let output = work.join("out.mp4");
                    let run = dpn(&work)
                        .args(["--device", device, "--hw-accel", hw, "--sub-mode", "skip"])
                        .args(&extra)
                        .arg(&source)
                        .arg(&output)
                        .output()
                        .expect("run direct_play_nice");
                    if !run.status.success() || !output.is_file() {
                        failures.push(format!(
                            "{case}: conversion failed: {}",
                            String::from_utf8_lossy(&run.stderr)
                                .lines()
                                .last()
                                .unwrap_or("")
                        ));
                        continue;
                    }
                    for violation in contract_violations(fixture, &source, &output, device, &work) {
                        failures.push(format!("{case}: {violation}"));
                    }
                }
            }
        }
    }
    assert!(
        failures.is_empty(),
        "direct-play contract violations:\n- {}",
        failures.join("\n- ")
    );
}
