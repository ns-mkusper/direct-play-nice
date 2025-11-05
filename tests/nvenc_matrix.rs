mod common;

use common::ensure_ffmpeg_present;
use serde_json::Value;
use std::error::Error;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::tempdir;

const ENABLE_ENV: &str = "ENABLE_NVENC_TESTS";

#[derive(Clone)]
struct NvencCase<'a> {
    name: &'a str,
    input: InputKind,
    extra_args: &'a [&'a str],
    expected_width: u32,
    expected_height: u32,
    expected_profile: &'a str,
    expected_level: i32,
    min_bitrate: Option<u64>,
    max_bitrate: Option<u64>,
}

#[derive(Clone, Copy)]
enum InputKind {
    Hd1080,
    Uhd2160,
}

#[derive(Debug)]
struct ProbeSummary {
    width: u32,
    height: u32,
    profile: String,
    level: i32,
    stream_bitrate: Option<u64>,
    format_bitrate: Option<u64>,
    format_size: Option<u64>,
    format_duration_sec: Option<f64>,
    encoder_tag: Option<String>,
}

impl ProbeSummary {
    fn effective_bitrate(&self) -> Option<u64> {
        if let Some(b) = self.stream_bitrate.filter(|b| *b > 0) {
            return Some(b);
        }
        if let Some(b) = self.format_bitrate.filter(|b| *b > 0) {
            return Some(b);
        }
        match (self.format_size, self.format_duration_sec) {
            (Some(size), Some(duration)) if duration > 0.0 => {
                Some(((size as f64 * 8.0) / duration).round() as u64)
            }
            _ => None,
        }
    }
}

#[test]
fn nvenc_end_to_end_matrix() -> Result<(), Box<dyn Error>> {
    if !nvenc_tests_enabled() {
        eprintln!(
            "skipping NVENC matrix (set {}=1 to enable hardware regression tests)",
            ENABLE_ENV
        );
        return Ok(());
    }
    if !tool_available("ffmpeg", &["-version"]) || !tool_available("ffprobe", &["-version"]) {
        eprintln!("skipping NVENC matrix (ffmpeg/ffprobe missing)");
        return Ok(());
    }
    if !ffmpeg_has_nvenc() {
        eprintln!("skipping NVENC matrix (h264_nvenc unavailable)");
        return Ok(());
    }

    ensure_ffmpeg_present();
    let exe = env!("CARGO_BIN_EXE_direct_play_nice");

    let cases = vec![
        NvencCase {
            name: "all_devices_match_source",
            input: InputKind::Hd1080,
            extra_args: &["--hw-accel=nvenc"],
            expected_width: 1280,
            expected_height: 720,
            expected_profile: "High",
            expected_level: 41,
            min_bitrate: None,
            max_bitrate: Some(65_000_000),
        },
        NvencCase {
            name: "chromecast_1080_direct",
            input: InputKind::Hd1080,
            extra_args: &["--hw-accel=nvenc", "-s", "chromecast_2nd_gen"],
            expected_width: 1920,
            expected_height: 1080,
            expected_profile: "High",
            expected_level: 41,
            min_bitrate: None,
            max_bitrate: Some(65_000_000),
        },
        NvencCase {
            name: "all_devices_capped_bitrate",
            input: InputKind::Hd1080,
            extra_args: &["--hw-accel=nvenc", "--max-video-bitrate", "4M"],
            expected_width: 1280,
            expected_height: 720,
            expected_profile: "High",
            expected_level: 41,
            min_bitrate: Some(3_000_000),
            max_bitrate: Some(4_500_000),
        },
        NvencCase {
            name: "forced_720p_quality",
            input: InputKind::Hd1080,
            extra_args: &["--hw-accel=nvenc", "--video-quality", "720p"],
            expected_width: 1280,
            expected_height: 720,
            expected_profile: "High",
            expected_level: 41,
            min_bitrate: None,
            max_bitrate: Some(65_000_000),
        },
        NvencCase {
            name: "google_tv_streamer_4k",
            input: InputKind::Uhd2160,
            extra_args: &[
                "--hw-accel=nvenc",
                "-s",
                "google_tv_streamer",
                "--video-quality",
                "2160p",
            ],
            expected_width: 3840,
            expected_height: 2160,
            expected_profile: "High",
            expected_level: 52,
            min_bitrate: None,
            max_bitrate: Some(250_000_000),
        },
    ];

    for case in cases {
        let temp = tempdir()?;
        let input_path = generate_input_clip(temp.path(), case.input)?;
        let output_path = temp.path().join(format!("{}.mp4", case.name));

        let mut cmd_args: Vec<&str> = case.extra_args.iter().copied().collect();
        cmd_args.push(input_path.to_str().unwrap());
        cmd_args.push(output_path.to_str().unwrap());

        let output = Command::new(exe).args(&cmd_args).output()?;
        if !output.status.success() {
            return Err(format!(
                "NVENC case '{}' failed\nstdout:\n{}\nstderr:\n{}",
                case.name,
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            )
            .into());
        }

        let probe = probe_output(&output_path)?;
        assert_eq!(
            probe.width, case.expected_width,
            "case '{}': unexpected output width",
            case.name
        );
        assert_eq!(
            probe.height, case.expected_height,
            "case '{}': unexpected output height",
            case.name
        );
        assert_eq!(
            probe.profile, case.expected_profile,
            "case '{}': unexpected profile",
            case.name
        );
        assert_eq!(
            probe.level, case.expected_level,
            "case '{}': unexpected level",
            case.name
        );

        if let Some(ref tag) = probe.encoder_tag {
            assert!(
                tag.to_ascii_lowercase().contains("nvenc"),
                "case '{}': encoder tag '{}' does not report NVENC",
                case.name,
                tag
            );
        } else {
            panic!("case '{}': missing encoder metadata tag", case.name);
        }

        if case.min_bitrate.is_some() || case.max_bitrate.is_some() {
            if let Some(effective) = probe.effective_bitrate() {
                if let Some(min) = case.min_bitrate {
                    assert!(
                        effective >= min,
                        "case '{}': bitrate {} bps below minimum {}",
                        case.name,
                        effective,
                        min
                    );
                }
                if let Some(max) = case.max_bitrate {
                    assert!(
                        effective <= max,
                        "case '{}': bitrate {} bps exceeds max {}",
                        case.name,
                        effective,
                        max
                    );
                }
            } else {
                panic!(
                    "case '{}': unable to determine effective bitrate for validation",
                    case.name
                );
            }
        }
    }

    Ok(())
}

fn nvenc_tests_enabled() -> bool {
    matches!(
        std::env::var(ENABLE_ENV).ok().as_deref(),
        Some("1") | Some("true") | Some("yes")
    )
}

fn tool_available(binary: &str, args: &[&str]) -> bool {
    Command::new(binary)
        .args(args)
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

fn ffmpeg_has_nvenc() -> bool {
    if let Ok(output) = Command::new("ffmpeg")
        .args(["-hide_banner", "-encoders"])
        .output()
    {
        if output.status.success() {
            return String::from_utf8_lossy(&output.stdout).contains("h264_nvenc");
        }
    }
    false
}

fn generate_input_clip(dir: &Path, kind: InputKind) -> Result<PathBuf, Box<dyn Error>> {
    let (name, width, height, level) = match kind {
        InputKind::Hd1080 => ("input_hd.mkv", 1920, 1080, "5.1"),
        InputKind::Uhd2160 => ("input_uhd.mkv", 3840, 2160, "5.2"),
    };
    let target = dir.join(name);

    let size_arg = format!("{}x{}", width, height);
    let duration = "3";
    let status = Command::new("ffmpeg")
        .args([
            "-y",
            "-f",
            "lavfi",
            "-i",
            &format!("testsrc=size={}:rate=30:duration={}", size_arg, duration),
            "-f",
            "lavfi",
            "-i",
            &format!("sine=frequency=440:duration={}", duration),
            "-c:v",
            "libx264",
            "-preset",
            "veryfast",
            "-pix_fmt",
            "yuv420p",
            "-profile:v",
            "high",
            "-level:v",
            level,
            "-c:a",
            "aac",
            "-shortest",
            target.to_str().unwrap(),
        ])
        .status()?;

    if !status.success() {
        return Err(format!(
            "ffmpeg failed to generate {} clip (status: {})",
            size_arg, status
        )
        .into());
    }

    Ok(target)
}

fn probe_output(path: &Path) -> Result<ProbeSummary, Box<dyn Error>> {
    let output = Command::new("ffprobe")
        .args([
            "-v",
            "error",
            "-print_format",
            "json",
            "-show_streams",
            "-show_format",
            path.to_str().unwrap(),
        ])
        .output()?;
    if !output.status.success() {
        return Err(format!(
            "ffprobe failed:\n{}",
            String::from_utf8_lossy(&output.stderr)
        )
        .into());
    }

    let json: Value = serde_json::from_slice(&output.stdout)?;
    let stream = json
        .get("streams")
        .and_then(|s| s.get(0))
        .ok_or("ffprobe missing stream info")?;

    let width = stream
        .get("width")
        .and_then(Value::as_u64)
        .ok_or("ffprobe missing width")? as u32;
    let height = stream
        .get("height")
        .and_then(Value::as_u64)
        .ok_or("ffprobe missing height")? as u32;
    let profile = stream
        .get("profile")
        .and_then(Value::as_str)
        .ok_or("ffprobe missing profile")?
        .to_string();
    let level = stream
        .get("level")
        .and_then(Value::as_i64)
        .ok_or("ffprobe missing level")? as i32;
    let stream_bitrate = parse_u64_field(stream.get("bit_rate"));
    let encoder_tag = stream
        .get("tags")
        .and_then(|tags| tags.get("encoder"))
        .and_then(Value::as_str)
        .map(|s| s.to_string());

    let format = json.get("format").ok_or("ffprobe missing format section")?;
    let format_bitrate = parse_u64_field(format.get("bit_rate"));
    let format_size = parse_u64_field(format.get("size"));
    let format_duration_sec = format
        .get("duration")
        .and_then(Value::as_str)
        .and_then(|s| s.parse::<f64>().ok());

    Ok(ProbeSummary {
        width,
        height,
        profile,
        level,
        stream_bitrate,
        format_bitrate,
        format_size,
        format_duration_sec,
        encoder_tag,
    })
}

fn parse_u64_field(value: Option<&Value>) -> Option<u64> {
    value.and_then(|v| match v {
        Value::Number(num) => num.as_u64(),
        Value::String(s) => s.parse::<u64>().ok(),
        _ => None,
    })
}
