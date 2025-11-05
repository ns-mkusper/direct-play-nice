use std::error::Error;
use std::fs;
use std::path::Path;
use std::process::Command;

use tempfile::tempdir;

fn nvenc_tests_enabled() -> bool {
    matches!(std::env::var("ENABLE_NVENC_TESTS").ok().as_deref(), Some("1"))
}

fn tool_available(binary: &str, args: &[&str]) -> bool {
    Command::new(binary).args(args).status().map(|s| s.success()).unwrap_or(false)
}

fn ffmpeg_has_nvenc() -> bool {
    if let Ok(output) = Command::new("ffmpeg").args(["-hide_banner", "-encoders"]).output() {
        if output.status.success() {
            return String::from_utf8_lossy(&output.stdout).contains("h264_nvenc");
        }
    }
    false
}

fn create_input_clip(path: &Path) -> Result<(), Box<dyn Error>> {
    let status = Command::new("ffmpeg")
        .args([
            "-y",
            "-f",
            "lavfi",
            "-i",
            "testsrc=size=1920x1080:rate=30:duration=2",
            "-f",
            "lavfi",
            "-i",
            "sine=frequency=1000:duration=2",
            "-c:v",
            "libx264",
            "-preset",
            "ultrafast",
            "-pix_fmt",
            "yuv420p",
            "-c:a",
            "aac",
            "-shortest",
            path.to_str().unwrap(),
        ])
        .status()?;
    if !status.success() {
        Err("ffmpeg could not create test clip".into())
    } else {
        Ok(())
    }
}

fn run_nvenc_conversion(input: &Path, output: &Path) -> Result<(String, String), Box<dyn Error>> {
    let exe = env!("CARGO_BIN_EXE_direct_play_nice");
    let output = Command::new(exe)
        .args([
            "-s",
            "chromecast_2nd_gen",
            "--hw-accel=nvenc",
            input.to_str().unwrap(),
            output.to_str().unwrap(),
        ])
        .output()?;
    if !output.status.success() {
        return Err(format!(
            "direct_play_nice failed: stdout=\n{}\nstderr=\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        )
        .into());
    }
    Ok((
        String::from_utf8_lossy(&output.stdout).to_string(),
        String::from_utf8_lossy(&output.stderr).to_string(),
    ))
}

fn probe_output(path: &Path) -> Result<String, Box<dyn Error>> {
    let output = Command::new("ffprobe")
        .args([
            "-v",
            "error",
            "-select_streams",
            "v:0",
            "-show_entries",
            "stream=codec_name,profile,level:format_tags=encoder",
            "-of",
            "default=nw=1:nk=1",
            path.to_str().unwrap(),
        ])
        .output()?;
    if !output.status.success() {
        Err("ffprobe failed to inspect output".into())
    } else {
        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }
}

#[test]
fn nvenc_h264_profile_and_level_are_correct() -> Result<(), Box<dyn Error>> {
    if !nvenc_tests_enabled() {
        eprintln!("skipping NVENC integration test (set ENABLE_NVENC_TESTS=1 to enable)");
        return Ok(());
    }
    if !tool_available("ffmpeg", &["-version"]) || !tool_available("ffprobe", &["-version"]) {
        eprintln!("skipping NVENC integration test because ffmpeg/ffprobe not found");
        return Ok(());
    }
    if !ffmpeg_has_nvenc() {
        eprintln!("skipping NVENC integration test because h264_nvenc is unavailable");
        return Ok(());
    }

    let dir = tempdir()?;
    let input_path = dir.path().join("input.mkv");
    let output_path = dir.path().join("output.mp4");

    create_input_clip(&input_path)?;
    let (stdout_log, stderr_log) = run_nvenc_conversion(&input_path, &output_path)?;

    assert!(
        stdout_log.contains("Video encoder selected: hardware")
            || stderr_log.contains("Video encoder selected: hardware"),
        "expected hardware encoder selection, got stdout=\n{}\nstderr=\n{}",
        stdout_log,
        stderr_log
    );
    assert!(
        !stdout_log.contains("NVENC initialization failed")
            && !stderr_log.contains("NVENC initialization failed"),
        "NVENC initialization unexpectedly failed; stdout=\n{}\nstderr=\n{}",
        stdout_log,
        stderr_log
    );

    assert!(output_path.exists(), "expected output file to exist");

    let probe = probe_output(&output_path)?;
    let mut codec_name = None;
    let mut profile = None;
    let mut level = None;
    let mut encoder_tag = None;

    for line in probe.lines() {
        if let Some(v) = line.strip_prefix("codec_name=") {
            codec_name = Some(v.trim().to_string());
        } else if let Some(v) = line.strip_prefix("profile=") {
            profile = Some(v.trim().to_string());
        } else if let Some(v) = line.strip_prefix("level=") {
            level = Some(v.trim().to_string());
        } else if let Some(v) = line.strip_prefix("TAG:encoder=") {
            encoder_tag = Some(v.trim().to_string());
        }
    }

    let codec_name = codec_name.ok_or("missing codec_name from ffprobe")?;
    assert_eq!(codec_name, "h264", "unexpected codec name: {}\n{}", codec_name, probe);

    let profile = profile.ok_or("missing profile from ffprobe")?;
    assert!(
        profile.to_lowercase().contains("high"),
        "expected High profile, got {}.\n{}",
        profile,
        probe
    );

    let level_value = level.ok_or("missing level from ffprobe")?;
    let level_num: i32 = level_value.parse().unwrap_or(-1);
    assert!(
        level_num > 0 && level_num <= 41,
        "expected Level <= 41, got {}.\n{}",
        level_num,
        probe
    );

    let encoder_tag = encoder_tag.ok_or("missing encoder tag from ffprobe")?;
    assert!(
        encoder_tag.to_lowercase().contains("nvenc"),
        "expected NVENC encoder tag, got {}.\n{}",
        encoder_tag,
        probe
    );

    let metadata = fs::metadata(&output_path)?;
    assert!(metadata.len() > 0, "output file is empty");

    Ok(())
}

