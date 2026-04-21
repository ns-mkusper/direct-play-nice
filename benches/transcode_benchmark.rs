use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::env;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use tempfile::TempDir;

fn resolve_binary() -> Option<PathBuf> {
    if let Ok(bin) = env::var("DPN_TRANSCODE_BENCH_BIN") {
        let path = PathBuf::from(bin);
        if path.is_file() {
            return Some(path);
        }
    }

    let mut candidates = Vec::new();
    if let Ok(cwd) = env::current_dir() {
        candidates.push(cwd.join("target").join("release").join("direct_play_nice"));
        candidates.push(
            cwd.join("target")
                .join("release")
                .join("direct_play_nice.exe"),
        );
    }

    candidates.into_iter().find(|p| p.is_file())
}

fn command_available(cmd: &str, args: &[&str]) -> bool {
    Command::new(cmd)
        .args(args)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

fn generate_input_clip(dir: &Path) -> Option<PathBuf> {
    let input = dir.join("transcode_input.mkv");
    let status = Command::new("ffmpeg")
        .args([
            "-hide_banner",
            "-y",
            "-f",
            "lavfi",
            "-i",
            "testsrc2=size=1280x720:rate=30000/1001:duration=2",
            "-f",
            "lavfi",
            "-i",
            "sine=frequency=1000:sample_rate=48000:duration=2",
            "-c:v",
            "mpeg2video",
            "-pix_fmt",
            "yuv420p",
            "-b:v",
            "10M",
            "-maxrate",
            "10M",
            "-bufsize",
            "20M",
            "-c:a",
            "mp2",
            "-b:a",
            "320k",
        ])
        .arg(input.as_os_str())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .ok()?;

    if status.success() {
        Some(input)
    } else {
        None
    }
}

fn run_transcode(bin: &Path, input: &Path, output: &Path) -> bool {
    Command::new(bin)
        .args([
            "--hw-accel",
            "none",
            "--video-quality",
            "720p",
            "--skip-codec-check",
        ])
        .arg(input.as_os_str())
        .arg(output.as_os_str())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

fn bench_transcode_pipeline(c: &mut Criterion) {
    let Some(bin) = resolve_binary() else {
        eprintln!("Skipping transcode benchmark: direct_play_nice binary not found.");
        return;
    };
    if !command_available("ffmpeg", &["-version"]) {
        eprintln!("Skipping transcode benchmark: ffmpeg not found.");
        return;
    }
    if !command_available("ffprobe", &["-version"]) {
        eprintln!("Skipping transcode benchmark: ffprobe not found.");
        return;
    }

    let temp = TempDir::new().expect("create benchmark temp dir");
    let Some(input) = generate_input_clip(temp.path()) else {
        eprintln!("Skipping transcode benchmark: failed to generate input clip.");
        return;
    };

    let run_idx = AtomicUsize::new(0);
    let mut group = c.benchmark_group("Transcode Conversion Speed");
    group.sample_size(10);
    group.bench_function("cpu_transcode_720p", |b| {
        b.iter(|| {
            let idx = run_idx.fetch_add(1, Ordering::Relaxed);
            let output = temp.path().join(format!("out_{idx}.mp4"));
            let ok = run_transcode(&bin, &input, &output);
            assert!(ok, "transcode benchmark iteration failed");
            let _ = black_box(output);
        })
    });
    group.finish();
}

criterion_group!(benches, bench_transcode_pipeline);
criterion_main!(benches);
