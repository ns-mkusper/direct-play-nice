use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::env;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use tempfile::TempDir;

fn resolve_binary() -> Option<PathBuf> {
    if let Ok(bin) = env::var("DPN_RESIZE_BENCH_BIN") {
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

fn generate_low_res_clip(dir: &Path) -> Option<PathBuf> {
    let input = dir.join("resize_input.mkv");
    let status = Command::new("ffmpeg")
        .args([
            "-hide_banner",
            "-y",
            "-f",
            "lavfi",
            "-i",
            "testsrc2=size=640x360:rate=30000/1001:duration=2",
            "-f",
            "lavfi",
            "-i",
            "sine=frequency=1000:sample_rate=48000:duration=2",
            "-c:v",
            "mpeg2video",
            "-pix_fmt",
            "yuv420p",
            "-b:v",
            "4M",
            "-c:a",
            "mp2",
            "-b:a",
            "192k",
            "-shortest",
        ])
        .arg(input.as_os_str())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .ok()?;

    status.success().then_some(input)
}

fn run_resize(bin: &Path, input: &Path, output: &Path, quality: &str) -> bool {
    Command::new(bin)
        .args([
            "--device",
            "chromecast",
            "--hw-accel",
            "none",
            "--video-quality",
            "720p",
            "--resize-quality",
            quality,
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

fn bench_resize_pipeline(c: &mut Criterion) {
    let Some(bin) = resolve_binary() else {
        eprintln!("Skipping resize benchmark: direct_play_nice binary not found.");
        return;
    };
    if !command_available("ffmpeg", &["-version"]) {
        eprintln!("Skipping resize benchmark: ffmpeg not found.");
        return;
    }

    let temp = TempDir::new().expect("create benchmark temp dir");
    let Some(input) = generate_low_res_clip(temp.path()) else {
        eprintln!("Skipping resize benchmark: failed to generate input clip.");
        return;
    };

    let run_idx = AtomicUsize::new(0);
    let mut group = c.benchmark_group("Resize Conversion Speed");
    group.sample_size(10);
    for quality in ["fast-bilinear", "lanczos", "spline"] {
        group.bench_function(format!("cpu_resize_720p_{quality}"), |b| {
            b.iter(|| {
                let idx = run_idx.fetch_add(1, Ordering::Relaxed);
                let output = temp.path().join(format!("resized_{quality}_{idx}.mp4"));
                let ok = run_resize(&bin, &input, &output, quality);
                assert!(ok, "resize benchmark iteration failed");
                let _ = black_box(output);
            })
        });
    }
    group.finish();
}

criterion_group!(benches, bench_resize_pipeline);
criterion_main!(benches);
