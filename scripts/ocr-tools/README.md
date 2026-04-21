# OCR Benchmark Runbook

This runbook captures the exact workflow for reproducible OCR stress
benchmarks on a self-hosted media server, including non-default ONNX
Runtime/CUDA library wiring.

Use these variables to keep runs portable across hosts:

```bash
REPO_DIR=/path/to/direct-play-nice
WORK_DIR=/path/to/workdir
BIN="$WORK_DIR/direct_play_nice_bench"
SRC=/path/to/input_video.mkv
RUN_DIR=/path/to/benchmarks/full_$(date +%Y%m%d_%H%M%S)_gpu_ocr
```

## 1) Build the benchmark binary

```bash
cd "$REPO_DIR"
git rev-parse --abbrev-ref HEAD
git rev-parse --short HEAD

# Build release binary from the current branch.
cargo build --release

# Copy to a stable benchmark path (optional but recommended).
cp -f target/release/direct_play_nice "$BIN"
chmod +x "$BIN"
```

## 2) Prepare ONNX Runtime + cuDNN userspace stack (no pacman install)

If you already have these directories, skip this step.

```bash
# Expected ORT directory:
#   /opt/direct-play-nice/ort116-runtime/lib
#
# Expected cuDNN extraction directory:
#   /opt/direct-play-nice/cudnn8-runtime/usr/lib

sudo mkdir -p /opt/direct-play-nice/pkgcache /opt/direct-play-nice/cudnn8-runtime

sudo curl -L --fail --retry 3 \
  -o /opt/direct-play-nice/pkgcache/cudnn-8.2.4.15-1-x86_64.pkg.tar.zst \
  https://archive.archlinux.org/packages/c/cudnn/cudnn-8.2.4.15-1-x86_64.pkg.tar.zst

sudo bsdtar -xf /opt/direct-play-nice/pkgcache/cudnn-8.2.4.15-1-x86_64.pkg.tar.zst \
  -C /opt/direct-play-nice/cudnn8-runtime
```

## 3) Run high-fidelity OCR stress benchmark

```bash
"$REPO_DIR/scripts/ocr-tools/run_ocr_stress_benchmark.sh" \
  --bin "$BIN" \
  --source "$SRC" \
  --run-dir "$RUN_DIR" \
  --output-name out.mp4 \
  --ocr-engine pp-ocr-v3 \
  --sub-mode force \
  --sample-ms 200 \
  --ocr-max-jobs 8 \
  --jobs-per-gpu 2 \
  --cuda-devices 0,1 \
  --require-gpu \
  --ort-lib /opt/direct-play-nice/ort116-runtime/lib \
  --env "LD_LIBRARY_PATH=/opt/direct-play-nice/cudnn8-runtime/usr/lib:/opt/direct-play-nice/ort116-runtime/lib:${LD_LIBRARY_PATH:-}" \
  --env "DPN_OCR_ALLOW_LEGACY_CUDA=1"
```

### Notes for Plex-like hosts

- Keep benchmark artifacts on a large non-root volume.
- Keep model/runtime directories in a stable absolute path.
- Keep `--run-dir` and `--source` configurable per host.

## 4) Artifacts to collect per run

The benchmark script writes these files under `RUN_DIR`:

- `command.txt`: exact invoked command
- `meta.txt`: start/end timestamps and status
- `run.log`: full program log
- `gpu_smi.csv`: 200ms GPU telemetry samples
- `benchmark_summary.json`: machine-readable metrics
- `benchmark_summary.md`: human summary

## 5) Quick sanity checks

```bash
grep -E "OCR worker plan|Running OCR with|OCR subtitle stream" "$RUN_DIR/run.log"
grep -c "quality fallback: using Tesseract" "$RUN_DIR/run.log"
grep -c "language fallback: using Tesseract" "$RUN_DIR/run.log"
sed -n '1,120p' "$RUN_DIR/benchmark_summary.md"
```
