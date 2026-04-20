# OCR Benchmark Runbook

This runbook captures the exact workflow for reproducible OCR stress
benchmarks on the Plex server, including non-default ONNX Runtime/CUDA
library wiring.

## 1) Build the benchmark binary

```bash
cd /home/mkusper/git/direct-play-nice
git rev-parse --abbrev-ref HEAD
git rev-parse --short HEAD

# Build release binary from the current branch.
cargo build --release

# Copy to a stable benchmark path (optional but recommended).
cp -f target/release/direct_play_nice /home/mkusper/dpn_tmp/direct_play_nice_bench
chmod +x /home/mkusper/dpn_tmp/direct_play_nice_bench
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
RUN_DIR=/mnt/data2/benchmarks/direct_play_nice_ocr/full_$(date +%Y%m%d_%H%M%S)_gpu_ort116_hifi
SRC="/mnt/data2/movies/Silence (2016)/Silence (2016) Remux-1080p.mkv"
BIN="/home/mkusper/dpn_tmp/direct_play_nice_bench"

/home/mkusper/dpn_tmp/run_ocr_stress_benchmark.sh \
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
