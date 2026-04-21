#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  run_onnx_tuning_matrix.sh \
    --bin /path/to/direct_play_nice \
    --source /path/to/source.mkv \
    --base-dir /path/to/benchmarks \
    [--ort-lib /path/to/onnxruntime/lib] \
    [--sample-ms 100] \
    [--ocr-max-jobs 8] \
    [--jobs-per-gpu 1] \
    [--cuda-devices 0,1]

Runs three sequential OCR stress benchmarks:
  A) defaults
  B) stricter Tesseract replacement gain
  C) pure ONNX (disable Tesseract fallback)

Writes:
  <base-dir>/matrix_<timestamp>/matrix_summary.md
EOF
}

BIN=""
SOURCE=""
BASE_DIR=""
ORT_LIB=""
SAMPLE_MS="100"
OCR_MAX_JOBS="8"
JOBS_PER_GPU="1"
CUDA_DEVICES="0,1"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --bin) BIN="$2"; shift 2 ;;
    --source) SOURCE="$2"; shift 2 ;;
    --base-dir) BASE_DIR="$2"; shift 2 ;;
    --ort-lib) ORT_LIB="$2"; shift 2 ;;
    --sample-ms) SAMPLE_MS="$2"; shift 2 ;;
    --ocr-max-jobs) OCR_MAX_JOBS="$2"; shift 2 ;;
    --jobs-per-gpu) JOBS_PER_GPU="$2"; shift 2 ;;
    --cuda-devices) CUDA_DEVICES="$2"; shift 2 ;;
    --help|-h) usage; exit 0 ;;
    *) echo "Unknown arg: $1" >&2; usage; exit 2 ;;
  esac
done

if [[ -z "$BIN" || -z "$SOURCE" || -z "$BASE_DIR" ]]; then
  echo "Missing required args." >&2
  usage
  exit 2
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RUNNER="$SCRIPT_DIR/run_ocr_stress_benchmark.sh"
if [[ ! -x "$RUNNER" ]]; then
  echo "Runner script not executable: $RUNNER" >&2
  exit 2
fi

TS="$(date +%Y%m%d_%H%M%S)"
MATRIX_DIR="$BASE_DIR/matrix_${TS}"
mkdir -p "$MATRIX_DIR"

run_case() {
  local label="$1"
  shift
  local run_dir="$MATRIX_DIR/$label"
  mkdir -p "$run_dir"
  echo "== running $label =="
  "$RUNNER" \
    --bin "$BIN" \
    --source "$SOURCE" \
    --run-dir "$run_dir" \
    --sample-ms "$SAMPLE_MS" \
    --ocr-max-jobs "$OCR_MAX_JOBS" \
    --jobs-per-gpu "$JOBS_PER_GPU" \
    --cuda-devices "$CUDA_DEVICES" \
    ${ORT_LIB:+--ort-lib "$ORT_LIB"} \
    "$@"
}

set +e
run_case "A_defaults"
status_a=$?
run_case "B_strict_gain" --env "DPN_OCR_TESS_FALLBACK_MIN_GAIN=0.12"
status_b=$?
run_case "C_pure_onnx" --env "DPN_OCR_DISABLE_TESS_FALLBACK=1"
status_c=$?
set -e

python3 - <<'PY' "$MATRIX_DIR" "$status_a" "$status_b" "$status_c"
import json
import sys
from pathlib import Path

matrix_dir = Path(sys.argv[1])
statuses = {"A_defaults": int(sys.argv[2]), "B_strict_gain": int(sys.argv[3]), "C_pure_onnx": int(sys.argv[4])}

rows = []
for label in ["A_defaults", "B_strict_gain", "C_pure_onnx"]:
    summary_path = matrix_dir / label / "benchmark_summary.json"
    if summary_path.exists():
        data = json.loads(summary_path.read_text())
        rows.append({
            "label": label,
            "status": data.get("exit_status"),
            "elapsed": data.get("elapsed_sec"),
            "rt": data.get("realtime_factor"),
            "fps": data.get("effective_fps"),
            "qfb": data.get("quality_fallback_count"),
            "lfb": data.get("language_fallback_count"),
            "gpu0": data.get("gpu", {}).get("gpu0", {}).get("avg_util_pct"),
            "gpu1": data.get("gpu", {}).get("gpu1", {}).get("avg_util_pct"),
        })
    else:
        rows.append({
            "label": label,
            "status": statuses[label],
            "elapsed": None,
            "rt": None,
            "fps": None,
            "qfb": None,
            "lfb": None,
            "gpu0": None,
            "gpu1": None,
        })

md = []
md.append("# ONNX OCR Tuning Matrix")
md.append("")
md.append("| Case | Exit | Elapsed (s) | RTx | FPS | Quality FB | Language FB | GPU0 avg util | GPU1 avg util |")
md.append("| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |")
for r in rows:
    elapsed = "" if r["elapsed"] is None else "{:.0f}".format(r["elapsed"])
    rtx = "" if r["rt"] is None else "{:.2f}".format(r["rt"])
    fps = "" if r["fps"] is None else "{:.2f}".format(r["fps"])
    qfb = "" if r["qfb"] is None else str(r["qfb"])
    lfb = "" if r["lfb"] is None else str(r["lfb"])
    gpu0 = "" if r["gpu0"] is None else "{:.2f}%".format(r["gpu0"])
    gpu1 = "" if r["gpu1"] is None else "{:.2f}%".format(r["gpu1"])
    md.append(
        "| {} | {} | {} | {} | {} | {} | {} | {} | {} |".format(
            r["label"], r["status"], elapsed, rtx, fps, qfb, lfb, gpu0, gpu1
        )
    )
md.append("")
md.append("Individual run folders contain full logs and high-frequency GPU telemetry.")
(matrix_dir / "matrix_summary.md").write_text("\n".join(md) + "\n")
print((matrix_dir / "matrix_summary.md").as_posix())
PY

echo "Matrix complete: $MATRIX_DIR"
