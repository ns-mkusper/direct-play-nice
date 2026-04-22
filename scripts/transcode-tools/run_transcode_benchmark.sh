#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage:
  run_transcode_benchmark.sh \
    --bin /path/to/direct_play_nice \
    --source /path/to/source.mkv \
    --run-dir /path/to/run_dir \
    [--hw-accel nvenc] \
    [--video-quality 1080p] \
    [--sub-mode skip] \
    [--sample-ms 200] \
    [--cpu-output cpu.mp4] \
    [--hw-output hw.mp4]

Notes:
  - Executes two benchmark passes on the same input:
      1) CPU baseline (--hw-accel none)
      2) Hardware attempt (--hw-accel <value>)
  - Captures logs, optional GPU telemetry, and summary artifacts:
      meta.txt
      cpu_command.txt
      hw_command.txt
      cpu.log
      hw.log
      gpu_smi.csv
      benchmark_summary.json
      benchmark_summary.md
USAGE
}

BIN=""
SOURCE=""
RUN_DIR=""
HW_ACCEL="nvenc"
VIDEO_QUALITY="1080p"
SUB_MODE="skip"
SAMPLE_MS="200"
CPU_OUTPUT_NAME="cpu.mp4"
HW_OUTPUT_NAME="hw.mp4"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --bin)
      BIN="$2"; shift 2 ;;
    --source)
      SOURCE="$2"; shift 2 ;;
    --run-dir)
      RUN_DIR="$2"; shift 2 ;;
    --hw-accel)
      HW_ACCEL="$2"; shift 2 ;;
    --video-quality)
      VIDEO_QUALITY="$2"; shift 2 ;;
    --sub-mode)
      SUB_MODE="$2"; shift 2 ;;
    --sample-ms)
      SAMPLE_MS="$2"; shift 2 ;;
    --cpu-output)
      CPU_OUTPUT_NAME="$2"; shift 2 ;;
    --hw-output)
      HW_OUTPUT_NAME="$2"; shift 2 ;;
    --help|-h)
      usage; exit 0 ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      exit 2 ;;
  esac
done

if [[ -z "$BIN" || -z "$SOURCE" || -z "$RUN_DIR" ]]; then
  echo "Missing required arguments." >&2
  usage
  exit 2
fi
if [[ ! -x "$BIN" ]]; then
  echo "Binary is not executable: $BIN" >&2
  exit 2
fi
if [[ ! -f "$SOURCE" ]]; then
  echo "Source file not found: $SOURCE" >&2
  exit 2
fi
if ! [[ "$SAMPLE_MS" =~ ^[0-9]+$ ]] || (( SAMPLE_MS < 50 )); then
  echo "--sample-ms must be an integer >= 50" >&2
  exit 2
fi
if ! command -v ffprobe >/dev/null 2>&1; then
  echo "ffprobe is required on PATH" >&2
  exit 2
fi

mkdir -p "$RUN_DIR"

CPU_OUT="$RUN_DIR/$CPU_OUTPUT_NAME"
HW_OUT="$RUN_DIR/$HW_OUTPUT_NAME"
CPU_LOG="$RUN_DIR/cpu.log"
HW_LOG="$RUN_DIR/hw.log"
GPU_SMI="$RUN_DIR/gpu_smi.csv"
META="$RUN_DIR/meta.txt"
CPU_CMD="$RUN_DIR/cpu_command.txt"
HW_CMD="$RUN_DIR/hw_command.txt"
SUMMARY_JSON="$RUN_DIR/benchmark_summary.json"
SUMMARY_MD="$RUN_DIR/benchmark_summary.md"

echo "$BIN --hw-accel none --video-quality $VIDEO_QUALITY --sub-mode $SUB_MODE --skip-codec-check '$SOURCE' '$CPU_OUT'" > "$CPU_CMD"
echo "$BIN --hw-accel $HW_ACCEL --video-quality $VIDEO_QUALITY --sub-mode $SUB_MODE --skip-codec-check '$SOURCE' '$HW_OUT'" > "$HW_CMD"

{
  echo "START_HUMAN=$(date -Is)"
  echo "BIN=$BIN"
  echo "SOURCE_BASENAME=$(basename "$SOURCE")"
  echo "VIDEO_QUALITY=$VIDEO_QUALITY"
  echo "SUB_MODE=$SUB_MODE"
  echo "HW_ACCEL=$HW_ACCEL"
  echo "SAMPLE_MS=$SAMPLE_MS"
} > "$META"

start_ts=$(date +%s)
echo "START_TS=$start_ts" >> "$META"

# Pass 1: CPU baseline
cpu_start=$(date +%s.%N)
"$BIN" --hw-accel none --video-quality "$VIDEO_QUALITY" --sub-mode "$SUB_MODE" --skip-codec-check "$SOURCE" "$CPU_OUT" > "$CPU_LOG" 2>&1
cpu_end=$(date +%s.%N)

# Pass 2: Hardware attempt with optional GPU telemetry
smi_pid=""
if command -v nvidia-smi >/dev/null 2>&1; then
  (
    nvidia-smi \
      --query-gpu=timestamp,index,utilization.gpu,utilization.memory,memory.used,power.draw,temperature.gpu \
      --format=csv,noheader,nounits \
      -lms "$SAMPLE_MS" > "$GPU_SMI"
  ) &
  smi_pid="$!"
fi

hw_start=$(date +%s.%N)
set +e
"$BIN" --hw-accel "$HW_ACCEL" --video-quality "$VIDEO_QUALITY" --sub-mode "$SUB_MODE" --skip-codec-check "$SOURCE" "$HW_OUT" > "$HW_LOG" 2>&1
hw_status="$?"
set -e
hw_end=$(date +%s.%N)

if [[ -n "$smi_pid" ]]; then
  kill "$smi_pid" 2>/dev/null || true
  wait "$smi_pid" 2>/dev/null || true
fi

end_ts=$(date +%s)
{
  echo "END_TS=$end_ts"
  echo "END_HUMAN=$(date -Is)"
  echo "HW_EXIT_STATUS=$hw_status"
} >> "$META"

python3 - <<'PY' "$SOURCE" "$CPU_OUT" "$HW_OUT" "$CPU_LOG" "$HW_LOG" "$GPU_SMI" "$SUMMARY_JSON" "$SUMMARY_MD" "$META" "$cpu_start" "$cpu_end" "$hw_start" "$hw_end"
import csv
import json
import subprocess
import sys
from pathlib import Path

source = Path(sys.argv[1])
cpu_out = Path(sys.argv[2])
hw_out = Path(sys.argv[3])
cpu_log = Path(sys.argv[4])
hw_log = Path(sys.argv[5])
gpu_smi = Path(sys.argv[6])
summary_json = Path(sys.argv[7])
summary_md = Path(sys.argv[8])
meta_path = Path(sys.argv[9])
cpu_start = float(sys.argv[10])
cpu_end = float(sys.argv[11])
hw_start = float(sys.argv[12])
hw_end = float(sys.argv[13])

meta = {}
for line in meta_path.read_text().splitlines():
    if "=" in line:
        k, v = line.split("=", 1)
        meta[k] = v

def ffprobe_value(args):
    return subprocess.check_output(args, text=True).strip()

def duration(path: Path) -> float:
    return float(
        ffprobe_value([
            "ffprobe", "-v", "error", "-show_entries", "format=duration",
            "-of", "default=nw=1:nk=1", str(path),
        ])
    )

def packet_count(path: Path) -> int:
    return int(
        ffprobe_value([
            "ffprobe", "-v", "error", "-select_streams", "v:0", "-count_packets",
            "-show_entries", "stream=nb_read_packets", "-of", "default=nw=1:nk=1", str(path),
        ])
    )

def encoder_tag(path: Path) -> str:
    try:
        return ffprobe_value([
            "ffprobe", "-v", "error", "-select_streams", "v:0",
            "-show_entries", "stream_tags=encoder",
            "-of", "default=nw=1:nk=1", str(path),
        ])
    except Exception:
        return ""

def parse_selected_encoder(log_path: Path) -> str:
    if not log_path.exists():
        return ""
    for line in log_path.read_text(errors="ignore").splitlines():
        if "Video encoder selected:" in line:
            return line.split("Video encoder selected:", 1)[1].strip()
    return ""

def hw_fallback_detected(log_path: Path) -> bool:
    if not log_path.exists():
        return False
    text = log_path.read_text(errors="ignore")
    return "retrying with software encoder" in text or "hardware disabled" in text

in_dur = duration(source)
cpu_elapsed = cpu_end - cpu_start
hw_elapsed = hw_end - hw_start

cpu_dur = duration(cpu_out) if cpu_out.exists() else 0.0
hw_dur = duration(hw_out) if hw_out.exists() else 0.0

cpu_packets = packet_count(cpu_out) if cpu_out.exists() else 0
hw_packets = packet_count(hw_out) if hw_out.exists() else 0

cpu_fps = (cpu_packets / cpu_elapsed) if cpu_elapsed > 0 else 0.0
hw_fps = (hw_packets / hw_elapsed) if hw_elapsed > 0 else 0.0

cpu_rt = (in_dur / cpu_elapsed) if cpu_elapsed > 0 else 0.0
hw_rt = (in_dur / hw_elapsed) if hw_elapsed > 0 else 0.0

speedup = (hw_elapsed and cpu_elapsed / hw_elapsed) or 0.0

gpu = {}
if gpu_smi.exists():
    with gpu_smi.open() as f:
        reader = csv.reader(f, skipinitialspace=True)
        for row in reader:
            if len(row) < 7:
                continue
            idx = int(row[1])
            util = float(row[2])
            mem = float(row[4])
            power = float(row[5])
            temp = float(row[6])
            stats = gpu.setdefault(idx, {"samples": 0, "util": [], "mem": [], "power": [], "temp": []})
            stats["samples"] += 1
            stats["util"].append(util)
            stats["mem"].append(mem)
            stats["power"].append(power)
            stats["temp"].append(temp)

for idx, stats in gpu.items():
    for k in ("util", "mem", "power", "temp"):
        values = stats[k]
        stats[f"avg_{k}"] = round(sum(values) / len(values), 2) if values else 0.0

cpu_selected = parse_selected_encoder(cpu_log)
hw_selected = parse_selected_encoder(hw_log)

summary = {
    "meta": meta,
    "input_duration_sec": round(in_dur, 3),
    "cpu": {
        "elapsed_sec": round(cpu_elapsed, 3),
        "output_duration_sec": round(cpu_dur, 3),
        "video_packets": cpu_packets,
        "effective_fps": round(cpu_fps, 2),
        "realtime_factor": round(cpu_rt, 2),
        "selected_encoder": cpu_selected,
        "encoder_tag": encoder_tag(cpu_out),
    },
    "hardware": {
        "elapsed_sec": round(hw_elapsed, 3),
        "output_duration_sec": round(hw_dur, 3),
        "video_packets": hw_packets,
        "effective_fps": round(hw_fps, 2),
        "realtime_factor": round(hw_rt, 2),
        "selected_encoder": hw_selected,
        "encoder_tag": encoder_tag(hw_out),
        "fallback_to_software_detected": hw_fallback_detected(hw_log),
        "hw_exit_status": int(meta.get("HW_EXIT_STATUS", "1")),
    },
    "comparison": {
        "hw_vs_cpu_speedup": round(speedup, 3),
        "lower_elapsed_is_better": True,
    },
    "gpu_telemetry": gpu,
}

summary_json.write_text(json.dumps(summary, indent=2) + "\n")

def tf(x):
    return f"{x:.2f}"

lines = []
lines.append("# Transcoding Benchmark (Sanitized)")
lines.append("")
lines.append("## Scope")
lines.append("")
lines.append("CPU baseline vs hardware-accel transcode on the same input file.")
lines.append("")
lines.append(f"- Input basename: `{source.name}`")
lines.append(f"- Input duration: `{in_dur:.3f}s`")
lines.append(f"- Requested hardware mode: `{meta.get('HW_ACCEL', '')}`")
lines.append(f"- Video quality preset: `{meta.get('VIDEO_QUALITY', '')}`")
lines.append("")
lines.append("## Final run summary")
lines.append("")
lines.append(f"- CPU elapsed: `{cpu_elapsed:.3f}s` ({cpu_rt:.2f}x realtime)")
lines.append(f"- HW elapsed: `{hw_elapsed:.3f}s` ({hw_rt:.2f}x realtime)")
lines.append(f"- HW/CPU speedup: `{speedup:.3f}x` (lower elapsed is better)")
lines.append("")
lines.append("## Metrics")
lines.append("")
lines.append("| Metric | CPU baseline | Hardware attempt |")
lines.append("| --- | ---: | ---: |")
lines.append(f"| Elapsed (s) | `{cpu_elapsed:.3f}` | `{hw_elapsed:.3f}` |")
lines.append(f"| Effective FPS | `{cpu_fps:.2f}` | `{hw_fps:.2f}` |")
lines.append(f"| Realtime factor | `{cpu_rt:.2f}x` | `{hw_rt:.2f}x` |")
lines.append(f"| Selected encoder | `{cpu_selected or 'unknown'}` | `{hw_selected or 'unknown'}` |")
lines.append(f"| Encoder tag | `{encoder_tag(cpu_out) or 'unknown'}` | `{encoder_tag(hw_out) or 'unknown'}` |")
lines.append("")

if gpu:
    lines.append("## GPU telemetry (hardware pass)")
    lines.append("")
    lines.append("| GPU | Avg util % | Avg mem MiB | Avg power W | Avg temp C | Samples |")
    lines.append("| --- | ---: | ---: | ---: | ---: | ---: |")
    for idx in sorted(gpu):
        s = gpu[idx]
        lines.append(
            f"| {idx} | `{tf(s['avg_util'])}` | `{tf(s['avg_mem'])}` | `{tf(s['avg_power'])}` | `{tf(s['avg_temp'])}` | `{s['samples']}` |"
        )
    lines.append("")

if summary["hardware"]["fallback_to_software_detected"]:
    lines.append("## Note")
    lines.append("")
    lines.append("Hardware run log indicates fallback to software encoder.")
    lines.append("")

lines.append("## Artifacts")
lines.append("")
lines.append("- `cpu.log`")
lines.append("- `hw.log`")
lines.append("- `gpu_smi.csv` (if NVIDIA telemetry was available)")
lines.append("- `benchmark_summary.json`")

summary_md.write_text("\n".join(lines) + "\n")
PY

echo "Wrote: $SUMMARY_JSON"
echo "Wrote: $SUMMARY_MD"
