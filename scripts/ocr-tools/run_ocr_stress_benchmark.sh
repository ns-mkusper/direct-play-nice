#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  run_ocr_stress_benchmark.sh \
    --bin /path/to/direct_play_nice \
    --source /path/to/source.mkv \
    --run-dir /path/to/run_dir \
    [--output-name output.mp4] \
    [--ort-lib /path/to/onnxruntime/lib] \
    [--ocr-engine pp-ocr-v3] \
    [--ocr-preprocess none|open-cv-basic|open-cv-subtitle|open-cv5-cuda-basic|open-cv5-cuda-subtitle] \
    [--sub-mode force] \
    [--max-source-seconds 600] \
    [--sample-ms 200] \
    [--ocr-max-jobs 8] \
    [--jobs-per-gpu 1] \
    [--cuda-devices 0,1] \
    [--require-gpu] \
    [--min-ocr-space-rate 0.12] \
    [--]

Notes:
  - Paths are fully parameterized; nothing is hardcoded.
  - Captures high-frequency GPU telemetry via nvidia-smi (-lms).
  - Writes summary artifacts:
      meta.txt
      command.txt
      run.log
      gpu_smi.csv
      benchmark_summary.json
      benchmark_summary.md
EOF
}

BIN=""
SOURCE=""
RUN_DIR=""
OUTPUT_NAME="output.mp4"
ORT_LIB=""
OCR_ENGINE="pp-ocr-v3"
OCR_PREPROCESS="none"
SUB_MODE="force"
MAX_SOURCE_SECONDS=""
SAMPLE_MS="200"
OCR_MAX_JOBS=""
JOBS_PER_GPU=""
CUDA_DEVICES=""
REQUIRE_GPU="0"
MIN_OCR_SPACE_RATE=""
EXTRA_ENVS=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --bin)
      BIN="$2"; shift 2 ;;
    --source)
      SOURCE="$2"; shift 2 ;;
    --run-dir)
      RUN_DIR="$2"; shift 2 ;;
    --output-name)
      OUTPUT_NAME="$2"; shift 2 ;;
    --ort-lib)
      ORT_LIB="$2"; shift 2 ;;
    --ocr-engine)
      OCR_ENGINE="$2"; shift 2 ;;
    --ocr-preprocess)
      OCR_PREPROCESS="$2"; shift 2 ;;
    --sub-mode)
      SUB_MODE="$2"; shift 2 ;;
    --max-source-seconds)
      MAX_SOURCE_SECONDS="$2"; shift 2 ;;
    --sample-ms)
      SAMPLE_MS="$2"; shift 2 ;;
    --ocr-max-jobs)
      OCR_MAX_JOBS="$2"; shift 2 ;;
    --jobs-per-gpu)
      JOBS_PER_GPU="$2"; shift 2 ;;
    --cuda-devices)
      CUDA_DEVICES="$2"; shift 2 ;;
    --require-gpu)
      REQUIRE_GPU="1"; shift ;;
    --min-ocr-space-rate)
      MIN_OCR_SPACE_RATE="$2"; shift 2 ;;
    --env)
      EXTRA_ENVS+=("$2"); shift 2 ;;
    --help|-h)
      usage; exit 0 ;;
    --)
      shift
      break ;;
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
if ! [[ "$SAMPLE_MS" =~ ^[0-9]+$ ]] || [[ "$SAMPLE_MS" -lt 50 ]]; then
  echo "--sample-ms must be an integer >= 50" >&2
  exit 2
fi
if [[ -n "$MAX_SOURCE_SECONDS" ]]; then
  if ! [[ "$MAX_SOURCE_SECONDS" =~ ^[0-9]+$ ]] || (( MAX_SOURCE_SECONDS < 30 )); then
    echo "--max-source-seconds must be an integer >= 30" >&2
    exit 2
  fi
fi
if [[ -n "$MIN_OCR_SPACE_RATE" ]]; then
  if ! [[ "$MIN_OCR_SPACE_RATE" =~ ^0(\.[0-9]+)?$|^1(\.0+)?$ ]]; then
    echo "--min-ocr-space-rate must be a decimal between 0 and 1" >&2
    exit 2
  fi
fi

mkdir -p "$RUN_DIR"

OUT="$RUN_DIR/$OUTPUT_NAME"
LOG="$RUN_DIR/run.log"
SMI="$RUN_DIR/gpu_smi.csv"
META="$RUN_DIR/meta.txt"
CMD_TXT="$RUN_DIR/command.txt"
SUMMARY_JSON="$RUN_DIR/benchmark_summary.json"
SUMMARY_MD="$RUN_DIR/benchmark_summary.md"
TRIMMED_SOURCE="$RUN_DIR/source_trimmed.mkv"

OCR_PREPROCESS_ARGS=()
if [[ -n "$OCR_PREPROCESS" ]]; then
  OCR_PREPROCESS_ARGS=(--ocr-preprocess "$OCR_PREPROCESS")
fi

BENCH_SOURCE="$SOURCE"
if [[ -n "$MAX_SOURCE_SECONDS" ]]; then
  ffmpeg -hide_banner -loglevel error -y \
    -ss 0 -i "$SOURCE" \
    -t "$MAX_SOURCE_SECONDS" \
    -map 0 -c copy "$TRIMMED_SOURCE"
  BENCH_SOURCE="$TRIMMED_SOURCE"
fi

printf '%q ' \
  "$BIN" \
  --ocr-engine "$OCR_ENGINE" \
  "${OCR_PREPROCESS_ARGS[@]}" \
  --sub-mode "$SUB_MODE" \
  --skip-codec-check \
  --delete-source=false \
  "$BENCH_SOURCE" "$OUT" >"$CMD_TXT"
printf '\n' >>"$CMD_TXT"

{
  echo "BIN=$BIN"
  echo "SOURCE=$SOURCE"
  echo "BENCH_SOURCE=$BENCH_SOURCE"
  echo "OUT=$OUT"
  echo "ORT_LIB=$ORT_LIB"
  echo "OCR_ENGINE=$OCR_ENGINE"
  echo "OCR_PREPROCESS=${OCR_PREPROCESS:-}"
  echo "SUB_MODE=$SUB_MODE"
  echo "OCR_MAX_JOBS=${OCR_MAX_JOBS:-}"
  echo "JOBS_PER_GPU=${JOBS_PER_GPU:-}"
  echo "CUDA_DEVICES=${CUDA_DEVICES:-}"
  echo "REQUIRE_GPU=$REQUIRE_GPU"
  echo "MAX_SOURCE_SECONDS=${MAX_SOURCE_SECONDS:-}"
  echo "SAMPLE_MS=$SAMPLE_MS"
  echo "MIN_OCR_SPACE_RATE=${MIN_OCR_SPACE_RATE:-}"
  echo "START_HUMAN=$(date -Is)"
} >"$META"

start_ts="$(date +%s)"
echo "START_TS=$start_ts" >>"$META"

(
  nvidia-smi \
    --query-gpu=timestamp,index,utilization.gpu,utilization.memory,memory.used,power.draw,temperature.gpu \
    --format=csv,noheader,nounits \
    -lms "$SAMPLE_MS" >"$SMI"
) &
SMI_PID="$!"

set +e
ENV_VARS=(
  "DPN_MAX_JOBS=1"
  "DIRECT_PLAY_NICE_LOCK_DIR=$RUN_DIR/locks"
  # Keep benchmark results focused on ONNX behavior unless the caller
  # explicitly overrides these policy toggles via --env.
  "DPN_OCR_FORCE_TESS_NON_ENGLISH=0"
  "DPN_OCR_DISABLE_TESS_FALLBACK=0"
)
if [[ -n "$OCR_MAX_JOBS" ]]; then
  ENV_VARS+=("DPN_OCR_MAX_JOBS=$OCR_MAX_JOBS")
fi
if [[ -n "$JOBS_PER_GPU" ]]; then
  ENV_VARS+=("DPN_OCR_JOBS_PER_GPU=$JOBS_PER_GPU")
fi
if [[ -n "$CUDA_DEVICES" ]]; then
  ENV_VARS+=("DPN_OCR_CUDA_DEVICES=$CUDA_DEVICES")
fi
if [[ "$REQUIRE_GPU" == "1" ]]; then
  ENV_VARS+=("DPN_OCR_REQUIRE_GPU=1")
fi
if [[ -n "$ORT_LIB" ]]; then
  ENV_VARS+=("LD_LIBRARY_PATH=$ORT_LIB:${LD_LIBRARY_PATH:-}")
  ORT_DYLIB="$ORT_LIB/libonnxruntime.so"
  if [[ ! -f "$ORT_DYLIB" ]]; then
    ORT_DYLIB="$(find "$ORT_LIB" -maxdepth 1 -type f -name 'libonnxruntime.so*' | head -n 1 || true)"
  fi
  if [[ -n "$ORT_DYLIB" && -f "$ORT_DYLIB" ]]; then
    ENV_VARS+=("ORT_DYLIB_PATH=$ORT_DYLIB")
  else
    echo "Warning: no ONNX Runtime library found under --ort-lib '$ORT_LIB'" >&2
  fi
fi
if [[ "${#EXTRA_ENVS[@]}" -gt 0 ]]; then
  ENV_VARS+=("${EXTRA_ENVS[@]}")
fi

resolve_effective_env_value() {
  local key="$1"
  local default="$2"
  local value="$default"
  local kv
  for kv in "${ENV_VARS[@]}"; do
    if [[ "$kv" == "$key="* ]]; then
      value="${kv#*=}"
    fi
  done
  printf '%s' "$value"
}

FORCE_TESS_NON_ENGLISH="$(resolve_effective_env_value "DPN_OCR_FORCE_TESS_NON_ENGLISH" "0")"
DISABLE_TESS_FALLBACK="$(resolve_effective_env_value "DPN_OCR_DISABLE_TESS_FALLBACK" "0")"
TESS_FALLBACK_MIN_GAIN="$(resolve_effective_env_value "DPN_OCR_TESS_FALLBACK_MIN_GAIN" "")"
{
  echo "FORCE_TESS_NON_ENGLISH=$FORCE_TESS_NON_ENGLISH"
  echo "DISABLE_TESS_FALLBACK=$DISABLE_TESS_FALLBACK"
  echo "TESS_FALLBACK_MIN_GAIN=$TESS_FALLBACK_MIN_GAIN"
} >>"$META"

env "${ENV_VARS[@]}" \
  "$BIN" \
  --ocr-engine "$OCR_ENGINE" \
  "${OCR_PREPROCESS_ARGS[@]}" \
  --sub-mode "$SUB_MODE" \
  --skip-codec-check \
  --delete-source=false \
  "$BENCH_SOURCE" "$OUT" >"$LOG" 2>&1
status="$?"
set -e

end_ts="$(date +%s)"
kill "$SMI_PID" 2>/dev/null || true
wait "$SMI_PID" 2>/dev/null || true

{
  echo "END_TS=$end_ts"
  echo "END_HUMAN=$(date -Is)"
  echo "ELAPSED_SEC=$((end_ts-start_ts))"
  echo "EXIT_STATUS=$status"
} >>"$META"

python3 - <<'PY' "$META" "$OUT" "$SMI" "$LOG" "$SUMMARY_JSON" "$SUMMARY_MD"
import csv
import json
import re
import statistics
import subprocess
import sys
from pathlib import Path

meta_path = Path(sys.argv[1])
out_path = Path(sys.argv[2])
gpu_path = Path(sys.argv[3])
log_path = Path(sys.argv[4])
summary_json = Path(sys.argv[5])
summary_md = Path(sys.argv[6])

meta = {}
for line in meta_path.read_text().splitlines():
    if "=" in line:
        k, v = line.split("=", 1)
        meta[k] = v

elapsed = float(meta.get("ELAPSED_SEC", "0") or 0)
exit_status = int(meta.get("EXIT_STATUS", "1") or 1)
out_bytes = out_path.stat().st_size if out_path.exists() else 0

def ffprobe_value(args):
    return subprocess.check_output(args, text=True).strip()

out_duration = 0.0
video_packets = 0
if out_path.exists():
    out_duration = float(
        ffprobe_value(
            [
                "ffprobe",
                "-v",
                "error",
                "-show_entries",
                "format=duration",
                "-of",
                "default=nw=1:nk=1",
                str(out_path),
            ]
        )
    )
    video_packets = int(
        ffprobe_value(
            [
                "ffprobe",
                "-v",
                "error",
                "-select_streams",
                "v:0",
                "-count_packets",
                "-show_entries",
                "stream=nb_read_packets",
                "-of",
                "default=nw=1:nk=1",
                str(out_path),
            ]
        )
    )

source_subtitle_streams = []
subtitle_codec_counts = {}
bitmap_codec_counts = {}
bitmap_codecs = {
    "hdmv_pgs_subtitle",
    "dvd_subtitle",
    "dvb_subtitle",
    "xsub",
}
bench_source = meta.get("BENCH_SOURCE")
if bench_source:
    try:
        ffprobe_json = subprocess.check_output(
            [
                "ffprobe",
                "-v",
                "error",
                "-select_streams",
                "s",
                "-show_entries",
                "stream=index,codec_name:stream_tags=language,title",
                "-of",
                "json",
                bench_source,
            ],
            text=True,
        )
        streams_obj = json.loads(ffprobe_json)
        for s in streams_obj.get("streams", []):
            codec = s.get("codec_name") or "unknown"
            stream_index = s.get("index")
            tags = s.get("tags") or {}
            language = tags.get("language") or "und"
            title = tags.get("title") or ""
            is_bitmap = codec in bitmap_codecs
            entry = {
                "index": stream_index,
                "codec": codec,
                "language": language,
                "title": title,
                "is_bitmap": is_bitmap,
            }
            source_subtitle_streams.append(entry)
            subtitle_codec_counts[codec] = subtitle_codec_counts.get(codec, 0) + 1
            if is_bitmap:
                bitmap_codec_counts[codec] = bitmap_codec_counts.get(codec, 0) + 1
    except Exception:
        pass

gpu = {}
if gpu_path.exists():
    with gpu_path.open() as f:
        reader = csv.reader(f, skipinitialspace=True)
        for row in reader:
            if len(row) < 7:
                continue
            idx = int(row[1])
            util = float(row[2])
            mem = float(row[4])
            power = float(row[5])
            temp = float(row[6])
            s = gpu.setdefault(
                idx, {"util": [], "power": [], "mem": [], "temp": [], "samples": 0}
            )
            s["samples"] += 1
            s["util"].append(util)
            s["power"].append(power)
            s["mem"].append(mem)
            s["temp"].append(temp)

gpu_summary = {}
for idx, s in sorted(gpu.items()):
    util_sorted = sorted(s["util"])
    p95_idx = max(0, int(len(util_sorted) * 0.95) - 1)
    gpu_summary[f"gpu{idx}"] = {
        "samples": s["samples"],
        "avg_util_pct": round(statistics.mean(s["util"]), 2),
        "p50_util_pct": round(statistics.median(s["util"]), 2),
        "p95_util_pct": round(util_sorted[p95_idx], 2),
        "peak_util_pct": round(max(s["util"]), 2),
        "avg_power_w": round(statistics.mean(s["power"]), 2),
        "peak_power_w": round(max(s["power"]), 2),
        "peak_vram_mib": round(max(s["mem"]), 2),
        "peak_temp_c": round(max(s["temp"]), 2),
    }

log_text = log_path.read_text(errors="replace") if log_path.exists() else ""
ocr_stream_lines = [
    ln for ln in log_text.splitlines() if "OCR subtitle stream" in ln and "cues" in ln
]
ocr_stream_cues = []
for ln in ocr_stream_lines:
    m = re.search(r"OCR subtitle stream\s+(\d+).*\((\d+)\s+cues", ln)
    if not m:
        continue
    ocr_stream_cues.append({"stream_index": int(m.group(1)), "cues": int(m.group(2))})
total_ocr_cues = sum(item["cues"] for item in ocr_stream_cues)

def extract_output_subtitle_text_metrics(path: Path):
    metrics = []
    if not path.exists():
        return metrics
    try:
        streams_obj = json.loads(
            subprocess.check_output(
                [
                    "ffprobe",
                    "-v",
                    "error",
                    "-select_streams",
                    "s",
                    "-show_entries",
                    "stream=index,codec_name:stream_tags=language,title",
                    "-of",
                    "json",
                    str(path),
                ],
                text=True,
            )
        )
    except Exception:
        return metrics
    for ordinal, stream in enumerate(streams_obj.get("streams", [])):
        codec = stream.get("codec_name") or "unknown"
        tags = stream.get("tags") or {}
        language = tags.get("language") or "und"
        try:
            text = subprocess.check_output(
                [
                    "ffmpeg",
                    "-v",
                    "error",
                    "-i",
                    str(path),
                    "-map",
                    f"0:s:{ordinal}",
                    "-f",
                    "srt",
                    "-",
                ],
                text=True,
                stderr=subprocess.DEVNULL,
            )
        except Exception:
            continue
        cue_lines = []
        for raw in text.splitlines():
            line = raw.strip()
            if not line or line.isdigit() or "-->" in line:
                continue
            cue_lines.append(line)
        chars = sum(len(line) for line in cue_lines)
        spaces = sum(line.count(" ") for line in cue_lines)
        words = sum(len(line.split()) for line in cue_lines)
        tokens = [tok for line in cue_lines for tok in line.split()]
        long_tokens = sum(1 for tok in tokens if len(tok) >= 14)
        max_token_len = max((len(tok) for tok in tokens), default=0)
        metrics.append(
            {
                "stream_index": stream.get("index"),
                "stream_ordinal": ordinal,
                "codec": codec,
                "language": language,
                "title": tags.get("title") or "",
                "line_count": len(cue_lines),
                "char_count": chars,
                "space_count": spaces,
                "word_count": words,
                "space_rate": (spaces / chars) if chars else None,
                "long_token_count": long_tokens,
                "max_token_len": max_token_len,
            }
        )
    return metrics

output_subtitle_text_metrics = extract_output_subtitle_text_metrics(out_path)
quality_fallback_lines = [
    ln for ln in log_text.splitlines() if "quality fallback: using Tesseract" in ln
]
language_fallback_lines = [
    ln for ln in log_text.splitlines() if "language fallback: using Tesseract" in ln
]
score_pattern = re.compile(
    r"tess_score=(?P<tess>[0-9]*\.?[0-9]+),\s*ppocr_score=(?P<ppocr>[0-9]*\.?[0-9]+),\s*ppocr_conf=(?P<conf>[0-9]*\.?[0-9]+)"
)

def parse_fallback_scores(lines):
    parsed = []
    for line in lines:
        m = score_pattern.search(line)
        if not m:
            continue
        parsed.append(
            {
                "tess_score": float(m.group("tess")),
                "ppocr_score": float(m.group("ppocr")),
                "ppocr_conf": float(m.group("conf")),
            }
        )
    return parsed

quality_fallback_scores = parse_fallback_scores(quality_fallback_lines)
language_fallback_scores = parse_fallback_scores(language_fallback_lines)

def average_key(items, key):
    if not items:
        return None
    return sum(item[key] for item in items) / len(items)

active_gpu_count = sum(1 for s in gpu_summary.values() if s["avg_util_pct"] >= 5.0)
gpu_count = len(gpu_summary)
gpu_parallelism_note = (
    f"{active_gpu_count}/{gpu_count} GPUs active (avg util >= 5%)"
    if gpu_count > 0
    else "No GPU telemetry samples captured"
)

summary = {
    "meta": meta,
    "exit_status": exit_status,
    "elapsed_sec": elapsed,
    "output_bytes": out_bytes,
    "output_duration_sec": out_duration,
    "video_packets": video_packets,
    "effective_fps": (video_packets / elapsed) if elapsed > 0 and video_packets > 0 else None,
    "realtime_factor": (out_duration / elapsed) if elapsed > 0 else None,
    "gpu": gpu_summary,
    "gpu_parallelism_note": gpu_parallelism_note,
    "source_subtitle_streams": source_subtitle_streams,
    "subtitle_codec_counts": subtitle_codec_counts,
    "bitmap_subtitle_codec_counts": bitmap_codec_counts,
    "ocr_stream_lines": ocr_stream_lines,
    "ocr_stream_cues": ocr_stream_cues,
    "total_ocr_cues": total_ocr_cues,
    "output_subtitle_text_metrics": output_subtitle_text_metrics,
    "min_ocr_space_rate": float(meta.get("MIN_OCR_SPACE_RATE") or 0.0),
    "force_tess_non_english": meta.get("FORCE_TESS_NON_ENGLISH", ""),
    "disable_tess_fallback": meta.get("DISABLE_TESS_FALLBACK", ""),
    "tess_fallback_min_gain": meta.get("TESS_FALLBACK_MIN_GAIN", ""),
    "quality_fallback_count": len(quality_fallback_lines),
    "language_fallback_count": len(language_fallback_lines),
    "quality_fallback_rate": (len(quality_fallback_lines) / total_ocr_cues)
    if total_ocr_cues > 0
    else None,
    "language_fallback_rate": (len(language_fallback_lines) / total_ocr_cues)
    if total_ocr_cues > 0
    else None,
    "quality_fallback_avg_tess_score": average_key(quality_fallback_scores, "tess_score"),
    "quality_fallback_avg_ppocr_score": average_key(quality_fallback_scores, "ppocr_score"),
    "quality_fallback_avg_ppocr_conf": average_key(quality_fallback_scores, "ppocr_conf"),
    "language_fallback_avg_tess_score": average_key(language_fallback_scores, "tess_score"),
    "language_fallback_avg_ppocr_score": average_key(language_fallback_scores, "ppocr_score"),
    "language_fallback_avg_ppocr_conf": average_key(language_fallback_scores, "ppocr_conf"),
    "worker_plan_lines": [
        ln for ln in log_text.splitlines() if "OCR worker plan:" in ln or "Running OCR with " in ln
    ],
}
summary_json.write_text(json.dumps(summary, indent=2) + "\n")

lines = []
lines.append("# OCR Stress Benchmark Summary")
lines.append("")
lines.append(f"- Exit status: `{exit_status}`")
lines.append(f"- Elapsed: `{elapsed:.0f}s`")
lines.append(f"- Output size: `{out_bytes} bytes`")
lines.append(f"- Output duration: `{out_duration:.3f}s`")
lines.append(f"- Video packets: `{video_packets}`")
lines.append(f"- OCR engine: `{meta.get('OCR_ENGINE', '')}`")
lines.append(f"- OCR preprocess: `{meta.get('OCR_PREPROCESS', '') or 'none'}`")
lines.append(f"- Subtitle mode: `{meta.get('SUB_MODE', '')}`")
lines.append(f"- Requested CUDA devices: `{meta.get('CUDA_DEVICES', '') or 'auto'}`")
lines.append(f"- OCR max jobs: `{meta.get('OCR_MAX_JOBS', '') or 'default'}`")
lines.append(f"- Jobs per GPU: `{meta.get('JOBS_PER_GPU', '') or 'default'}`")
lines.append(f"- GPU required: `{meta.get('REQUIRE_GPU', '0')}`")
lines.append(f"- Force non-English Tesseract: `{summary['force_tess_non_english'] or '0'}`")
lines.append(f"- Disable Tesseract fallback: `{summary['disable_tess_fallback'] or '0'}`")
lines.append(f"- Tesseract min gain override: `{summary['tess_fallback_min_gain'] or 'default'}`")
if summary["effective_fps"] is not None:
    lines.append(f"- Effective throughput: `{summary['effective_fps']:.2f} FPS`")
if summary["realtime_factor"] is not None:
    lines.append(f"- Realtime factor: `{summary['realtime_factor']:.2f}x`")
lines.append(f"- GPU parallelism: `{summary['gpu_parallelism_note']}`")
lines.append("")
lines.append("## GPU Stats")
lines.append("")
lines.append("| GPU | Samples | Avg Util | P50 Util | P95 Util | Peak Util | Avg Power | Peak Power | Peak VRAM | Peak Temp |")
lines.append("| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |")
for gpu_name, s in gpu_summary.items():
    lines.append(
        f"| {gpu_name} | {s['samples']} | {s['avg_util_pct']:.2f}% | {s['p50_util_pct']:.2f}% | {s['p95_util_pct']:.2f}% | "
        f"{s['peak_util_pct']:.2f}% | {s['avg_power_w']:.2f}W | {s['peak_power_w']:.2f}W | {s['peak_vram_mib']:.0f} MiB | {s['peak_temp_c']:.0f}C |"
    )
lines.append("")
lines.append("## Source Subtitle Streams")
lines.append("")
if source_subtitle_streams:
    lines.append("| Stream | Codec | Language | Bitmap | Title |")
    lines.append("| ---: | --- | --- | --- | --- |")
    for s in source_subtitle_streams:
        title = (s["title"] or "").replace("|", "/")
        lines.append(
            f"| {s['index']} | {s['codec']} | {s['language']} | {'yes' if s['is_bitmap'] else 'no'} | {title} |"
        )
    lines.append("")
    codec_summary = ", ".join(f"{k}={v}" for k, v in sorted(subtitle_codec_counts.items()))
    bitmap_summary = ", ".join(f"{k}={v}" for k, v in sorted(bitmap_codec_counts.items())) or "none"
    lines.append(f"- Subtitle codec counts: `{codec_summary}`")
    lines.append(f"- Bitmap subtitle codec counts: `{bitmap_summary}`")
else:
    lines.append("- No subtitle streams found in source probe.")
lines.append("")
lines.append("## OCR Streams")
lines.append("")
if ocr_stream_lines:
    for ln in ocr_stream_lines:
        lines.append(f"- {ln}")
    if ocr_stream_cues:
        lines.append("")
        lines.append("### OCR Cue Counts")
        lines.append("")
        for item in ocr_stream_cues:
            lines.append(f"- Stream `{item['stream_index']}` -> `{item['cues']}` cues")
        lines.append(f"- Total OCR cues emitted: `{total_ocr_cues}`")
else:
    lines.append("- No OCR stream summary lines found in run.log")
lines.append("")
lines.append("## Output Subtitle Text Quality")
lines.append("")
if output_subtitle_text_metrics:
    lines.append("| Stream | Language | Lines | Chars | Spaces | Space Rate | Long Tokens | Max Token |")
    lines.append("| ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: |")
    for item in output_subtitle_text_metrics:
        rate = item["space_rate"]
        rate_s = f"{rate:.4f}" if rate is not None else "n/a"
        lines.append(
            f"| {item['stream_index']} | {item['language']} | {item['line_count']} | {item['char_count']} | "
            f"{item['space_count']} | {rate_s} | {item['long_token_count']} | {item['max_token_len']} |"
        )
else:
    lines.append("- No extractable text subtitle streams found in output.")
lines.append("")
lines.append("## Fallback Counts")
lines.append("")
lines.append(f"- Quality fallback count: `{summary['quality_fallback_count']}`")
lines.append(f"- Language fallback count: `{summary['language_fallback_count']}`")
if summary["quality_fallback_rate"] is not None:
    lines.append(f"- Quality fallback rate: `{summary['quality_fallback_rate'] * 100:.2f}%`")
if summary["language_fallback_rate"] is not None:
    lines.append(f"- Language fallback rate: `{summary['language_fallback_rate'] * 100:.2f}%`")
if summary["quality_fallback_avg_ppocr_score"] is not None:
    lines.append(
        f"- Quality fallback avg scores: `tess={summary['quality_fallback_avg_tess_score']:.3f}`, "
        f"`ppocr={summary['quality_fallback_avg_ppocr_score']:.3f}`, "
        f"`ppocr_conf={summary['quality_fallback_avg_ppocr_conf']:.3f}`"
    )
if summary["language_fallback_avg_ppocr_score"] is not None:
    lines.append(
        f"- Language fallback avg scores: `tess={summary['language_fallback_avg_tess_score']:.3f}`, "
        f"`ppocr={summary['language_fallback_avg_ppocr_score']:.3f}`, "
        f"`ppocr_conf={summary['language_fallback_avg_ppocr_conf']:.3f}`"
    )
lines.append("")
lines.append("## Worker Plan")
lines.append("")
if summary["worker_plan_lines"]:
    for ln in summary["worker_plan_lines"]:
        lines.append(f"- {ln}")
else:
    lines.append("- No OCR worker plan lines found in run.log")
summary_md.write_text("\n".join(lines) + "\n")

min_space_rate = float(meta.get("MIN_OCR_SPACE_RATE") or 0.0)
if min_space_rate > 0.0:
    checked = [
        item for item in output_subtitle_text_metrics
        if item.get("language", "").lower() in {"eng", "en", "en-us", "en_us"}
        and item.get("char_count", 0) >= 120
    ]
    failing = [item for item in checked if (item.get("space_rate") or 0.0) < min_space_rate]
    if failing:
        details = ", ".join(
            f"stream {item['stream_index']} space_rate={item['space_rate']:.4f}"
            for item in failing
        )
        print(f"OCR text quality gate failed: {details}; threshold={min_space_rate:.4f}", file=sys.stderr)
        sys.exit(3)
PY

echo "Benchmark completed with status $status"
echo "Artifacts:"
echo "  $META"
echo "  $CMD_TXT"
echo "  $LOG"
echo "  $SMI"
echo "  $SUMMARY_JSON"
echo "  $SUMMARY_MD"

exit "$status"
