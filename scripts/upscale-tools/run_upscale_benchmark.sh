#!/usr/bin/env bash
# AI upscale benchmark: deterministic ffmpeg upscales vs direct_play_nice --ai-upscale-model.
#
# Inputs are 480p clips with a matching 1080p reference. Every candidate upscales
# the 480p clip to 1080p; the script records wall time, FPS, realtime factor,
# output size, PSNR/SSIM, and VMAF (when ffmpeg has libvmaf) against the reference.
#
# Usage:
#   scripts/upscale-tools/run_upscale_benchmark.sh --clip NAME=lowres.mkv:ref_1080p.mkv [--clip ...] \
#     [--bin target/release/direct_play_nice] [--work-dir DIR] [--models realesr-animevideov3,custom:NAME=/path/model.onnx] \
#     [--device auto|cuda|cpu] [--tile N] [--hw-accel auto|none|nvenc] [--frames N] [--env KEY=VALUE]...
#
# The low-res clip must carry an audio stream (DPN validates AAC audio in the output).
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
bin="${DPN_UPSCALE_BENCH_BIN:-$root_dir/target/release/direct_play_nice}"
work_dir="${DPN_UPSCALE_BENCH_WORK:-$(mktemp -d)}"
models="realesr-animevideov3,realesr-general-x4v3"
device="auto"
tile="0"
hw_accel="auto"
frames="240"
label="$(hostname)"
declare -a clips=()
declare -a extra_env=()

while [ $# -gt 0 ]; do
  case "$1" in
    --clip) clips+=("$2"); shift 2 ;;
    --bin) bin="$2"; shift 2 ;;
    --work-dir) work_dir="$2"; shift 2 ;;
    --models) models="$2"; shift 2 ;;
    --device) device="$2"; shift 2 ;;
    --tile) tile="$2"; shift 2 ;;
    --hw-accel) hw_accel="$2"; shift 2 ;;
    --frames) frames="$2"; shift 2 ;;
    --label) label="$2"; shift 2 ;;
    --env) extra_env+=("$2"); shift 2 ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
done

if [ "${#clips[@]}" -eq 0 ]; then
  echo "at least one --clip NAME=lowres.mkv:ref_1080p.mkv is required" >&2
  exit 2
fi
# Portable wall clock and core count (macOS date has no %N and no nproc).
now() { python3 -c 'import time; print(f"{time.time():.3f}")'; }
cores() { getconf _NPROCESSORS_ONLN 2>/dev/null || nproc 2>/dev/null || echo 4; }

for cmd in ffmpeg ffprobe awk python3; do
  command -v "$cmd" >/dev/null 2>&1 || { echo "missing required command: $cmd" >&2; exit 1; }
done
[ -x "$bin" ] || { echo "direct_play_nice binary not found: $bin" >&2; exit 1; }

mkdir -p "$work_dir"
report="$work_dir/upscale_report.csv"
config_file="$work_dir/empty-config.toml"
touch "$config_file"
# Capture first: under pipefail, `grep -q` closing the pipe early would mark ffmpeg as failed.
filter_list="$(ffmpeg -hide_banner -filters 2>/dev/null || true)"
have_vmaf=0
if printf '%s' "$filter_list" | grep -q " libvmaf "; then
  have_vmaf=1
fi

echo "host,clip,method,status,elapsed_s,fps,realtime,size_bytes,psnr,ssim,vmaf_mean,vmaf_harmonic,width,height" > "$report"

probe_frames() {
  ffprobe -v error -select_streams v:0 -count_frames -show_entries stream=nb_read_frames -of csv=p=0 "$1"
}

probe_rate() {
  ffprobe -v error -select_streams v:0 -show_entries stream=r_frame_rate -of csv=p=0 "$1" | awk -F/ '{ if ($2 > 0) printf "%.4f", $1/$2; else print $1 }'
}

probe_dims() {
  ffprobe -v error -select_streams v:0 -show_entries stream=width,height -of csv=p=0 "$1"
}

score() {
  local out="$1" ref="$2" tag="$3"
  local stats psnr ssim vmaf_mean vmaf_harm
  stats="$(ffmpeg -hide_banner -i "$out" -i "$ref" -frames:v "$frames" \
    -lavfi "[0:v][1:v]psnr=stats_file=-;[0:v][1:v]ssim=stats_file=-" -f null - 2>&1 || true)"
  psnr="$(printf '%s' "$stats" | grep -o 'PSNR .*average:[0-9.]*' | sed 's/.*average://' | tail -1)"
  ssim="$(printf '%s' "$stats" | grep -o 'SSIM .*All:[0-9.]*' | sed 's/.*All://' | tail -1)"
  vmaf_mean=""; vmaf_harm=""
  if [ "$have_vmaf" = "1" ]; then
    local log="$work_dir/vmaf_${tag}.json"
    ffmpeg -hide_banner -loglevel error -i "$out" -i "$ref" -frames:v "$frames" \
      -lavfi "[0:v][1:v]libvmaf=log_fmt=json:log_path=$log:n_threads=$(cores)" -f null - || true
    if [ -s "$log" ]; then
      vmaf_mean="$(python3 -c "import json,sys; print(round(json.load(open(sys.argv[1]))['pooled_metrics']['vmaf']['mean'],2))" "$log" 2>/dev/null || true)"
      vmaf_harm="$(python3 -c "import json,sys; print(round(json.load(open(sys.argv[1]))['pooled_metrics']['vmaf']['harmonic_mean'],2))" "$log" 2>/dev/null || true)"
    fi
  fi
  printf '%s,%s,%s,%s' "${psnr:-}" "${ssim:-}" "${vmaf_mean:-}" "${vmaf_harm:-}"
}

row() {
  # host clip method status elapsed fps realtime size metrics dims
  printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' "$label" "$1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" "$9" >> "$report"
}

run_ffmpeg_baseline() {
  local clip="$1" src="$2" ref="$3" flags="$4"
  local out="$work_dir/${clip}_ffmpeg_${flags}.mp4"
  local started ended elapsed nframes rate fps realtime size metrics dims
  started="$(now)"
  ffmpeg -hide_banner -loglevel error -y -i "$src" -frames:v "$frames" \
    -vf "scale=1920:1080:flags=${flags},format=yuv420p" -c:v libx264 -preset fast -crf 18 -an "$out"
  ended="$(now)"
  elapsed="$(awk -v s="$started" -v e="$ended" 'BEGIN { printf "%.3f", e - s }')"
  nframes="$(probe_frames "$out")"; rate="$(probe_rate "$src")"
  fps="$(awk -v n="$nframes" -v t="$elapsed" 'BEGIN { printf "%.2f", n / t }')"
  realtime="$(awk -v f="$fps" -v r="$rate" 'BEGIN { printf "%.2f", f / r }')"
  size="$(wc -c < "$out" | tr -d ' ')"
  metrics="$(score "$out" "$ref" "${clip}_ffmpeg_${flags}")"
  dims="$(probe_dims "$out")"
  row "$clip" "ffmpeg-${flags}" "ok" "$elapsed" "$fps" "$realtime" "$size" "$metrics" "$dims"
}

run_dpn() {
  local clip="$1" src="$2" ref="$3" model_spec="$4"
  # A model entry is either a built-in name or custom:NAME=/path/to/model.onnx.
  local model="$model_spec" model_flag="$model_spec" model_path_args=()
  if [[ "$model_spec" == custom:* ]]; then
    local custom="${model_spec#custom:}"
    model="${custom%%=*}"
    model_flag="custom"
    model_path_args=(--ai-upscale-model-path "${custom#*=}")
  fi
  local out="$work_dir/${clip}_dpn_${model}.mp4"
  local log="$work_dir/${clip}_dpn_${model}.log"
  local started ended elapsed nframes rate fps realtime size metrics dims status
  started="$(now)"
  if env "${extra_env[@]}" "$bin" \
      --config-file "$config_file" \
      --device roku \
      --video-quality 1080p \
      --hw-accel "$hw_accel" \
      --sub-mode skip \
      --ai-upscale-model "$model_flag" "${model_path_args[@]}" \
      --ai-upscale-device "$device" \
      --ai-upscale-tile "$tile" \
      --delete-source=false \
      "$src" "$out" >"$log" 2>&1; then
    status="ok"
  else
    status="failed"
    echo "DPN candidate failed: clip=$clip model=$model log=$log" >&2
    tail -n 20 "$log" >&2 || true
    row "$clip" "dpn-${model}" "$status" "" "" "" "" ",,," ","
    return 0
  fi
  ended="$(now)"
  elapsed="$(awk -v s="$started" -v e="$ended" 'BEGIN { printf "%.3f", e - s }')"
  nframes="$(probe_frames "$out")"; rate="$(probe_rate "$src")"
  fps="$(awk -v n="$nframes" -v t="$elapsed" 'BEGIN { printf "%.2f", n / t }')"
  realtime="$(awk -v f="$fps" -v r="$rate" 'BEGIN { printf "%.2f", f / r }')"
  size="$(wc -c < "$out" | tr -d ' ')"
  metrics="$(score "$out" "$ref" "${clip}_dpn_${model}")"
  dims="$(probe_dims "$out")"
  row "$clip" "dpn-${model}" "$status" "$elapsed" "$fps" "$realtime" "$size" "$metrics" "$dims"
  grep -E "AI upscale: [0-9]+ frames" "$log" | tail -1 | sed "s/^/  [$clip $model] /" || true
}

IFS=',' read -r -a model_list <<< "$models"
for spec in "${clips[@]}"; do
  clip="${spec%%=*}"; rest="${spec#*=}"; src="${rest%%:*}"; ref="${rest#*:}"
  [ -r "$src" ] || { echo "low-res clip not readable: $src" >&2; exit 1; }
  [ -r "$ref" ] || { echo "reference clip not readable: $ref" >&2; exit 1; }
  echo "== clip $clip: $src -> 1080p (ref $ref)"
  for flags in lanczos spline; do
    run_ffmpeg_baseline "$clip" "$src" "$ref" "$flags"
  done
  for model in "${model_list[@]}"; do
    run_dpn "$clip" "$src" "$ref" "$model"
  done
done

echo
echo "report: $report"
column -s, -t < "$report" 2>/dev/null || cat "$report"
