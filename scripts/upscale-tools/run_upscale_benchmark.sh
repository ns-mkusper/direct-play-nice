#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
bin="${DPN_UPSCALE_BENCH_BIN:-$root_dir/target/release/direct_play_nice}"
work_dir="${DPN_UPSCALE_BENCH_WORK:-$(mktemp -d)}"
duration="${DPN_UPSCALE_BENCH_DURATION:-6}"
rate="${DPN_UPSCALE_BENCH_RATE:-24}"

mkdir -p "$work_dir"

if [ ! -x "$bin" ]; then
  cargo build --release --manifest-path "$root_dir/Cargo.toml"
fi

require_cmd() {
  if ! command -v "$1" >/dev/null 2>&1; then
    echo "missing required command: $1" >&2
    exit 1
  fi
}

require_cmd ffmpeg
require_cmd ffprobe

ref="$work_dir/ref_720p.mp4"
low="$work_dir/low_360p.mkv"
baseline="$work_dir/upscale_fast_bilinear.mp4"
report="$work_dir/upscale_report.csv"

if [ -n "${DPN_UPSCALE_REF_VIDEO:-}" ]; then
  ffmpeg -hide_banner -y -i "$DPN_UPSCALE_REF_VIDEO" \
    -vf "scale=1280:720:flags=lanczos,fps=$rate,format=yuv420p" \
    -t "$duration" -an -c:v libx264 -preset veryfast -crf 16 "$ref"
else
  ffmpeg -hide_banner -y \
    -f lavfi -i "testsrc2=size=1280x720:rate=$rate:duration=$duration" \
    -f lavfi -i "testsrc=size=1280x720:rate=$rate:duration=$duration" \
    -filter_complex "[0:v][1:v]blend=all_mode=overlay:all_opacity=0.25,format=yuv420p" \
    -an -c:v libx264 -preset veryfast -crf 16 "$ref"
fi

ffmpeg -hide_banner -y -i "$ref" \
  -vf "scale=640:360:flags=lanczos,format=yuv420p" \
  -c:v mpeg2video -b:v 4M -an "$low"

run_candidate() {
  local scaler="$1"
  local output="$2"
  local started ended elapsed fps realtime size_bytes

  started="$(date +%s.%N)"
  "$bin" \
    --device chromecast \
    --hw-accel "${DPN_UPSCALE_HW_ACCEL:-none}" \
    --video-quality 720p \
    --upscale-mode fit-quality \
    --scaler-quality "$scaler" \
    --skip-codec-check \
    "$low" "$output" >/dev/null
  ended="$(date +%s.%N)"

  elapsed="$(awk -v s="$started" -v e="$ended" 'BEGIN { printf "%.3f", e - s }')"
  fps="$(awk -v d="$duration" -v r="$rate" -v t="$elapsed" 'BEGIN { printf "%.3f", (d * r) / t }')"
  realtime="$(awk -v d="$duration" -v t="$elapsed" 'BEGIN { printf "%.3f", d / t }')"
  size_bytes="$(wc -c < "$output" | tr -d ' ')"

  printf "%s,%s,%s,%s,%s" "$scaler" "$elapsed" "$fps" "$realtime" "$size_bytes"
}

metric_summary() {
  local candidate="$1"
  local metric_log="$work_dir/metrics_$(basename "$candidate").log"
  local vmaf psnr ssim

  if ffmpeg -hide_banner -y -i "$candidate" -i "$ref" \
    -lavfi "[0:v]setpts=PTS-STARTPTS[dist];[1:v]setpts=PTS-STARTPTS[ref];[dist][ref]libvmaf=log_fmt=json:log_path=$work_dir/vmaf_$(basename "$candidate").json" \
    -f null - >/dev/null 2>"$metric_log"; then
    vmaf="$(grep -o '"mean":[0-9.]*' "$work_dir/vmaf_$(basename "$candidate").json" | head -n1 | cut -d: -f2)"
  else
    vmaf="na"
  fi

  ffmpeg -hide_banner -y -i "$candidate" -i "$ref" \
    -lavfi "[0:v]setpts=PTS-STARTPTS[dist];[1:v]setpts=PTS-STARTPTS[ref];[dist][ref]psnr=stats_file=-" \
    -f null - >/dev/null 2>"$metric_log" || true
  psnr="$(grep -o 'average:[0-9.]*' "$metric_log" | tail -n1 | cut -d: -f2)"

  ffmpeg -hide_banner -y -i "$candidate" -i "$ref" \
    -lavfi "[0:v]setpts=PTS-STARTPTS[dist];[1:v]setpts=PTS-STARTPTS[ref];[dist][ref]ssim=stats_file=-" \
    -f null - >/dev/null 2>"$metric_log" || true
  ssim="$(grep -o 'All:[0-9.]*' "$metric_log" | tail -n1 | cut -d: -f2)"

  printf ",%s,%s,%s\n" "${vmaf:-na}" "${psnr:-na}" "${ssim:-na}"
}

echo "scaler,elapsed_seconds,fps,realtime_factor,size_bytes,vmaf,psnr,ssim" > "$report"

{
  run_candidate "fast-bilinear" "$baseline"
  metric_summary "$baseline"
} >> "$report"

for scaler in bilinear bicubic lanczos spline; do
  output="$work_dir/upscale_${scaler}.mp4"
  {
    run_candidate "$scaler" "$output"
    metric_summary "$output"
  } >> "$report"
done

echo "Upscale benchmark report: $report"
cat "$report"
