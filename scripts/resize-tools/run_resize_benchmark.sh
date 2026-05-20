#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
bin="${DPN_RESIZE_BENCH_BIN:-$root_dir/target/release/direct_play_nice}"
work_dir="${DPN_RESIZE_BENCH_WORK:-$(mktemp -d)}"
duration="${DPN_RESIZE_BENCH_DURATION:-6}"
rate="${DPN_RESIZE_BENCH_RATE:-24}"
config_file="${DPN_RESIZE_BENCH_CONFIG:-$work_dir/empty-config.toml}"
seek="${DPN_RESIZE_BENCH_SS:-}"
source_video="${DPN_RESIZE_REF_VIDEO:-${BENCHMARK_SOURCE_PATH:-}}"

mkdir -p "$work_dir"
touch "$config_file"

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

source_high="$work_dir/source_720p.mkv"
ref="$work_dir/ref_360p.mp4"
baseline="$work_dir/resize_fast_bilinear.mp4"
report="$work_dir/resize_report.csv"

if [ -n "$source_video" ]; then
  if [ ! -r "$source_video" ]; then
    echo "resize benchmark source is not readable: $source_video" >&2
    exit 1
  fi

  seek_args=()
  if [ -n "$seek" ]; then
    seek_args=(-ss "$seek")
  fi

  ffmpeg -hide_banner -y "${seek_args[@]}" -i "$source_video" \
    -vf "scale=1280:720:flags=lanczos,fps=$rate,format=yuv420p" \
    -t "$duration" -an -c:v mpeg2video -b:v 12M "$source_high"
else
  ffmpeg -hide_banner -y \
    -f lavfi -i "testsrc2=size=1280x720:rate=$rate:duration=$duration" \
    -f lavfi -i "testsrc=size=1280x720:rate=$rate:duration=$duration" \
    -filter_complex "[0:v][1:v]blend=all_mode=overlay:all_opacity=0.25,format=yuv420p" \
    -an -c:v mpeg2video -b:v 12M "$source_high"
fi

ffmpeg -hide_banner -y -i "$source_high" \
  -vf "scale=640:360:flags=lanczos,format=yuv420p" \
  -c:v libx264 -preset veryfast -crf 16 -an "$ref"

run_candidate() {
  local quality="$1"
  local output="$2"
  local started ended elapsed fps realtime size_bytes

  started="$(date +%s.%N)"
  "$bin" \
    --config-file "$config_file" \
    --device chromecast \
    --hw-accel "${DPN_RESIZE_HW_ACCEL:-none}" \
    --video-quality 360p \
    --resize-quality "$quality" \
    --skip-codec-check \
    "$source_high" "$output" >/dev/null
  ended="$(date +%s.%N)"

  elapsed="$(awk -v s="$started" -v e="$ended" 'BEGIN { printf "%.3f", e - s }')"
  fps="$(awk -v d="$duration" -v r="$rate" -v t="$elapsed" 'BEGIN { printf "%.3f", (d * r) / t }')"
  realtime="$(awk -v d="$duration" -v t="$elapsed" 'BEGIN { printf "%.3f", d / t }')"
  size_bytes="$(wc -c < "$output" | tr -d ' ')"

  printf "%s,%s,%s,%s,%s" "$quality" "$elapsed" "$fps" "$realtime" "$size_bytes"
}

metric_field() {
  local line="$1"
  local name="$2"
  awk -v name="$name" '{
    for (i = 1; i <= NF; i++) {
      if ($i ~ "^" name ":[0-9.]*") {
        sub("^" name ":", "", $i)
        print $i
        exit
      }
    }
  }' <<<"$line"
}

ssim_db_value() {
  local line="$1"
  sed -nE 's/.*All:[0-9.]+ \(([0-9.]+)\).*/\1/p' <<<"$line"
}

metric_summary() {
  local candidate="$1"
  local metric_log="$work_dir/metrics_$(basename "$candidate").log"
  local vmaf_json="$work_dir/vmaf_$(basename "$candidate").json"
  local vmaf psnr_line psnr_y psnr_u psnr_v psnr_avg psnr_min psnr_max
  local ssim_line ssim_y ssim_u ssim_v ssim_all ssim_db

  if ffmpeg -hide_banner -y -i "$candidate" -i "$ref" \
    -lavfi "[0:v]setpts=PTS-STARTPTS[dist];[1:v]setpts=PTS-STARTPTS[ref];[dist][ref]libvmaf=log_fmt=json:log_path=$vmaf_json" \
    -f null - >/dev/null 2>"$metric_log"; then
    vmaf="$(
      awk -F: '
        /"pooled_metrics"[[:space:]]*:/ { in_pooled = 1; next }
        in_pooled && /"aggregate_metrics"[[:space:]]*:/ { in_pooled = 0 }
        in_pooled && /"vmaf"[[:space:]]*:/ { in_vmaf = 1; next }
        in_vmaf && /"mean"[[:space:]]*:/ {
          value = $2
          gsub(/[,[:space:]]/, "", value)
          print value
          exit
        }
      ' "$vmaf_json"
    )"
    if [ -z "$vmaf" ]; then
      vmaf="$(
        awk -F: '
          /"vmaf"[[:space:]]*:[[:space:]]*[0-9.]+/ {
            value = $2
            gsub(/[,[:space:]]/, "", value)
            sum += value
            count += 1
          }
          END {
            if (count > 0) {
              printf "%.6f", sum / count
            }
          }
        ' "$vmaf_json"
      )"
    fi
  else
    vmaf="na"
  fi

  ffmpeg -hide_banner -y -i "$candidate" -i "$ref" \
    -lavfi "[0:v]setpts=PTS-STARTPTS[dist];[1:v]setpts=PTS-STARTPTS[ref];[dist][ref]psnr=stats_file=-" \
    -f null - >/dev/null 2>"$metric_log" || true
  psnr_line="$(grep -o 'PSNR .*' "$metric_log" | tail -n1)"
  psnr_y="$(metric_field "$psnr_line" "y")"
  psnr_u="$(metric_field "$psnr_line" "u")"
  psnr_v="$(metric_field "$psnr_line" "v")"
  psnr_avg="$(metric_field "$psnr_line" "average")"
  psnr_min="$(metric_field "$psnr_line" "min")"
  psnr_max="$(metric_field "$psnr_line" "max")"

  ffmpeg -hide_banner -y -i "$candidate" -i "$ref" \
    -lavfi "[0:v]setpts=PTS-STARTPTS[dist];[1:v]setpts=PTS-STARTPTS[ref];[dist][ref]ssim=stats_file=-" \
    -f null - >/dev/null 2>"$metric_log" || true
  ssim_line="$(grep -o 'SSIM .*' "$metric_log" | tail -n1)"
  ssim_y="$(metric_field "$ssim_line" "Y")"
  ssim_u="$(metric_field "$ssim_line" "U")"
  ssim_v="$(metric_field "$ssim_line" "V")"
  ssim_all="$(metric_field "$ssim_line" "All")"
  ssim_db="$(ssim_db_value "$ssim_line")"

  printf ",%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n" \
    "${vmaf:-na}" \
    "${psnr_y:-na}" "${psnr_u:-na}" "${psnr_v:-na}" "${psnr_avg:-na}" "${psnr_min:-na}" "${psnr_max:-na}" \
    "${ssim_y:-na}" "${ssim_u:-na}" "${ssim_v:-na}" "${ssim_all:-na}" "${ssim_db:-na}"
}

echo "quality,elapsed_seconds,fps,realtime_factor,size_bytes,vmaf,psnr_y,psnr_u,psnr_v,psnr_avg,psnr_min,psnr_max,ssim_y,ssim_u,ssim_v,ssim_all,ssim_db" > "$report"

{
  run_candidate "fast-bilinear" "$baseline"
  metric_summary "$baseline"
} >> "$report"

for quality in bilinear bicubic lanczos spline; do
  output="$work_dir/resize_${quality}.mp4"
  {
    run_candidate "$quality" "$output"
    metric_summary "$output"
  } >> "$report"
done

echo "Resize benchmark report: $report"
cat "$report"
