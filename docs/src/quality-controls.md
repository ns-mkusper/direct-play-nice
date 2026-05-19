# Quality Controls

By default, the CLI preserves source quality (`match-source`) for both video
and audio.

## Video quality presets

`--video-quality` supports:

- `match-source`
- `360p`
- `480p`
- `720p`
- `1080p`
- `1440p`
- `2160p`

These presets apply resolution caps and target bitrate ranges suitable for
common direct-play scenarios.

## Audio quality presets

`--audio-quality` supports:

- `match-source`
- `320k`
- `256k`
- `224k`
- `192k`
- `160k`
- `128k`
- `96k`

## Custom bitrate overrides

Use these for explicit control:

- `--max-video-bitrate <RATE>` (for example `4800k`, `6M`, `12.5mbps`)
- `--max-audio-bitrate <RATE>`

When overrides are provided, they constrain the selected quality profile.

## Resizing

When a source exceeds a selected device or quality cap, the video is resized down
while preserving aspect ratio and keeping encoder-friendly dimensions. The tool
does not enlarge source dimensions as part of this deterministic FFmpeg resizing
path.

`--resize-quality` supports:

- `fast-bilinear` (fastest)
- `bilinear`
- `bicubic`
- `lanczos` (default, highest-quality deterministic default)
- `spline`

Higher-quality resize kernels generally cost more CPU time. They remain
deterministic FFmpeg software scalers and do not add AI model dependencies.

The resize benchmark harness compares downscale speed and full-reference quality
against a deterministic 360p reference:

```bash
scripts/resize-tools/run_resize_benchmark.sh
```

The benchmark writes a CSV report with elapsed time, FPS, realtime factor,
output size, and available quality metrics such as VMAF, PSNR, and SSIM.
Set `DPN_RESIZE_REF_VIDEO=/path/to/reference.mkv` to use a real source clip.
If `DPN_RESIZE_REF_VIDEO` is unset, the harness also honors the benchmark-runner
`BENCHMARK_SOURCE_PATH` environment variable. Set `DPN_RESIZE_BENCH_SS=00:01:30`
to start from a representative section and `DPN_RESIZE_HW_ACCEL=nvenc` to
exercise hardware encoding during the run.
