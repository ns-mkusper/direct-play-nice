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

## Upscaling

Upscaling is disabled by default. This preserves the source resolution unless
the source exceeds a selected device or quality cap.

Use `--upscale-mode fit-quality` to enlarge low-resolution sources toward an
explicit `--video-quality` target while still respecting the selected device
ceiling:

```bash
direct_play_nice --video-quality 1080p --upscale-mode fit-quality --scaler-quality lanczos input.mkv output.mp4
```

Use `--upscale-mode force` only when you want the output to grow toward the
quality target, or the selected device ceiling when no quality target is set.

`--scaler-quality` supports:

- `fast-bilinear` (default, fastest)
- `bilinear`
- `bicubic`
- `lanczos`
- `spline`

Higher-quality scalers generally cost more CPU time. They remain deterministic
FFmpeg software scalers and do not add AI model dependencies.

The upscale benchmark harness can compare speed and full-reference quality:

```bash
scripts/upscale-tools/run_upscale_benchmark.sh
```

The benchmark writes a CSV report with elapsed time, FPS, realtime factor,
output size, and available quality metrics such as VMAF, PSNR, and SSIM.
Set `DPN_UPSCALE_REF_VIDEO=/path/to/reference.mkv` to use a real source clip,
`DPN_UPSCALE_BENCH_SS=00:01:30` to start from a representative section, and
`DPN_UPSCALE_HW_ACCEL=nvenc` to exercise hardware encoding during the run.
