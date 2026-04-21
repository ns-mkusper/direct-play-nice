# Troubleshooting

## `--probe-hw` shows no usable hardware encoders

- confirm FFmpeg build includes your target encoder (`h264_nvenc`, `h264_qsv`, etc.)
- run `--probe-codecs --only-video --only-hw`
- set `--hw-accel none` as a fallback path while debugging

## OCR falls back to CPU unexpectedly

- verify runtime libraries for your platform/provider are installed
- use `DPN_OCR_REQUIRE_GPU=1` to fail fast instead of silently falling back
- try `--ocr-engine pp-ocr-v3` on older GPUs/runtime stacks

## Output not directly playable on a target client

- verify target selection (`--device`)
- inspect stream details with `--probe-streams`
- set explicit bitrate/quality limits to match endpoint constraints
- compare against [SUPPORTED_DEVICES.md](../../SUPPORTED_DEVICES.md)
