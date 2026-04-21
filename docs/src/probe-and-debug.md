# Probe and Debug

## Hardware probe

```bash
direct_play_nice --probe-hw
direct_play_nice --probe-hw --probe-json
```

## Codec inventory

```bash
direct_play_nice --probe-codecs --only-video
direct_play_nice --probe-codecs --only-video --only-hw --probe-json
```

## Input stream probe

```bash
direct_play_nice --probe-streams input.mkv
direct_play_nice --probe-streams --output json input.mkv
```

## Concurrency controls

- `DIRECT_PLAY_NICE_MAX_JOBS`
- `DIRECT_PLAY_NICE_JOBS_PER_GPU`

Use these to tune parallel conversion throughput per machine.

## OCR runtime diagnostics (Linux)

- verify ONNX Runtime linkage if OCR provider loading fails
- set `ORT_DYLIB_PATH` when `libonnxruntime.so` is in a non-standard location
- set `DPN_OCR_REQUIRE_GPU=1` for fail-fast behavior when GPU OCR is mandatory
