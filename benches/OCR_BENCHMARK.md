# OCR Benchmark — Silence (2016) (April 17, 2026)

## Scope

This document records the latest full-movie OCR stress benchmark on `plexserver`.

- Host: `plexserver` (`mkusper@10.0.0.246:2204`)
- Benchmark source archive: `/mnt/data4/benchmarks/ocr/silence_2016/source.mkv`
- Source size/duration: `38,405,520,626 bytes`, `9669.494s` (2h41m09s)
- Command profile: `--ocr-engine pp-ocr-v3 --sub-mode force --skip-codec-check --delete-source=false`

## Input stream layout

```text
0: video    h264                lang=eng  title=English
1: audio    dts                 lang=eng  title=English
2: subtitle hdmv_pgs_subtitle   lang=eng  title=English
3: subtitle hdmv_pgs_subtitle   lang=eng  title=English (SDH)
4: subtitle hdmv_pgs_subtitle   lang=fre  title=French
5: subtitle hdmv_pgs_subtitle   lang=spa  title=Spanish
```

## Run history (this benchmark session)

### Attempt A (failed)

- Run dir: `/mnt/data4/benchmarks/ocr/silence_2016/run_20260417_221431`
- Start: `2026-04-17T22:14:31-05:00`
- End: `2026-04-17T22:29:18-05:00`
- Elapsed: `886s`
- Exit status: `1`
- Failure: ONNX Runtime CUDA provider load error (`libcublasLt.so.13` missing from provider dependency chain).

### Attempt B (successful, final)

- Run dir: `/mnt/data4/benchmarks/ocr/silence_2016/run_20260417_223648_ort116`
- Start: `2026-04-17T22:36:48-05:00`
- End: `2026-04-17T23:44:08-05:00`
- Elapsed: `4040s` (`67m20s`)
- Exit status: `0`
- Runtime override used: `LD_LIBRARY_PATH=/opt/direct-play-nice/ort116-runtime/lib`, `ORT_DYLIB_PATH=/opt/direct-play-nice/ort116-runtime/lib/libonnxruntime.so`

## Final performance metrics (Attempt B)

| Metric | Value |
| --- | ---: |
| Output duration | `9669.482s` |
| Output file size | `1,367,470,630 bytes` |
| Video packets (stream 0) | `231,830` |
| Effective throughput | `57.38 FPS` |
| Realtime factor | `2.39x` |

### GPU telemetry (nvidia-smi, 1s sampling)

| Metric | GPU0 | GPU1 |
| --- | ---: | ---: |
| Samples | `4009` | `4009` |
| Avg utilization | `32.49%` | `0.00%` |
| Peak utilization | `100.00%` | `0.00%` |
| Avg power | `46.17W` | `7.26W` |
| Peak power | `83.62W` | `7.67W` |
| Peak VRAM | `463 MiB` | `5 MiB` |
| Peak temp | `75C` | `46C` |

## OCR output metrics (Attempt B)

- Spacing fallback hits (`PP-OCRv4 spacing fallback...` log lines): `4993`
- Empty-text warning-like lines: `0`
- OCR subtitle outputs:
  - Stream `2` -> `1480` cues
  - Stream `3` -> `1787` cues
  - Stream `4` -> `1662` cues
  - Stream `5` -> `1653` cues
- Total OCR cues emitted: `6582`

## Notes

- This source includes **four** bitmap subtitle streams (eng/eng-sdh/fre/spa). OCR load is materially higher than older one-stream stress runs.
- Benchmark artifacts are intentionally stored off `/` under `/mnt/data4/benchmarks/ocr/silence_2016/`.
