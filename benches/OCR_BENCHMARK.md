# OCR Benchmark - Full-Movie Stress (Sanitized)

## Scope

This document records a full-movie OCR stress run for a representative
feature-length remux on a self-hosted Linux server.

- Host: self-hosted Linux media server (anonymized)
- Source path: anonymized local benchmark copy
- Source duration: `9669.494s` (`2h41m09s`)
- OCR mode: PP-OCRv3 with GPU-required runtime profile
- Subtitle mode: force OCR for bitmap subtitle streams

## Input stream layout

```text
0: video    h264                lang=eng  title=English
1: audio    dts                 lang=eng  title=English
2: subtitle hdmv_pgs_subtitle   lang=eng  title=English
3: subtitle hdmv_pgs_subtitle   lang=eng  title=English (SDH)
4: subtitle hdmv_pgs_subtitle   lang=fre  title=French
5: subtitle hdmv_pgs_subtitle   lang=spa  title=Spanish
```

## Final run summary

- Run date: `2026-04-20`
- Exit status: `0`
- Elapsed wall time: `2646s` (`44m06s`)
- Effective throughput: `87.62 FPS`
- Realtime factor: `3.65x`

## Performance metrics

| Metric | Value |
| --- | ---: |
| Output duration | `9669.493s` |
| Effective throughput | `87.62 FPS` |
| Realtime factor | `3.65x` |

## GPU telemetry

Sampling used one-second intervals during the benchmark run.

| Metric | GPU0 | GPU1 |
| --- | ---: | ---: |
| Average utilization | `34.04%` | `22.25%` |

## OCR output metrics

- OCR subtitle outputs:
  - Stream `2` -> `1480` cues
  - Stream `3` -> `1787` cues
  - Stream `4` -> `1662` cues
  - Stream `5` -> `1653` cues
- Total OCR cues emitted: `6582`
- Quality fallback count: `2747`
- Language fallback count: `3315`

## Notes

- This source has four bitmap subtitle streams, so OCR load is materially
  higher than single-stream validation runs.
- Paths, hostnames, and internal runtime directories are intentionally
  anonymized in this report.
