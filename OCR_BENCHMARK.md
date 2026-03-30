# OCR Benchmark — Silence (2016)

## Scope and inputs

- Full-movie runs (02:41:09) used for performance metrics: legacy,
  Tesseract, PP-OCRv4 (`*_new2` runs).
- 5-minute slice (00:15:00–00:20:00) used for accuracy and side-by-side
  text comparisons.
- GPU stats are from GPU0 `nvidia-smi` logs captured during runs.

## Performance (full movie)

| Engine | Runtime (s) | Avg W | Peak W | GPU util (avg/peak) |
| --- | ---: | ---: | ---: | ---: |
| Legacy (bitmap) | 3768.84 | 9.46 | 11.89 | 2.50%/5.00% |
| Tesseract (LSTM) | 6401.81 | 14.47 | 26.83 | 0.60%/6.00% |
| PP-OCRv4 (ORT) | 4628.22 | 9.47 | 11.89 | 2.50%/5.00% |

### Comparison summary

- PP-OCRv4 (Auto) is ~27% faster than Tesseract with ~99.8% fewer
  empty-text warnings.

### CPU Notes

- The original PP-OCRv4 run on `plexserver` reported CUDA EP availability,
  but GPU utilization remained ~0%, so inference appeared CPU-bound.
- `check_gpu_env.sh` confirmed `libcudnn.so` was missing; cuDNN was
  installed via ALA (`cudnn-9.20.0.48-1`).
- A 10-second idle sample on `plexserver` measured GPU0 at ~11.84W
  peak/avg. The older idle log in `silence_full/idle_power.csv` is invalid
  due to a bad query string.

## GPU attempts (5-minute slice, GTX 960)

| Stack | Status | Runtime | Max GPU util | Max power | Max VRAM | Output SRT |
| --- | --- | --- | --- | --- | --- | --- |
| CUDA13.2/cuDNN9.20/ORT1.24.4 | `139` | 50s | 25% | 37.56W | 105MB | No |
| CUDA11.8/cuDNN8.4.1/ORT1.14.1 | `139` | 87s | 0% | 11.79W | 13MB | No |
| CUDA11.8/cuDNN8.4.1/ORT1.15.1 | `139` | 33s | n/a | n/a | n/a | No |
| CUDA11.4/cuDNN8.2.4/ORT1.13.1 + PP-OCRv4 | `139` | 32s | 21% | 36.13W | n/a | No |
| CUDA11.4/cuDNN8.2.4/ORT1.13.1 + PP-OCRv3 | `139` | 32s | 21% | 36.24W | n/a | No |

### Notes

- The legacy stack was a zero-config run with all `DPN_OCR_*` and
  `ORT_CUDA_FLAGS` unset (except `DPN_OCR_REQUIRE_GPU=1`).
- CUDA EP initialized in both cases, but the OCR process segfaulted
  before provider-specific errors surfaced in logs.
- No `NVRM` XID entries appeared in `dmesg` during the legacy run.
- A 2026-03-29 zero-config rerun with CUDA 11.8/cuDNN 8.4.1/ORT 1.14.1
  still segfaulted (`139`) after ~32s with no GPU utilization.
- A 2026-03-29 single-GPU run (CUDA_VISIBLE_DEVICES=0) with CUDA
  11.8/cuDNN 8.4.1/ORT 1.15.1 also segfaulted (`139`) after ~33s.
- A 2026-03-29 single-GPU run with CUDA 11.4/cuDNN 8.2.4/ORT 1.13.1 hit
  ~21% GPU utilization and peaked above 36W, but still segfaulted with
  both PP-OCRv4 and PP-OCRv3 models.
- Conclusion: GTX 960 (Maxwell) is hardware-incompatible with PP-OCRv4
  and PP-OCRv3 CUDA execution (illegal instruction / provider init
  failure).

## CPU-only slice (PP-OCRv4 vs Tesseract)

5-minute slice (00:15:00–00:20:00) rebuilt from the source file and
converted with `--delete-source=false` for direct comparison.

| Engine | Runtime | Frames | FPS |
| --- | ---: | ---: | ---: |
| PP-OCRv4 (CPU) | 90s | 7267 | 80.74 |
| Tesseract (CPU) | 64s | 7267 | 113.55 |

Notes

- Full-movie runs still show PP-OCRv4 faster overall (~27% faster than
  Tesseract), but this 5-minute slice favored Tesseract on CPU.
- Baseline locked on 2026-03-29 using `cpu_victory.mp4` and
  `tesseract_baseline.mp4`.

## Accuracy (5-minute slice, stream 0)

Levenshtein similarity is computed against the Tesseract SRT for the
same slice.

| Engine | Cues (stream 1) | Empty warns (full) | Levenshtein vs Tess (avg) |
| --- | ---: | ---: | ---: |
| Legacy (bitmap passthrough) | N/A | N/A | N/A |
| Tesseract (LSTM) | 58 | 949 | 1.000 (baseline) |
| PP-OCRv4 (CPU + spacing) | 68 | 1 | 0.9414 |

## Troubleshooting (GPU 0% util)

- Run `./check_gpu_env.sh` to confirm whether `libcudnn.so` is
  discoverable and to see missing shared libraries for
  `libonnxruntime.so`.
- If cuDNN is missing, install the matching cuDNN package for your CUDA
  version and ensure it is on `LD_LIBRARY_PATH`/`ldconfig`.
- In containers, verify `NVIDIA_VISIBLE_DEVICES`,
  `NVIDIA_DRIVER_CAPABILITIES`, and that the NVIDIA Container Toolkit is
  installed and `--gpus all` is set.

## System stability note (Arch Linux)

- A previous full upgrade attempt led to downtime. We switched to ALA
  and installed only `cudnn` and `onnxruntime-cuda` without touching
  `glibc` or `gcc-libs`.
- The final legacy stack used for GPU tests was CUDA 11.8, cuDNN 8.4.1,
  and ONNX Runtime 1.14.1.
- `pacman -Qk` reported missing files under `bind` and permission errors
  on `/var/named/*.zone`. These should be corrected before further
  system updates.

## Top 5 longest lines (slice, stream 0)

Left = PP-OCRv4 output, Right = Tesseract output.

```text
1
left_time: 00:04:53,919 --> 00:04:59,380
left_len: 66
left_text: S it only here that there is such faith or in other villages, too?
right_time: 00:04:53,919 --> 00:04:59,380
right_len: 66
right_text: S it only here that there is such faith or in other villages, too?

2
left_time: 00:01:53,364 --> 00:01:58,529
left_len: 52
left_text: ‘100 dangerous. There are more executions than ever.
right_time: 00:01:53,364 --> 00:01:58,529
right_len: 52
right_text: ‘100 dangerous. There are more executions than ever.

3
left_time: 00:04:40,781 --> 00:04:44,365
left_len: 50
left_text: Every Christian here is part of our secret church.
right_time: 00:04:40,781 --> 00:04:44,365
right_len: 50
right_text: Every Christian here is part of our secret church.

4
left_time: 00:01:58,619 --> 00:02:01,986
left_len: 48
left_text: If they know we're Christian, we will be killed.
right_time: 00:01:58,619 --> 00:02:01,986
right_len: 48
right_text: If they know we're Christian, we will be killed.

5
left_time: 00:03:50,564 --> 00:03:54,056
left_len: 45
left_text: The only sacrament he can perform is baptism.
right_time: 00:03:50,564 --> 00:03:54,056
right_len: 45
right_text: The only sacrament he can perform is baptism.
```

## Repro (slice)

Slice creation:

```bash
ffmpeg -ss 00:15:00 -t 00:05:00 \
  -i "Silence (2016) Remux-1080p fixed.mp4" \
  -map 0:v -map 0:a -map 0:s -c copy silence_5min.mkv
```

PP-OCRv4 run (HEVC to avoid H.264 profile verification issues during
initial testing):

```bash
direct_play_nice \
  --streaming-devices chromecast_google_tv \
  --video-codec hevc \
  --video-quality match-source \
  --audio-quality match-source \
  --sub-mode force \
  --ocr-engine pp-ocr-v4 \
  --ocr-format srt \
  silence_5min.mkv ppocr_cpu_fallback_hevc.mp4
```

Tesseract run (same pipeline):

```bash
direct_play_nice \
  --streaming-devices chromecast_google_tv \
  --video-codec hevc \
  --video-quality match-source \
  --audio-quality match-source \
  --sub-mode force \
  --ocr-engine tesseract \
  --ocr-format srt \
  --delete-source=false \
  silence_5min.mkv tesseract_hevc.mp4
```
