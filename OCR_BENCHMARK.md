# OCR Benchmark — Silence (2016)

Scope and inputs
- Full‑movie runs (02:41:09) used for performance metrics: legacy, Tesseract, PP‑OCRv4 (`*_new2` runs).
- 5‑minute slice (00:15:00–00:20:00) used for accuracy and side‑by‑side text comparisons.
- GPU stats are from GPU0 `nvidia-smi` logs captured during full‑movie runs.

## Performance (full movie)

| Engine | Total Runtime (s) | Avg Power (W) | Peak Power (W) | GPU Util (avg/peak) |
| --- | ---: | ---: | ---: | ---: |
| Legacy (bitmap passthrough) | 3768.84 | 9.46 | 11.89 | 2.50% / 5.00% |
| Tesseract (LSTM) | 6401.81 | 14.47 | 26.83 | 0.60% / 6.00% |
| PP‑OCRv4 (ONNX Runtime) | 4628.22 | 9.47 | 11.89 | 2.50% / 5.00% |

Comparison summary
- PP‑OCRv4 (Auto) is ~27% faster than Tesseract with ~99.8% fewer empty‑text warnings.

Notes
- PP‑OCRv4 run on `plexserver` reported CUDA EP availability but GPU utilization remained ~0%, so inference appears to be CPU‑bound (likely missing CUDA/cuDNN runtime libraries). Use `DPN_OCR_REQUIRE_GPU=1` to fail fast and surface this condition.
- `check_gpu_env.sh` on `plexserver` (Arch Linux) confirmed `libcudnn.so` is missing; install `cudnn` (e.g., `sudo pacman -S cudnn`) so the CUDA EP can initialize.
- A 10‑second idle sample on `plexserver` measured GPU0 at ~11.84W peak/avg (query: `nvidia-smi --query-gpu=utilization.gpu,power.draw`). The older idle log in `silence_full/idle_power.csv` is invalid due to a bad query string.

## Accuracy (5‑minute slice, stream 0)

Levenshtein similarity is computed against the Tesseract SRT for the same slice.

| Engine | Cues Extracted (stream 0) | Empty Warnings (full run) | Levenshtein Similarity vs Tesseract (avg) |
| --- | ---: | ---: | ---: |
| Legacy (bitmap passthrough) | N/A | N/A | N/A |
| Tesseract (LSTM) | 49 | 949 | 1.000 (baseline) |
| PP‑OCRv4 + spacing fallback | 60 | 1 | 0.165 |

## Troubleshooting (GPU 0% util)

- Run `./check_gpu_env.sh` to confirm whether `libcudnn.so` is discoverable and to see missing shared libraries for `libonnxruntime.so`.
- If cuDNN is missing, install the matching cuDNN package for your CUDA version and ensure it is on `LD_LIBRARY_PATH`/`ldconfig`.
- In containers, verify `NVIDIA_VISIBLE_DEVICES`, `NVIDIA_DRIVER_CAPABILITIES`, and that the NVIDIA Container Toolkit is installed and `--gpus all` is set.

## Top 5 longest lines (slice, stream 0)

Left = PP‑OCRv4 output, Right = Tesseract output.

```
rank | left_time | left_len | left_text | right_time | right_len | right_text
1 | 00:04:53,919 --> 00:04:59,380 | 66 | S it only here that there is such faith or in other villages, too? | 00:04:53,919 --> 00:04:59,380 | 66 | S it only here that there is such faith or in other villages, too?
2 | 00:01:53,364 --> 00:01:58,529 | 52 | ‘100 dangerous. There are more executions than ever. | 00:01:53,364 --> 00:01:58,529 | 52 | ‘100 dangerous. There are more executions than ever.
3 | 00:04:40,781 --> 00:04:44,365 | 50 | Every Christian here is part of our secret church. | 00:04:40,781 --> 00:04:44,365 | 50 | Every Christian here is part of our secret church.
4 | 00:01:58,619 --> 00:02:01,986 | 48 | If they know we're Christian, we will be killed. | 00:01:58,619 --> 00:02:01,986 | 48 | If they know we're Christian, we will be killed.
5 | 00:03:50,564 --> 00:03:54,056 | 45 | The only sacrament he can perform is baptism. | 00:03:50,564 --> 00:03:54,056 | 45 | The only sacrament he can perform is baptism.
```

## Repro (slice)

- Slice creation:
  - `ffmpeg -ss 00:15:00 -t 00:05:00 -i "Silence (2016) Remux-1080p fixed.mp4" -map 0:v -map 0:a -map 0:s -c copy silence_5min.mkv`
- PP‑OCRv4 run (HEVC to avoid H.264 profile verification issues during testing):
  - `direct_play_nice --streaming-devices chromecast_google_tv --video-codec hevc --video-quality match-source --audio-quality match-source --sub-mode force --ocr-engine pp-ocr-v4 --ocr-format srt silence_5min.mkv ppocr_cpu_fallback_hevc.mp4`
- Tesseract run (same pipeline):
  - `direct_play_nice --streaming-devices chromecast_google_tv --video-codec hevc --video-quality match-source --audio-quality match-source --sub-mode force --ocr-engine tesseract --ocr-format srt --delete-source=false silence_5min.mkv tesseract_hevc.mp4`
