# AI Upscale Benchmark

Companion to the [AI Upscaling](../docs/src/ai-upscaling.md) chapter and issue #111.
Two 20-second clips were cut from library media, downscaled to 854x480 with
lanczos, and upscaled back to 1920x1080 by every candidate. Scores compare the
upscaled output with the original 1080p frames. The clean downscale is the
kindest case for deterministic kernels; the crf 30 variants add the compression
damage real sub-HD sources carry. Hosts are anonymised as in the OCR report.

- Metrics: PSNR and SSIM from ffmpeg, VMAF from libvmaf (mean over 240 frames)
- Model fps: inference only, Python ONNX Runtime 1.22 CUDA provider, laptop GPU
- The GAN-trained Real-ESRGAN models lower PSNR by design; read VMAF for
  perceived quality

## Model survey

### Anime (Mary and the Witch's Flower), clean lanczos downscale to 854x480

| Method | PSNR | SSIM | VMAF | Model fps |
| --- | ---: | ---: | ---: | ---: |
| ffmpeg lanczos (baseline) | 34.96 | 0.9659 | 76.6 | - |
| ffmpeg spline | 34.97 | 0.9662 | 75.7 | - |
| realesr-animevideov3 (built in) | 33.47 | 0.9631 | 82.8 | 25.6 |
| realesr-general-x4v3 (built in) | 32.74 | 0.9590 | 83.3 | 14.8 |
| realesr-general-wdn-x4v3 | 33.51 | 0.9604 | 80.9 | 15.0 |
| SPAN x2 official | 34.24 | 0.9648 | 79.6 | 27.9 |
| SPAN x4 official | 34.16 | 0.9643 | 80.7 | 22.0 |
| 2x LiveAction SPAN (NC licence) | 33.97 | 0.9634 | 83.0 | 28.2 |
| 2x ModernSpanimation SPAN | 33.38 | 0.9566 | 82.6 | 21.6 |
| EfRLFN x2 | 33.98 | 0.9611 | 69.2 | 18.5 |
| EfRLFN x4 | 34.76 | 0.9629 | 68.5 | 14.9 |
| NanoVSR 644k (15-frame window) | 34.18 | 0.9646 | 79.8 | 9.3 |

### Live action (Silence), clean lanczos downscale to 854x480

| Method | PSNR | SSIM | VMAF | Model fps |
| --- | ---: | ---: | ---: | ---: |
| ffmpeg lanczos (baseline) | 36.50 | 0.9667 | 82.1 | - |
| ffmpeg spline | 36.51 | 0.9668 | 81.1 | - |
| realesr-animevideov3 (built in) | 36.41 | 0.9636 | 78.7 | 26.4 |
| realesr-general-x4v3 (built in) | 35.50 | 0.9575 | 82.8 | 14.4 |
| realesr-general-wdn-x4v3 | 35.95 | 0.9559 | 76.8 | 14.6 |
| SPAN x2 official | 37.24 | 0.9692 | n/a | 21.3 |
| SPAN x4 official | 37.47 | 0.9691 | n/a | 23.7 |
| 2x LiveAction SPAN (NC licence) | 36.61 | 0.9658 | 85.4 | 28.8 |
| 2x ModernSpanimation SPAN | 36.00 | 0.9607 | 80.9 | 21.8 |
| EfRLFN x2 | 36.10 | 0.9645 | 71.0 | 18.5 |
| EfRLFN x4 | 35.84 | 0.9645 | 72.7 | 16.4 |
| NanoVSR 644k (15-frame window) | 37.27 | 0.9683 | n/a | 12.4 |

### Anime, 480p re-encoded with x264 crf 30 (compression artefacts)

| Method | PSNR | SSIM | VMAF | Model fps |
| --- | ---: | ---: | ---: | ---: |
| ffmpeg lanczos (baseline) | 33.90 | 0.9466 | 53.7 | - |
| ffmpeg spline | 33.91 | 0.9468 | 53.1 | - |
| realesr-animevideov3 (built in) | 32.81 | 0.9526 | 68.8 | 25.6 |
| realesr-general-x4v3 (built in) | 32.54 | 0.9486 | 68.3 | 14.9 |
| realesr-general-wdn-x4v3 | 33.20 | 0.9497 | 63.8 | 14.7 |
| SPAN x2 official | 33.28 | 0.9450 | 55.1 | 28.2 |
| SPAN x4 official | 33.22 | 0.9447 | 56.1 | 23.8 |
| 2x LiveAction SPAN (NC licence) | 33.51 | 0.9510 | 56.9 | 28.2 |
| NanoVSR 644k (15-frame window) | 33.27 | 0.9452 | 55.3 | 12.4 |

### Live action, 480p re-encoded with x264 crf 30

| Method | PSNR | SSIM | VMAF | Model fps |
| --- | ---: | ---: | ---: | ---: |
| ffmpeg lanczos (baseline) | 35.03 | 0.9354 | 63.8 | - |
| ffmpeg spline | 35.04 | 0.9355 | n/a | - |
| realesr-animevideov3 (built in) | 35.14 | 0.9374 | n/a | 26.1 |
| realesr-general-x4v3 (built in) | 34.51 | 0.9317 | n/a | 14.8 |
| realesr-general-wdn-x4v3 | 35.09 | 0.9331 | n/a | 15.1 |
| SPAN x2 official | 35.43 | 0.9369 | n/a | 29.2 |
| SPAN x4 official | 35.58 | 0.9368 | n/a | 24.0 |
| 2x LiveAction SPAN (NC licence) | 35.31 | 0.9383 | n/a | 29.2 |
| NanoVSR 644k (15-frame window) | 35.41 | 0.9362 | 64.8 | 11.9 |

## direct_play_nice end to end

`scripts/upscale-tools/run_upscale_benchmark.sh` drives the real binary: decode,
model, deterministic fit to 1920x1080, encode (`--hw-accel auto`, NVENC on both
hosts), output validation. Elapsed time includes model load. Baselines are
ffmpeg `scale` with libx264 preset fast, which is why they are faster than any
DPN run, including a DPN run without AI upscaling.

### Laptop, CPU provider (explicit `--ai-upscale-device cpu`), 3-second clip

| Clip | Method | Elapsed s | FPS | Realtime | Output MB | PSNR | SSIM | VMAF |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| anime3s | ffmpeg-lanczos | 0.8 | 88.1 | 3.68x | 2.5 | 32.91 | 0.9429 | 70.18 |
| anime3s | ffmpeg-spline | 0.8 | 85.7 | 3.57x | 2.5 | 32.93 | 0.9431 | 69.55 |
| anime3s | dpn-realesr-animevideov3 | 77.8 | 0.9 | 0.04x | 2.8 | 30.11 | 0.9324 | 66.40 |

### Laptop, NVIDIA RTX PRO 5000 Blackwell 24 GB, CUDA provider, ONNX Runtime 1.22

| Clip | Method | Elapsed s | FPS | Realtime | Output MB | PSNR | SSIM | VMAF |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| anime_mary | ffmpeg-lanczos | 2.1 | 116.7 | 4.87x | 8.2 | 34.95 | 0.9661 | 76.09 |
| anime_mary | ffmpeg-spline | 2.1 | 114.1 | 4.76x | 8.1 | 34.97 | 0.9663 | 75.19 |
| anime_mary | dpn-realesr-animevideov3 | 46.4 | 10.2 | 0.42x | 20.5 | 33.22 | 0.9625 | 82.99 |
| anime_mary | dpn-realesr-general-x4v3 | 60.8 | 7.8 | 0.32x | 20.2 | 32.87 | 0.9575 | 83.87 |
| anime_mary | dpn-span-x2-ch48 | 24.5 | 19.3 | 0.80x | 20.3 | 35.09 | 0.9659 | 81.38 |
| anime_mary | dpn-span-x4-ch48 | 47.1 | 10.0 | 0.42x | 20.3 | 35.00 | 0.9657 | 81.27 |
| live_silence | ffmpeg-lanczos | 1.6 | 148.2 | 6.18x | 5.7 | 36.48 | 0.9656 | 81.58 |
| live_silence | ffmpeg-spline | 1.6 | 149.4 | 6.23x | 5.6 | 36.47 | 0.9657 | 80.48 |
| live_silence | dpn-realesr-animevideov3 | 44.9 | 10.6 | 0.44x | 20.4 | 35.99 | 0.9630 | 81.20 |
| live_silence | dpn-realesr-general-x4v3 | 59.7 | 7.9 | 0.33x | 20.2 | 35.38 | 0.9550 | 85.56 |
| live_silence | dpn-span-x2-ch48 | 24.8 | 19.1 | 0.80x | 20.2 | 37.37 | 0.9684 | 87.79 |
| live_silence | dpn-span-x4-ch48 | 48.3 | 9.8 | 0.41x | 20.2 | 37.60 | 0.9685 | 87.63 |

### Laptop, Intel Arrow Lake Xe iGPU, OpenVINO provider (onnxruntime-openvino 1.24), NVENC encode

| Clip | Method | Elapsed s | FPS | Realtime | Output MB | PSNR | SSIM | VMAF |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| anime_mary | ffmpeg-lanczos | 1.7 | 138.7 | 5.78x | 8.2 | 34.95 | 0.9661 | 76.09 |
| anime_mary | ffmpeg-spline | 1.8 | 133.8 | 5.58x | 8.1 | 34.97 | 0.9663 | 75.19 |
| anime_mary | dpn-realesr-animevideov3 | 128.9 | 3.7 | 0.15x | 20.5 | 33.22 | 0.9625 | 82.96 |
| anime_mary | dpn-realesr-general-x4v3 | 214.1 | 2.2 | 0.09x | 20.2 | 32.87 | 0.9575 | 83.84 |
| live_silence | ffmpeg-lanczos | 1.4 | 170.7 | 7.12x | 5.7 | 36.48 | 0.9656 | 81.58 |
| live_silence | ffmpeg-spline | 1.4 | 166.1 | 6.93x | 5.6 | 36.47 | 0.9657 | 80.48 |
| live_silence | dpn-realesr-animevideov3 | 126.1 | 3.8 | 0.16x | 20.4 | 36.01 | 0.9630 | 81.33 |
| live_silence | dpn-realesr-general-x4v3 | 199.5 | 2.4 | 0.10x | 20.2 | 35.40 | 0.9550 | 85.43 |

### Laptop, CUDA provider with `--ai-upscale-tile 256`, 3-second clip

| Clip | Method | Elapsed s | FPS | Realtime | Output MB | PSNR | SSIM | VMAF |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| anime3s | ffmpeg-lanczos | 0.8 | 92.2 | 3.85x | 2.5 | 32.91 | 0.9429 | 70.18 |
| anime3s | ffmpeg-spline | 0.8 | 91.8 | 3.83x | 2.5 | 32.93 | 0.9431 | 69.55 |
| anime3s | dpn-realesr-animevideov3 | 10.2 | 6.7 | 0.28x | 2.8 | 30.11 | 0.9324 | 66.39 |

### Media server, CPU provider, 3-second clip

| Clip | Method | Elapsed s | FPS | Realtime | Output MB | PSNR | SSIM | VMAF |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| anime3s | ffmpeg-lanczos | 2.1 | 34.1 | 1.42x | 2.6 | 32.91 | 0.9424 | 71.17 |
| anime3s | ffmpeg-spline | 2.0 | 35.1 | 1.47x | 2.5 | 32.93 | 0.9426 | 70.50 |
| anime3s | dpn-realesr-animevideov3 | 234.1 | 0.3 | 0.01x | 2.8 | 28.64 | 0.9285 | 67.32 |

### Media server, NVIDIA GTX 960 Maxwell 2 GB, CUDA provider, ONNX Runtime 1.16 + cuDNN 8

| Clip | Method | Elapsed s | FPS | Realtime | Output MB | PSNR | SSIM | VMAF |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| anime_mary | ffmpeg-lanczos | 5.9 | 40.6 | 1.69x | 8.2 | 34.96 | 0.9660 | 76.10 |
| anime_mary | ffmpeg-spline | 5.9 | 40.5 | 1.69x | 8.1 | 34.97 | 0.9662 | 75.21 |
| anime_mary | dpn-realesr-animevideov3 | 153.1 | 3.1 | 0.13x | 20.3 | 31.69 | 0.9603 | 82.99 |
| anime_mary | dpn-realesr-general-x4v3 | 264.2 | 1.8 | 0.07x | 20.2 | 32.05 | 0.9557 | 83.87 |
| anime_mary | dpn-span-x2-ch48 | 143.4 | 3.3 | 0.14x | 20.2 | 33.93 | 0.9637 | 81.31 |
| anime_mary | dpn-span-x4-ch48 | 170.6 | 2.8 | 0.12x | 20.2 | 33.87 | 0.9635 | 81.17 |
| live_silence | ffmpeg-lanczos | 4.2 | 57.1 | 2.38x | 5.7 | 36.47 | 0.9656 | 81.50 |
| live_silence | ffmpeg-spline | 4.2 | 57.6 | 2.40x | 5.6 | 36.50 | 0.9657 | 80.45 |
| live_silence | dpn-realesr-animevideov3 | 153.5 | 3.1 | 0.13x | 20.1 | 35.99 | 0.9629 | 81.30 |
| live_silence | dpn-realesr-general-x4v3 | 264.9 | 1.8 | 0.07x | 19.9 | 35.36 | 0.9549 | 85.67 |
| live_silence | dpn-span-x2-ch48 | 143.7 | 3.3 | 0.14x | 20.2 | 37.36 | 0.9681 | 87.85 |
| live_silence | dpn-span-x4-ch48 | 173.7 | 2.7 | 0.11x | 20.2 | 37.59 | 0.9681 | 87.64 |

### Mac mini, Apple M4 Pro, CoreML provider (ONNX Runtime 1.22 macOS arm64), VideoToolbox encode

| Clip | Method | Elapsed s | FPS | Realtime | Output MB | PSNR | SSIM | VMAF |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| anime_mary | ffmpeg-lanczos | 1.5 | 165.3 | 6.89x | 8.2 | 34.82 | 0.9658 | 75.77 |
| anime_mary | ffmpeg-spline | 1.4 | 165.8 | 6.91x | 8.1 | 34.83 | 0.9660 | 74.88 |
| anime_mary | dpn-realesr-animevideov3 | 149.2 | 3.2 | 0.13x | 9.9 | 32.60 | 0.9606 | 82.31 |
| anime_mary | dpn-realesr-general-x4v3 | 274.5 | 1.7 | 0.07x | 11.8 | 32.35 | 0.9562 | 83.25 |
| anime_mary | dpn-span-x2-ch48 | 214.9 | 2.2 | 0.09x | 10.7 | 34.17 | 0.9639 | 80.13 |
| live_silence | ffmpeg-lanczos | 1.1 | 208.7 | 8.70x | 5.7 | 36.48 | 0.9655 | 81.32 |
| live_silence | ffmpeg-spline | 1.2 | 202.0 | 8.43x | 5.6 | 36.47 | 0.9656 | 80.38 |
| live_silence | dpn-realesr-animevideov3 | 151.1 | 3.2 | 0.13x | 11.9 | 36.42 | 0.9614 | 80.04 |
| live_silence | dpn-realesr-general-x4v3 | 272.0 | 1.8 | 0.07x | 12.1 | 35.12 | 0.9535 | 84.12 |
| live_silence | dpn-span-x2-ch48 | 215.4 | 2.2 | 0.09x | 14.1 | 37.10 | 0.9669 | 85.82 |

### Mac mini, Apple M4 Pro, CPU provider, 3-second clip

| Clip | Method | Elapsed s | FPS | Realtime | Output MB | PSNR | SSIM | VMAF |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| anime3s | ffmpeg-lanczos | 0.6 | 121.6 | 5.07x | 2.6 | 32.46 | 0.9419 | 70.22 |
| anime3s | ffmpeg-spline | 0.6 | 119.6 | 4.99x | 2.5 | 32.47 | 0.9421 | 69.53 |
| anime3s | dpn-realesr-animevideov3 | 53.0 | 1.4 | 0.06x | 1.9 | 30.16 | 0.9318 | 68.97 |

## Reading the numbers

- On clean downscales the deterministic kernels already sit near the fidelity
  ceiling: no model beats lanczos on PSNR for anime, and only the fidelity-trained
  SPAN models do for live action. VMAF tells a different story: the compact
  Real-ESRGAN models and SPAN gain 3 to 7 points on anime, and the general
  Real-ESRGAN model holds a small VMAF lead on live action.
- On compression-damaged anime the Real-ESRGAN models gain about 15 VMAF points
  over lanczos while also raising SSIM; that is the case the feature is for.
- `realesr-animevideov3` is the throughput choice and the anime choice;
  `realesr-general-x4v3` is the live-action choice at roughly half the model fps.
- The video-aware NanoVSR did not score above the single-image models here and
  needs a 15-frame lookahead; EfRLFN is fast but blurs (VMAF below bilinear).
- End to end, the laptop GPU reaches 0.4x realtime with the 4x models and 0.8x
  with a 2x model; the Maxwell server is several times slower. CPU inference is
  about one frame per second. Treat AI upscaling as a batch job for sub-HD
  libraries, not an import-time default.
