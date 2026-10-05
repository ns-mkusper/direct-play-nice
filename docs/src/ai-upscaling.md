# AI Upscaling

An opt-in super-resolution path for sources that sit below the resolution cap
you asked for. It is separate from the deterministic `--resize-quality`
kernels, which only ever shrink. It is off by default and stays off unless you
name a model.

```bash
direct_play_nice --device roku --video-quality 1080p \
  --ai-upscale-model realesr-animevideov3 input_480p.mkv output.mp4
```

## What it does

1. The direct-play assessment treats a source smaller than the active cap
   (`--video-quality` narrowed by the device limit) as needing conversion.
2. Each decoded frame is converted to RGB and run through an ONNX
   super-resolution model via ONNX Runtime. Built-in models enlarge 4x.
3. The enlarged frame goes through the normal libswscale fit to the exact
   target size using your `--resize-quality` kernel, then to the encoder.

Sources already at or above the cap are left alone and the model is never
loaded. A `--dry-run` reports the planned enlargement without touching the
model.

## Models

| Name | Architecture | Scale | License | Source |
| --- | --- | ---: | --- | --- |
| `realesr-animevideov3` | Real-ESRGAN SRVGGNetCompact, 16 conv | 4x | BSD-3-Clause | Hugging Face `skillsafe-ai/realesr-animevideov3` |
| `realesr-general-x4v3` | Real-ESRGAN SRVGGNetCompact, 32 conv | 4x | BSD-3-Clause | Hugging Face `CoderViking/realesr-general-x4v3-onnx` |
| `custom` | any single-image model | probed | yours | `--ai-upscale-model-path` |

Built-in models download on first use into the OCR model directory
(`DPN_OCR_MODEL_DIR`, or `models/` beside the binary, or
`~/.config/direct-play-nice/models`). `DPN_UPSCALE_MODEL_DIR` overrides the
location. Checksums are verified after download.

A custom model needs one float32 input shaped `[1, 3, height, width]` with RGB
values in `[0, 1]`, dynamic height and width, and one output of the same layout
enlarged by an integer factor. The factor is probed with a 16x16 frame at load
time, which also proves the execution provider works before any frame is
decoded.

## Devices and fallback

`--ai-upscale-device` controls where the model runs. There is no silent CPU
fallback:

- `auto` (default): the ONNX Runtime CUDA provider, then OpenVINO on an Intel
  GPU, then DirectML on Windows or CoreML on macOS when those builds are used.
  If none is available the run fails before any output is written and the
  message names the CPU flag.
- `cuda`: require the CUDA provider.
- `openvino`: require the OpenVINO provider on an Intel GPU. This needs an
  ONNX Runtime build that includes OpenVINO (the `onnxruntime-openvino`
  package ships one; point `ORT_DYLIB_PATH` at its `libonnxruntime.so` and put
  its directory on the library path) plus the Intel GPU compute runtime
  (`intel-opencl-icd` and `libze-intel-gpu1` on Ubuntu). On hosts with more
  than one GPU, `DPN_UPSCALE_OPENVINO_DEVICE=GPU.0` picks the Intel one.
- `cpu`: run on the CPU. Expect about one frame per second at 480p.

`--ai-upscale-tile <PIXELS>` splits each frame into overlapping tiles so large
frames fit in small GPU memory; `256` to `512` works for 2 to 4 GB cards.
Whole-frame 480p inference fits in 2 GB without tiling.

`--resize-backend cuda` is ignored for a stream that is being upscaled; the
model works on software frames.

## Hardware compatibility

By platform class, for the hardware media servers usually run on. The
execution provider column is what the shipped builds link: CUDA and OpenVINO
on Linux and Windows, DirectML on Windows, CoreML on macOS, CPU everywhere.
OpenVINO only becomes usable when the ONNX Runtime library `ORT_DYLIB_PATH`
points at was built with it. There is no ROCm provider, so AMD GPUs are
CPU-path only on Linux.

| Platform | Provider | AI upscale | Requirements and notes |
| --- | --- | --- | --- |
| NVIDIA Turing and newer (GTX 16xx, RTX 20/30/40/50) | CUDA | supported | Any ONNX Runtime CUDA build (1.16 to current) with matching CUDA and cuDNN libraries on the path. 2 GB of VRAM runs 480p whole frame; use `--ai-upscale-tile 256` for 720p sources on 2 to 4 GB cards. Validated on a Blackwell laptop GPU. |
| NVIDIA Pascal (GTX 10xx, Quadro P, P2000/P4000) | CUDA | supported, not validated | Needs an ONNX Runtime CUDA 12 build (1.17 to 1.26). The CUDA 13 builds that start at 1.27 drop compute capability below 7.5. |
| NVIDIA Maxwell (GTX 750/9xx, Quadro M) | CUDA | supported with old runtimes | Only ONNX Runtime builds that still carry sm_5x kernels work; 1.16 with CUDA 12 and cuDNN 8 is validated on a GTX 960. Newer builds fail fast with `cudaErrorNoKernelImageForDevice`. |
| NVIDIA Kepler and older (GTX 6xx/7xx) | none | CPU path only | No ONNX Runtime CUDA build targets these parts. |
| Intel iGPU and QuickSync boxes (N100, Celeron, Core UHD/Iris), Intel Arc | OpenVINO (Linux and Windows), DirectML on Windows | supported with an OpenVINO runtime | Needs an ONNX Runtime build with OpenVINO and the Intel GPU compute runtime; see `--ai-upscale-device openvino`. Validated on an Arrow Lake Xe iGPU at 4.5 fps model time for 480p 4x, about the same as a GTX 960. Entry-level iGPUs (N100, older UHD) will be slower. DirectML on Windows is compiled, not validated. |
| AMD Radeon and Ryzen APUs | CPU on Linux, DirectML on Windows | CPU path on Linux; DirectML compiled, not validated | The Linux build has no ROCm provider. DirectML on Windows is compiled, not validated. |
| Apple Silicon and Intel Macs | CoreML | compiled, not validated | The CoreML provider is linked and both macOS CI jobs pass; no hardware run yet. |
| x86 CPU only (NAS such as Synology, QNAP, Unraid boxes; servers without a GPU) | CPU | works, impractical | `--ai-upscale-device cpu` is required to opt in. Measured 0.3 to 0.9 fps at 480p. Fine for a short clip, not for a library. |
| ARM SBCs (Raspberry Pi 4/5, Rockchip) via the aarch64 build | CPU | works in principle, not recommended | Same CPU path with far less compute; expect well under 0.3 fps. |
| Docker image (`ghcr.io/ns-mkusper/direct-play-nice`) | CPU as shipped | CPU path only | The image bundles the CPU-only ONNX Runtime 1.16.3 tarball. For CUDA, mount an ONNX Runtime GPU build, point `ORT_DYLIB_PATH` at it, and run with `--gpus all`. Not validated. |

Whatever the platform, `--ai-upscale-device auto` either finds a GPU provider
or stops before writing anything, with a message naming the CPU flag.

### Validation runs

| Host | GPU | ONNX Runtime | Provider | Model time, 480p 4x | End to end |
| --- | --- | --- | --- | ---: | ---: |
| Laptop | NVIDIA RTX PRO 5000 Blackwell, 24 GB | 1.22, CUDA 12 / cuDNN 9 | CUDA | 39 ms/frame (26 fps) | 10 fps, 0.42x realtime |
| Laptop | same, `--ai-upscale-tile 256` | 1.22 | CUDA | 61 ms/frame (16 fps) | identical output |
| Laptop | same, CPU path | 1.22 | CPU | 1072 ms/frame (0.9 fps) | 0.9 fps, 0.04x |
| Media server | 2x NVIDIA GTX 960 Maxwell, 2 GB | 1.16, CUDA 12 / cuDNN 8 | CUDA | 269 ms/frame (3.7 fps) | 3.1 fps, 0.13x |
| Media server | same | 1.22, CUDA 12 / cuDNN 9 | CUDA | fails fast (no sm_52 kernels) | no output written |
| Media server | same, CPU path | 1.16 | CPU | 3.4 s/frame (0.3 fps) | 0.3 fps, 0.01x |

The 2x SPAN model (`custom`) reaches 19 fps end to end on the laptop (0.80x
realtime) because the deterministic fit from a 2x output is far cheaper than
from a 4x output; it is also the best live-action model in the benchmark.
Export it with `scripts/upscale-tools/export_sr_onnx.py` from the Apache-2.0
`spanx2_ch48` weights.

## Benchmark results

See [UPSCALE_BENCHMARK.md](https://github.com/ns-mkusper/direct-play-nice/tree/main/benches)
for the full tables. The short version:

- Fidelity metrics (PSNR, SSIM) against a clean 1080p reference do not favour
  super-resolution: lanczos and spline score as well or better on clean
  downscales, because perceptual models trade exact pixel agreement for
  sharpness.
- VMAF, which tracks perceived quality, favours the compact Real-ESRGAN models
  by several points on anime and holds even or slightly ahead on live action
  for the general model. On compression-degraded sources the gap widens.
- Throughput is far below the deterministic scalers. Treat this as a batch
  feature for sub-HD libraries, not something to run on every import.
- Picks: `realesr-animevideov3` for anime and for speed,
  `realesr-general-x4v3` for live action among the built-ins, and the 2x SPAN
  model through `custom` when you can export it (best live-action VMAF and the
  fastest end to end).

## Reproducing the benchmark

```bash
scripts/upscale-tools/run_upscale_benchmark.sh \
  --clip anime=clip_480p.mkv:clip_1080p.mkv \
  --models realesr-animevideov3,realesr-general-x4v3,custom:span-x2=/path/span_x2.onnx \
  --device cuda --hw-accel auto
```

The low-res clip needs an audio track (output validation expects AAC audio).
The script writes a CSV with wall time, FPS, realtime factor, output size,
PSNR, SSIM, and VMAF when the local ffmpeg has `libvmaf`.

## Models evaluated and not shipped

| Family | Why not built in |
| --- | --- |
| Official SPAN x2/x4 (Apache-2.0) | Best fidelity and best live-action VMAF of the set, and the 2x variant is the fastest end to end. No hosted ONNX with a stable checksum yet, so it ships as a `custom` recipe (`scripts/upscale-tools/export_sr_onnx.py`) until the file can be attached to a release. |
| EfRLFN (MIT, real-time SR paper 2026) | Fast, but scored below lanczos on VMAF for both clips. |
| RealPLKSR (MIT) | 20x slower than the compact models; fixed 512 or 256 pixel input. |
| NanoVSR (MIT, video-aware) | Needs a 15-frame window per pass; 9 fps at 480p on a laptop GPU and no VMAF gain over single-image models in this test. Worth revisiting for temporal stability. |
| AnimeJaNai, LiveAction SPAN | CC-BY-NC-SA licensed; usable through `custom` for personal libraries, not redistributed. |
| Diffusion video SR (SeedVR, STAR, Upscale-A-Video) | Seconds per frame; not viable for a transcoder. |
