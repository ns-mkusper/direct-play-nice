# Hardware Acceleration

This chapter documents acceleration paths actually implemented in
`direct_play_nice`:

- AI OCR for bitmap subtitle streams (PGS/VobSub/DVD)
- H.264/HEVC hardware transcoding via FFmpeg encoders

## OCR acceleration (bitmap subtitles)

`direct_play_nice` uses ONNX Runtime providers for PP-OCR and has explicit
legacy-NVIDIA logic in `auto` mode.

### What is supported in this project

- NVIDIA CUDA path for PP-OCRv3/PP-OCRv4 (primary validated path)
- Legacy NVIDIA behavior: if `nvidia-smi` reports compute capability major
  `<= 5` (Maxwell-class and older), `--ocr-engine auto` prefers `pp-ocr-v3`
  and disables classifier for stability
- Windows DirectML and Apple CoreML provider paths are wired and can be used
  when runtimes are installed[^ort-cuda-ep][^ort-directml-ep][^ort-coreml-ep]
- CPU fallback is available (or forced with `DPN_OCR_FORCE_CPU=1`)

### OCR workload guidance by hardware class

- Older NVIDIA families (Maxwell/Pascal-era systems): prefer `--ocr-engine pp-ocr-v3`[^nvidia-cuda-gpus]
- Newer NVIDIA families (Turing/Ampere/Ada): start with `--ocr-engine pp-ocr-v4`
- Non-NVIDIA GPUs: use `auto` and verify provider availability with probe logs;
  if providers are unavailable, OCR falls back to CPU/Tesseract path

### OCR performance and validation artifacts

- Full-movie OCR benchmark (self-hosted Linux, PP-OCRv3 GPU-required profile):
  `87.62 FPS`, `3.65x` realtime
  - [OCR benchmark report](../../benches/OCR_BENCHMARK.md)
- OCR AI/GPU paths are covered in integration tests:
  - [AI OCR stream coverage test](../../tests/cli_bitmap_subs.rs)
  - [Multilingual OCR accuracy/perf stress](../../tests/cli_ocr_multilang_stress.rs)

## Transcoding acceleration

`direct_play_nice` hardware encoder selection is currently targeted at H.264
and HEVC output.

### Codec and hardware mapping implemented by the CLI

- H.264 hardware encoders: `h264_nvenc`, `h264_qsv`, `h264_vaapi`,
  `h264_videotoolbox`, `h264_amf`[^ffmpeg-nvenc][^ffmpeg-qsv][^ffmpeg-vaapi][^ffmpeg-videotoolbox][^ffmpeg-amf]
- HEVC hardware encoders: `hevc_nvenc`, `hevc_qsv`, `hevc_vaapi`,
  `hevc_videotoolbox`, `hevc_amf`[^ffmpeg-nvenc][^ffmpeg-qsv][^ffmpeg-vaapi][^ffmpeg-videotoolbox][^ffmpeg-amf]
- Backend availability is OS/build dependent and discovered at runtime[^nvidia-codec-matrix][^intel-onevpl][^amd-amf-sdk][^apple-videotoolbox]

You can inspect your current host/build support with:

```bash
direct_play_nice --probe-hw --probe-codecs --only-video --only-hw --probe-json
```

### Transcoding performance and validation artifacts

- NVENC end-to-end matrix test validates profile/level/bitrate/device behavior:
  - [NVENC matrix test](../../tests/nvenc_matrix.rs)
- NVENC regression tests:
  - [Profile/level integration test](../../tests/nvenc_integration.rs)
  - [Duration-preservation regression](../../tests/nvenc_duration.rs)
- Practical performance benefit is lower CPU pressure and higher conversion
  concurrency on hosts with working hardware encoders.

[^ort-cuda-ep]: ONNX Runtime CUDA execution provider: <https://onnxruntime.ai/docs/execution-providers/CUDA-ExecutionProvider.html>.
[^ort-directml-ep]: ONNX Runtime DirectML execution provider: <https://onnxruntime.ai/docs/execution-providers/DirectML-ExecutionProvider.html>.
[^ort-coreml-ep]: ONNX Runtime CoreML execution provider: <https://onnxruntime.ai/docs/execution-providers/CoreML-ExecutionProvider.html>.
[^nvidia-cuda-gpus]: NVIDIA CUDA GPU compute capability list: <https://developer.nvidia.com/cuda-gpus/>.
[^ffmpeg-nvenc]: FFmpeg `h264_nvenc` / `hevc_nvenc` encoder options: <https://ffmpeg.org/ffmpeg-codecs.html#9_002e29-nvenc>.
[^ffmpeg-qsv]: FFmpeg `h264_qsv` / `hevc_qsv` encoder options: <https://ffmpeg.org/ffmpeg-codecs.html#QSV-Encoders>.
[^ffmpeg-vaapi]: FFmpeg `h264_vaapi` / `hevc_vaapi` encoder options: <https://ffmpeg.org/ffmpeg-codecs.html#VAAPI-encoders>.
[^ffmpeg-videotoolbox]: FFmpeg `h264_videotoolbox` / `hevc_videotoolbox` encoder options: <https://ffmpeg.org/ffmpeg-codecs.html#VideoToolbox-encoders>.
[^ffmpeg-amf]: FFmpeg `h264_amf` / `hevc_amf` encoder options: <https://ffmpeg.org/ffmpeg-codecs.html#AMD-AMF-Video-encoders>.
[^nvidia-codec-matrix]: NVIDIA Video encode/decode support matrix: <https://developer.nvidia.com/video-encode-and-decode-gpu-support-matrix>.
[^intel-onevpl]: Intel oneVPL supported hardware: <https://www.intel.com/content/www/us/en/docs/onevpl/upgrade-from-msdk/2021-3/supported-hardware.html>.
[^amd-amf-sdk]: AMD AMF SDK: <https://github.com/GPUOpen-LibrariesAndSDKs/AMF>.
[^apple-videotoolbox]: Apple VideoToolbox framework: <https://developer.apple.com/documentation/videotoolbox>.
