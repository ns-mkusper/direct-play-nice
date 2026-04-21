# Hardware Acceleration

This chapter covers GPU acceleration in `direct_play_nice` for both:

- AI OCR of bitmap subtitles
- video transcoding

Support depends on your GPU hardware, driver stack, and how FFmpeg/ONNX Runtime
were built in your environment.

## OCR acceleration (bitmap subtitle AI OCR)

OCR acceleration is used for bitmap subtitle streams (PGS/VobSub/DVD) when OCR
is enabled. The runtime uses ONNX Runtime execution providers with fallback.

Typical provider order is:

- CUDA (NVIDIA, when available)
- DirectML (Windows)
- CoreML (Apple platforms)
- CPU fallback

### OCR architecture guidance

- NVIDIA: CUDA-capable GPUs supported by your CUDA Toolkit + cuDNN + ONNX
  Runtime build.
- Windows DirectML: DirectX 12-capable GPUs (vendor/driver dependent).
- Apple CoreML: Apple platform support depends on CoreML/ONNX Runtime provider
  compatibility.

### OCR official references

- ONNX Runtime execution providers overview:
  <https://onnxruntime.ai/docs/execution-providers/>
- ONNX Runtime CUDA EP:
  <https://onnxruntime.ai/docs/execution-providers/CUDA-ExecutionProvider.html>
- ONNX Runtime DirectML EP:
  <https://onnxruntime.ai/docs/execution-providers/DirectML-ExecutionProvider.html>
- ONNX Runtime CoreML EP:
  <https://onnxruntime.ai/docs/execution-providers/CoreML-ExecutionProvider.html>
- NVIDIA CUDA documentation:
  <https://docs.nvidia.com/cuda/>
- NVIDIA CUDA GPU compute capability list:
  <https://developer.nvidia.com/cuda-gpus/>
- Microsoft DirectML reference:
  <https://learn.microsoft.com/en-us/windows/ai/directml/directml-reference>

## Transcoding acceleration

`direct_play_nice` can use hardware-accelerated transcoding through FFmpeg when
available. You can inspect what your current build/runtime supports with:

```bash
direct_play_nice --probe-hw --probe-codecs --only-video --only-hw --probe-json
```

Common acceleration paths include:

- NVIDIA NVENC/NVDEC
- Intel Quick Sync Video (QSV)
- VAAPI (primarily Linux)
- AMD AMF
- Apple VideoToolbox

### Transcoding architecture guidance

- NVIDIA: see the NVIDIA Video Codec support matrix for exact GPU generation and
  codec-level support.
- Intel: see oneVPL supported hardware/runtime docs for QSV capability details.
- AMD: AMF support depends on GPU generation, driver, and platform support in
  FFmpeg and AMF.
- Apple: VideoToolbox support depends on macOS + hardware codec support.

### Transcoding official references

- FFmpeg CLI documentation:
  <https://ffmpeg.org/ffmpeg.html>
- FFmpeg codec documentation:
  <https://ffmpeg.org/ffmpeg-codecs.html>
- NVIDIA Video Codec SDK readme:
  <https://docs.nvidia.com/video-technologies/video-codec-sdk/13.0/read-me/index.html>
- NVIDIA encode/decode support matrix:
  <https://developer.nvidia.com/video-encode-and-decode-gpu-support-matrix>
- Intel oneVPL supported hardware:
  <https://www.intel.com/content/www/us/en/docs/onevpl/upgrade-from-msdk/2021-3/supported-hardware.html>
- AMD AMF SDK:
  <https://github.com/GPUOpen-LibrariesAndSDKs/AMF>
- Apple VideoToolbox:
  <https://developer.apple.com/documentation/videotoolbox>
