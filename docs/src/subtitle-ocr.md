# Subtitle OCR

Bitmap subtitle formats (PGS/VobSub/DVD) are not directly compatible with MP4
Direct Play in many client stacks. `direct_play_nice` can OCR bitmap subtitles
into text tracks using AI OCR backends (PP-OCR/Tesseract). This path is meant
for bitmap subtitle streams; text subtitles are muxed directly when compatible.

For official GPU architecture/provider references and compatibility links, see
[Hardware Acceleration](./hardware-acceleration.md).

## Defaults

- `--sub-mode auto`
- `--ocr-engine auto`
- `--ocr-format srt`

## Common overrides

- `--sub-mode skip` disable subtitle processing
- `--sub-mode force` force subtitle processing
- `--ocr-engine pp-ocr-v4` force PP-OCR v4 pipeline
- `--ocr-engine pp-ocr-v3` fallback for older GPU/runtime combinations
- `--ocr-format ass` request ASS (may be downgraded in MP4)
- `--ocr-write-srt-sidecar` write `.srt` sidecars in addition to embedded output

## GPU behavior

The OCR runtime attempts provider fallback when available (for example CUDA,
DirectML, CoreML, then CPU). You can force behavior with:

- `DPN_OCR_REQUIRE_GPU=1`
- `DPN_OCR_FORCE_CPU=1`

ONNX engines:

- `--ocr-engine pp-ocr-v4` for modern GPU/runtime stacks
- `--ocr-engine pp-ocr-v3` for legacy/older GPU compatibility cases

Linux runtime notes:

- Ensure CUDA/cuDNN and ONNX Runtime are version-compatible.
- `ORT_DYLIB_PATH=/path/to/libonnxruntime.so` can be used if ONNX Runtime is
  not discoverable on default library paths.
- For older NVIDIA stacks, `--ocr-engine pp-ocr-v3` can be more stable than
  `pp-ocr-v4`.
- Use `scripts/ocr-tools/check_gpu_env.sh` to inspect runtime/library setup.
- Containerized workloads may need NVIDIA Container Toolkit and exposed runtime
  libraries.

## Model location

Models are downloaded to a default model directory unless `DPN_OCR_MODEL_DIR`
is set.

Default model filenames:

- v4: `ch_PP-OCRv4_det_infer.onnx`, `ch_ppocr_mobile_v2.0_cls_infer.onnx`,
  `en_PP-OCRv4_rec_infer.onnx`
- v3: `ch_PP-OCRv3_det_infer.onnx`, `ch_ppocr_mobile_v2.0_cls_train.onnx`,
  `en_PP-OCRv3_rec_infer.onnx`

## Config-file example

```toml
sub_mode = "auto"           # auto | force | skip
ocr_default_language = "eng"
ocr_engine = "auto"         # auto | tesseract | pp-ocr-v3 | pp-ocr-v4 | external
ocr_format = "srt"          # srt | ass
ocr_write_srt_sidecar = false
ocr_external_command = "python3 /opt/ocr/run.py"
```
