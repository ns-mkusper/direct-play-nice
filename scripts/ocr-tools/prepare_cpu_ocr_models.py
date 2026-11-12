#!/usr/bin/env python3
"""Download pinned CI OCR models into a new, isolated directory."""

import argparse
import hashlib
import subprocess
import sys
from pathlib import Path

from validate_cpu_fixture_report import MODELS

# Same model sources as src/subtitle_ocr/engine/models.rs. The independent
# hashes in MODELS are verified before any CI test can initialize an engine.
URLS = {
    "ch_PP-OCRv3_det_infer.onnx": "https://huggingface.co/SWHL/RapidOCR/resolve/main/PP-OCRv3/ch_PP-OCRv3_det_infer.onnx?download=true",
    "ch_ppocr_mobile_v2.0_cls_train.onnx": "https://huggingface.co/SWHL/RapidOCR/resolve/main/PP-OCRv3/ch_ppocr_mobile_v2.0_cls_train.onnx?download=true",
    "en_PP-OCRv3_rec_infer.onnx": "https://huggingface.co/SWHL/RapidOCR/resolve/main/PP-OCRv3/en_PP-OCRv3_rec_infer.onnx?download=true",
    "latin_PP-OCRv3_rec_mobile.onnx": "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/master/onnx/PP-OCRv4/rec/latin_PP-OCRv3_rec_mobile.onnx",
    "japan_PP-OCRv4_rec_mobile.onnx": "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/master/onnx/PP-OCRv4/rec/japan_PP-OCRv4_rec_mobile.onnx",
    "korean_PP-OCRv4_rec_mobile.onnx": "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/master/onnx/PP-OCRv4/rec/korean_PP-OCRv4_rec_mobile.onnx",
    "chinese_cht_PP-OCRv3_rec_mobile.onnx": "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/master/onnx/PP-OCRv4/rec/chinese_cht_PP-OCRv3_rec_mobile.onnx",
}


def prepare_models(destination):
    if set(URLS) != set(MODELS):
        raise ValueError("model URLs and pinned hashes do not match")
    # Never reuse, overwrite, or remove an existing directory or cached model.
    destination.mkdir()
    for name, expected in MODELS.items():
        path = destination / name
        subprocess.run(
            ["curl", "--fail", "--location", "--retry", "3", "--silent", "--show-error", "--output", str(path), URLS[name]],
            check=True,
        )
        with path.open("rb") as model:
            actual = hashlib.file_digest(model, "sha256").hexdigest()
        if actual != expected:
            raise ValueError(f"model hash mismatch: {name}")
        print(f"Verified {name}", flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("destination", type=Path)
    args = parser.parse_args()
    try:
        prepare_models(args.destination)
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        print(f"CPU OCR model preparation failed: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
