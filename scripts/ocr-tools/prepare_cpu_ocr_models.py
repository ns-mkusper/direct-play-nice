#!/usr/bin/env python3
"""Download pinned CI OCR models into a new, isolated directory."""

import argparse
import subprocess
import sys
from pathlib import Path

from validate_cpu_fixture_report import MODELS, sha256_file

# Same model sources as src/subtitle_ocr/engine/models.rs. The independent
# hashes in MODELS are verified before any CI test can initialize an engine.
HUGGINGFACE = "https://huggingface.co/SWHL/RapidOCR/resolve/main/PP-OCRv3/{}?download=true"
MODELSCOPE = ("https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/master/"
              "onnx/PP-OCRv4/rec/{}")
URLS = {
    name: source.format(name)
    for source, names in (
        (HUGGINGFACE, ("ch_PP-OCRv3_det_infer.onnx", "ch_ppocr_mobile_v2.0_cls_train.onnx",
                       "en_PP-OCRv3_rec_infer.onnx")),
        (MODELSCOPE, ("latin_PP-OCRv3_rec_mobile.onnx", "japan_PP-OCRv4_rec_mobile.onnx",
                      "korean_PP-OCRv4_rec_mobile.onnx", "chinese_cht_PP-OCRv3_rec_mobile.onnx")),
    )
    for name in names
}


def prepare_models(destination):
    """Download and verify every pinned model; never reuse an existing directory."""
    if set(URLS) != set(MODELS):
        raise ValueError("model URLs and pinned hashes do not match")
    destination.mkdir()
    for name, expected in MODELS.items():
        path = destination / name
        subprocess.run(["curl", "--fail", "--location", "--retry", "3", "--silent",
                        "--show-error", "--output", str(path), URLS[name]], check=True)
        if sha256_file(path) != expected:
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
