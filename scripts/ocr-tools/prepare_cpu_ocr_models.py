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
        (MODELSCOPE, ("japan_PP-OCRv4_rec_mobile.onnx", "korean_PP-OCRv4_rec_mobile.onnx",
                      "chinese_cht_PP-OCRv3_rec_mobile.onnx")),
    )
    for name in names
}
# Upstream ships IR v10, which ORT 1.16 rejects. As in models.rs, verify the upstream
# bytes, then lower only the leading ir_version varint; MODELS pins the result.
URLS["latin_PP-OCRv5_rec_mobile_ir9.onnx"] = (
    "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/v3.8.0/"
    "onnx/PP-OCRv5/rec/latin_PP-OCRv5_rec_mobile.onnx")
IR10_UPSTREAM = {
    "latin_PP-OCRv5_rec_mobile_ir9.onnx":
        "b20bd37c168a570f583afbc8cd7925603890efbcdc000a59e22c269d160b5f5a",
}


def lower_ir_version(path):
    """Rewrite a leading `ir_version: 10` field to 9 in place."""
    data = bytearray(path.read_bytes())
    if data[:2] != b"\x08\x0a":
        raise ValueError(f"unexpected ir_version encoding: {path.name}")
    data[1] = 9
    path.write_bytes(data)


def prepare_models(destination):
    """Download and verify every pinned model; never reuse an existing directory."""
    if set(URLS) != set(MODELS):
        raise ValueError("model URLs and pinned hashes do not match")
    destination.mkdir()
    for name, expected in MODELS.items():
        path = destination / name
        subprocess.run(["curl", "--fail", "--location", "--retry", "3", "--silent",
                        "--show-error", "--output", str(path), URLS[name]], check=True)
        upstream = IR10_UPSTREAM.get(name)
        if upstream is not None:
            if sha256_file(path) != upstream:
                raise ValueError(f"upstream model hash mismatch: {name}")
            lower_ir_version(path)
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
