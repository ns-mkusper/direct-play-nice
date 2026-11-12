#!/usr/bin/env python3
"""Fail closed on the stable CPU PP-OCRv3 fixture report and its prerequisites."""

import argparse
import hashlib
import json
import math
import struct
import sys
from pathlib import Path

# Keep these floors independent of the report, so changing fixture metadata cannot
# silently lower the CI bar. CER and WER are not bounded above by 1.
THRESHOLDS = {"eng": 0.90, "fra": 0.88, "spa": 0.88, "jpn": 0.50, "kor": 0.50}
# Independent copies of the anonymous fixture labels, not report-supplied values.
EXPECTED_TEXT = {
    "eng": "WE SHOULD LEAVE NOW",
    "fra": "NOUS DEVONS PARTIR",
    "spa": "DEBEMOS IRNOS AHORA",
    "jpn": "今出発しよう",
    "kor": "지금 떠나야 해",
}
# SHA-256 pins for every model the v3 initializer can load in CI.
MODELS = {
    "ch_PP-OCRv3_det_infer.onnx":
        "3439588c030faea393a54515f51e983d8e155b19a2e8aba7891934c1cf0de526",
    "ch_ppocr_mobile_v2.0_cls_train.onnx":
        "70581b300b83babd9e0dd1d7d74c5b006869e8796da277a70c2e405bf9d77c82",
    "en_PP-OCRv3_rec_infer.onnx":
        "ef7abd8bd3629ae57ea2c28b425c1bd258a871b93fd2fe7c433946ade9b5d9ea",
    "latin_PP-OCRv3_rec_mobile.onnx":
        "e9d7a33667e8aaa702862975186adf2012e3f390cc0f9422865957125f8071cf",
    "japan_PP-OCRv4_rec_mobile.onnx":
        "e1075a67dba758ecfc7ebc78a10ae61c95ac8fb66a9c86fab5541e33f085cb7a",
    "korean_PP-OCRv4_rec_mobile.onnx":
        "ab151ba9065eccd98f884cf4d927db091be86137276392072edd4f9d43ad7426",
    "chinese_cht_PP-OCRv3_rec_mobile.onnx":
        "779656d044ce388045e02ea9244724616194e63928606436cdfc6dc3c9528cc6",
}
PROFILES = {
    "latin": "latin_PP-OCRv3_rec_mobile.onnx",
    "japanese": "japan_PP-OCRv4_rec_mobile.onnx",
    "korean": "korean_PP-OCRv4_rec_mobile.onnx",
    "cjk": "chinese_cht_PP-OCRv3_rec_mobile.onnx",
}
# The fixture probe constructs one engine per evaluation mode.
PROBE_ENGINE_COUNT = 3


class ValidationError(ValueError):
    pass


def require(condition, message):
    if not condition:
        raise ValidationError(message)


def number(value, name, maximum=None):
    """Return ``value`` if it is a finite, non-negative int or float."""
    require(type(value) in (int, float) and math.isfinite(value), f"{name} must be finite numeric")
    require(value >= 0 and (maximum is None or value <= maximum), f"{name} is out of range")
    return value


def exact_int(value, expected, message):
    """Require an int (not a bool or float) equal to ``expected``."""
    require(type(value) is int and value == expected, message)


def sha256_file(path):
    with path.open("rb") as source:
        return hashlib.file_digest(source, "sha256").hexdigest()


def no_duplicate_keys(pairs):
    obj = {}
    for key, value in pairs:
        require(key not in obj, f"duplicate JSON key: {key}")
        obj[key] = value
    return obj


def check_finite(value):
    if isinstance(value, dict):
        value = list(value.values())
    if isinstance(value, list):
        for item in value:
            check_finite(item)
    elif type(value) is float:
        require(math.isfinite(value), "non-finite JSON number")


def load_report(path):
    with path.open(encoding="utf-8") as source:
        report = json.load(source, object_pairs_hook=no_duplicate_keys)
    check_finite(report)
    return report


def validate_prerequisites(model_dir, ort_path, log):
    require(ort_path.is_file(), "ONNX Runtime library is missing")
    for name, expected in MODELS.items():
        path = model_dir / name
        require(path.is_file(), f"required model missing: {name}")
        require(sha256_file(path) == expected, f"model hash mismatch: {name}")

    # The engine logs before each optional profile initialization and warns if it fails.
    require(log.count("Initializing PP-OCRv3 models (") == PROBE_ENGINE_COUNT,
            "missing PP-OCRv3 initialization")
    require("OCR execution provider order: cpu (GPU providers disabled)" in log,
            "CPU provider not confirmed")
    for profile, name in PROFILES.items():
        marker = f"Initializing PP-OCRv3 {profile} rec model at '{(model_dir / name).resolve()}'"
        require(log.count(marker) == PROBE_ENGINE_COUNT,
                f"missing {profile} recognizer initialization")
        require(f"{profile} rec model failed to initialize" not in log,
                f"{profile} recognizer fell back")


def float32(value):
    return struct.unpack("!f", struct.pack("!f", value))[0]


def strict_cer(expected, actual):
    """Mirror the Rust strict CER: scalar-value Levenshtein in f32 math."""
    previous = list(range(len(actual) + 1))
    for i, expected_char in enumerate(expected, 1):
        current = [i]
        for j, actual_char in enumerate(actual, 1):
            current.append(min(previous[j] + 1, current[j - 1] + 1,
                               previous[j - 1] + (expected_char != actual_char)))
        previous = current
    return float32(previous[-1] / len(expected))


def validate_fixture(fixture, seen):
    require(isinstance(fixture, dict), "invalid fixture")
    name = fixture.get("name")
    require(isinstance(name, str) and name in THRESHOLDS and name not in seen,
            f"unexpected or duplicate fixture: {name}")
    seen.add(name)
    require(fixture.get("language") == name, f"{name}: language mismatch")
    for key in ("expected_text", "actual_text"):
        require(isinstance(fixture.get(key), str) and fixture[key], f"{name}: missing {key}")
    require(fixture["expected_text"] == EXPECTED_TEXT[name], f"{name}: fixture label changed")

    # Recompute CER from the text so forged scores cannot pass.
    computed_cer = strict_cer(EXPECTED_TEXT[name], fixture["actual_text"])
    reported_cer = number(fixture.get("strict_cer"), f"{name}.strict_cer")
    require(float32(reported_cer) == computed_cer, f"{name}: strict CER disagrees with text")
    similarity = number(fixture.get("similarity"), f"{name}.similarity", 1)
    strict_cap = max(0.0, min(1.0, float32(1.0 - computed_cer)))
    require(float32(similarity) <= strict_cap, f"{name}: similarity exceeds strict CER cap")
    threshold = number(fixture.get("min_similarity"), f"{name}.min_similarity", 1)
    require(threshold >= THRESHOLDS[name], f"{name}: threshold below configured floor")
    require(similarity >= threshold and fixture.get("meets_threshold") is True,
            f"{name}: below threshold")
    require(fixture.get("used_tesseract") is False and "tesseract_quality" in fixture
            and fixture["tesseract_quality"] is None, f"{name}: Tesseract used")
    for key in ("cer", "wer"):
        number(fixture.get(key), f"{name}.{key}")
    require(type(fixture.get("infer_ms")) is int, f"{name}.infer_ms must be an integer")
    number(fixture["infer_ms"], f"{name}.infer_ms")
    for key in ("ppocr_quality", "ppocr_confidence"):
        number(fixture.get(key), f"{name}.{key}", 1)


def validate_language_summary(summary, seen):
    require(isinstance(summary, dict), "invalid language summary")
    name = summary.get("language")
    require(isinstance(name, str) and name in THRESHOLDS and name not in seen,
            "unexpected or duplicate language summary")
    seen.add(name)
    exact_int(summary.get("fixture_count"), 1, f"{name}: invalid language count")
    for key in ("avg_cer", "avg_wer", "avg_strict_cer"):
        number(summary.get(key), f"{name}.{key}")
    number(summary.get("avg_similarity"), f"{name}.avg_similarity", 1)
    require(number(summary.get("pass_rate"), f"{name}.pass_rate", 1) == 1,
            f"{name}: language pass rate")


def validate_report(report):
    """Require a perfect, internally consistent pure-onnx fixture result."""
    require(isinstance(report, dict) and report.get("engine") == "PP-OCRv3",
            "expected PP-OCRv3 report")
    modes = report.get("modes")
    require(isinstance(modes, list)
            and all(isinstance(m, dict) and isinstance(m.get("mode"), str) for m in modes),
            "invalid modes")
    pure = [mode for mode in modes if mode["mode"] == "pure-onnx"]
    require(len(pure) == 1, "expected exactly one pure-onnx mode")
    mode = pure[0]
    exact_int(mode.get("fixture_count"), len(THRESHOLDS), "invalid fixture count")
    exact_int(mode.get("tesseract_replacement_count"), 0,
              "Tesseract replacement in pure-onnx mode")
    for key in ("avg_cer", "avg_wer", "avg_strict_cer", "avg_infer_ms"):
        number(mode.get(key), key)
    number(mode.get("avg_similarity"), "avg_similarity", 1)
    require(number(mode.get("pass_rate"), "pass_rate", 1) == 1, "pure-onnx pass rate is not 100%")

    fixtures = mode.get("fixtures")
    require(isinstance(fixtures, list) and len(fixtures) == len(THRESHOLDS),
            "expected five fixtures")
    seen = set()
    for fixture in fixtures:
        validate_fixture(fixture, seen)
    require(seen == set(THRESHOLDS), "missing fixtures")

    summaries = mode.get("per_language")
    require(isinstance(summaries, list) and len(summaries) == len(THRESHOLDS),
            "invalid language summaries")
    seen = set()
    for summary in summaries:
        validate_language_summary(summary, seen)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("report", type=Path)
    parser.add_argument("log", type=Path)
    parser.add_argument("model_dir", type=Path)
    parser.add_argument("ort_library", type=Path)
    args = parser.parse_args()
    try:
        report = load_report(args.report)
        validate_prerequisites(args.model_dir, args.ort_library,
                               args.log.read_text(encoding="utf-8"))
        validate_report(report)
    except (OSError, UnicodeError, ValueError, OverflowError) as error:
        print(f"CPU OCR fixture validation failed: {error}", file=sys.stderr)
        return 1
    print("CPU pure-onnx fixture validation passed (5/5)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
