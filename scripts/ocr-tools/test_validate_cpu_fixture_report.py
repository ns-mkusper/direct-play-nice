import copy
import hashlib
import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

import prepare_cpu_ocr_models as prepare
import validate_cpu_fixture_report as validator


def passing_report():
    fixtures = []
    languages = []
    for name, threshold in validator.THRESHOLDS.items():
        fixtures.append({
            "name": name, "language": name, "expected_text": validator.EXPECTED_TEXT[name],
            "actual_text": validator.EXPECTED_TEXT[name],
            "infer_ms": 1, "cer": 0.0, "wer": 0.0, "strict_cer": 0.0,
            "similarity": 1.0, "min_similarity": threshold, "meets_threshold": True,
            "used_tesseract": False, "ppocr_quality": 1.0, "ppocr_confidence": 0.9,
            "tesseract_quality": None,
        })
        languages.append({
            "language": name, "fixture_count": 1, "avg_cer": 0.0, "avg_wer": 0.0,
            "avg_strict_cer": 0.0, "avg_similarity": 1.0, "pass_rate": 1.0,
        })
    mode = {
        "mode": "pure-onnx", "fixture_count": 5, "avg_cer": 0.0, "avg_wer": 0.0,
        "avg_strict_cer": 0.0, "avg_similarity": 1.0, "pass_rate": 1.0,
        "avg_infer_ms": 1.0, "tesseract_replacement_count": 0,
        "per_language": languages, "fixtures": fixtures,
    }
    return {"engine": "PP-OCRv3", "modes": [{"mode": "hybrid"}, mode, {"mode": "strict-gain"}]}


class ReportTests(unittest.TestCase):
    def setUp(self):
        self.report = passing_report()
        self.mode = self.report["modes"][1]

    def test_passing_and_threshold_boundary(self):
        self.mode["fixtures"][0]["similarity"] = 0.90
        validator.validate_report(self.report)

    def test_below_threshold_even_if_pass_claimed(self):
        self.mode["fixtures"][0]["similarity"] = 0.89
        with self.assertRaises(validator.ValidationError):
            validator.validate_report(self.report)

    def test_rejects_forged_scores_and_modified_labels(self):
        for kind in ("forged cer", "forged similarity", "modified label"):
            with self.subTest(kind=kind):
                report = passing_report()
                fixture = report["modes"][1]["fixtures"][0]
                if kind == "modified label":
                    fixture["expected_text"] = "ALTERED"
                    fixture["actual_text"] = "ALTERED"
                else:
                    fixture["actual_text"] = "WRONG OUTPUT"
                    if kind == "forged similarity":
                        fixture["strict_cer"] = validator.strict_cer(
                            fixture["expected_text"], fixture["actual_text"]
                        )
                with self.assertRaises(validator.ValidationError):
                    validator.validate_report(report)

    def test_strict_cer_unicode_and_float32_rounding(self):
        self.assertEqual(validator.strict_cer("지금 떠나야 해", "지금떠나야해"), 0.25)
        self.assertEqual(validator.strict_cer("今出発しよう", "今出発しょう"),
                         validator.float32(1 / 6))

    def test_threshold_cannot_be_lowered(self):
        self.mode["fixtures"][0]["min_similarity"] = 0.89
        with self.assertRaises(validator.ValidationError):
            validator.validate_report(self.report)

    def test_missing_and_duplicate_fixtures_or_modes(self):
        for kind in ("missing fixture", "duplicate fixture", "missing mode", "duplicate mode"):
            with self.subTest(kind=kind):
                report = passing_report()
                mode = report["modes"][1]
                if kind == "missing fixture":
                    mode["fixtures"].pop()
                elif kind == "duplicate fixture":
                    mode["fixtures"][1] = copy.deepcopy(mode["fixtures"][0])
                elif kind == "missing mode":
                    report["modes"].pop(1)
                else:
                    report["modes"].append(copy.deepcopy(mode))
                with self.assertRaises(validator.ValidationError):
                    validator.validate_report(report)

    def test_rejects_malformed_and_untrue_pass(self):
        for field, value in (("similarity", float("nan")), ("similarity", 1.01),
                             ("similarity", True), ("cer", -1), ("infer_ms", 0.5),
                             ("meets_threshold", False), ("used_tesseract", True), ("actual_text", None)):
            with self.subTest(field=field, value=value):
                report = passing_report()
                report["modes"][1]["fixtures"][0][field] = value
                with self.assertRaises(validator.ValidationError):
                    validator.validate_report(report)

    def test_malformed_missing_and_duplicate_json_keys(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "report.json"
            with self.assertRaises(OSError):
                validator.load_report(path)
            for text in ('{', '{"engine":1,"engine":2}', '{"value":1e999}'):
                path.write_text(text, encoding="utf-8")
                with self.subTest(text=text), self.assertRaises(ValueError):
                    validator.load_report(path)

    def test_cli_missing_report_exits_nonzero(self):
        result = subprocess.run(
            [sys.executable, validator.__file__, "/missing/report.json", "/missing/log", "/missing/models", "/missing/ort"],
            capture_output=True, text=True, check=False,
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("validation failed", result.stderr)


class ModelPreparationTests(unittest.TestCase):
    def test_downloads_are_verified_and_existing_data_is_untouched(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            destination = root / "new-models"
            models = {"model.onnx": hashlib.sha256(b"good").hexdigest()}
            urls = {"model.onnx": "https://example.invalid/model"}

            def download(args, check):
                self.assertTrue(check)
                Path(args[args.index("--output") + 1]).write_bytes(b"good")

            with patch.object(prepare, "MODELS", models), patch.object(prepare, "URLS", urls), \
                    patch.object(prepare.subprocess, "run", side_effect=download):
                prepare.prepare_models(destination)
                self.assertEqual((destination / "model.onnx").read_bytes(), b"good")
                with self.assertRaises(FileExistsError):
                    prepare.prepare_models(destination)
                self.assertEqual((destination / "model.onnx").read_bytes(), b"good")

                bad = root / "bad-models"
                with patch.object(prepare.subprocess, "run", side_effect=lambda args, check: Path(
                    args[args.index("--output") + 1]).write_bytes(b"corrupt")):
                    with self.assertRaisesRegex(ValueError, "hash mismatch"):
                        prepare.prepare_models(bad)
                self.assertEqual((bad / "model.onnx").read_bytes(), b"corrupt")

    def test_incomplete_model_manifest_fails_before_creating_directory(self):
        with tempfile.TemporaryDirectory() as directory:
            destination = Path(directory) / "models"
            with patch.object(prepare, "URLS", {}), self.assertRaises(ValueError):
                prepare.prepare_models(destination)
            self.assertFalse(destination.exists())


class PrerequisiteTests(unittest.TestCase):
    def test_models_runtime_and_profile_initialization_required(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            ort = root / "ort.so"
            ort.write_bytes(b"runtime")
            digest = hashlib.sha256(b"model").hexdigest()
            models = {name: digest for name in validator.MODELS}
            for name in models:
                (root / name).write_bytes(b"model")
            log = "Initializing PP-OCRv3 models (\n" * 3
            log += "OCR execution provider order: cpu (GPU providers disabled)\n"
            for profile, name in validator.PROFILES.items():
                log += f"Initializing PP-OCRv3 {profile} rec model at '{root / name}'\n" * 3
            with patch.object(validator, "MODELS", models):
                validator.validate_prerequisites(root, ort, log)
                for bad_log in ("", log + "japanese rec model failed to initialize", log.replace("latin rec model", "other rec model")):
                    with self.subTest(log=bad_log[:50]), self.assertRaises(validator.ValidationError):
                        validator.validate_prerequisites(root, ort, bad_log)
                with self.assertRaises(validator.ValidationError):
                    validator.validate_prerequisites(root, root / "missing.so", log)
                (root / next(iter(models))).write_bytes(b"corrupt")
                with self.assertRaises(validator.ValidationError):
                    validator.validate_prerequisites(root, ort, log)
                (root / next(iter(models))).unlink()
                with self.assertRaises(validator.ValidationError):
                    validator.validate_prerequisites(root, ort, log)
                (root / next(iter(models))).write_bytes(b"model")
                cjk = root / validator.PROFILES["cjk"]
                cjk.write_bytes(b"corrupt")
                with self.assertRaisesRegex(validator.ValidationError, "model hash mismatch"):
                    validator.validate_prerequisites(root, ort, log)


if __name__ == "__main__":
    unittest.main()
