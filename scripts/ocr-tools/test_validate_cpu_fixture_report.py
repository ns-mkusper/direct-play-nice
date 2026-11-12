"""Tests for the CPU OCR fixture validator and model preparation scripts."""

import contextlib
import copy
import hashlib
import io
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

import prepare_cpu_ocr_models as prepare
import validate_cpu_fixture_report as validator


def passing_report():
    """Return a minimal report that satisfies every validator check."""
    fixtures = []
    languages = []
    for name, threshold in validator.THRESHOLDS.items():
        fixtures.append({
            "name": name,
            "language": name,
            "expected_text": validator.EXPECTED_TEXT[name],
            "actual_text": validator.EXPECTED_TEXT[name],
            "infer_ms": 1,
            "cer": 0.0,
            "wer": 0.0,
            "strict_cer": 0.0,
            "similarity": 1.0,
            "min_similarity": threshold,
            "meets_threshold": True,
            "used_tesseract": False,
            "ppocr_quality": 1.0,
            "ppocr_confidence": 0.9,
            "tesseract_quality": None,
        })
        languages.append({
            "language": name,
            "fixture_count": 1,
            "avg_cer": 0.0,
            "avg_wer": 0.0,
            "avg_strict_cer": 0.0,
            "avg_similarity": 1.0,
            "pass_rate": 1.0,
        })
    mode = {
        "mode": "pure-onnx",
        "fixture_count": 5,
        "avg_cer": 0.0,
        "avg_wer": 0.0,
        "avg_strict_cer": 0.0,
        "avg_similarity": 1.0,
        "pass_rate": 1.0,
        "avg_infer_ms": 1.0,
        "tesseract_replacement_count": 0,
        "per_language": languages,
        "fixtures": fixtures,
    }
    modes = [{"mode": "hybrid"}, mode, {"mode": "strict-gain"}]
    return {"engine": "PP-OCRv3", "modes": modes}


class ReportTests(unittest.TestCase):
    def setUp(self):
        self.report = passing_report()
        self.mode = self.report["modes"][1]

    def assert_rejected(self, report=None):
        with self.assertRaises(validator.ValidationError):
            validator.validate_report(report or self.report)

    def test_passing_and_threshold_boundary(self):
        self.mode["fixtures"][0]["similarity"] = 0.90
        validator.validate_report(self.report)

    def test_below_threshold_even_if_pass_claimed(self):
        self.mode["fixtures"][0]["similarity"] = 0.89
        self.assert_rejected()

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
                self.assert_rejected(report)

    def test_strict_cer_unicode_and_float32_rounding(self):
        self.assertEqual(
            validator.strict_cer("지금 떠나야 해", "지금떠나야해"), 0.25
        )
        self.assertEqual(
            validator.strict_cer("今出発しよう", "今出発しょう"),
            validator.float32(1 / 6),
        )

    def test_threshold_cannot_be_lowered(self):
        self.mode["fixtures"][0]["min_similarity"] = 0.89
        self.assert_rejected()

    def test_missing_and_duplicate_fixtures_or_modes(self):
        kinds = (
            "missing fixture",
            "duplicate fixture",
            "missing mode",
            "duplicate mode",
        )
        for kind in kinds:
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
                self.assert_rejected(report)

    def test_rejects_malformed_and_untrue_pass(self):
        cases = (
            ("similarity", float("nan")),
            ("similarity", 1.01),
            ("similarity", True),
            ("cer", -1),
            ("infer_ms", 0.5),
            ("meets_threshold", False),
            ("used_tesseract", True),
            ("actual_text", None),
        )
        for field, value in cases:
            with self.subTest(field=field, value=value):
                report = passing_report()
                report["modes"][1]["fixtures"][0][field] = value
                self.assert_rejected(report)

    def test_rejects_bad_language_summaries(self):
        cases = (
            ("fixture_count", True),
            ("pass_rate", 0.5),
            ("language", "deu"),
        )
        for field, value in cases:
            with self.subTest(field=field, value=value):
                report = passing_report()
                report["modes"][1]["per_language"][0][field] = value
                self.assert_rejected(report)

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
        missing = ("report.json", "log", "models", "ort")
        result = subprocess.run(
            [sys.executable, validator.__file__]
            + [f"/missing/{name}" for name in missing],
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("validation failed", result.stderr)


def fake_curl(content):
    """Return a ``subprocess.run`` stand-in that writes ``content``."""
    def run(args, check):
        assert check
        Path(args[args.index("--output") + 1]).write_bytes(content)
    return run


class ModelPreparationTests(unittest.TestCase):
    def setUp(self):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.root = Path(directory.name)
        models = {"model.onnx": hashlib.sha256(b"good").hexdigest()}
        urls = {"model.onnx": "https://example.invalid/model"}
        for name, value in (("MODELS", models), ("URLS", urls)):
            patcher = patch.object(prepare, name, value)
            patcher.start()
            self.addCleanup(patcher.stop)

    def prepare_with(self, content, destination):
        with patch.object(
            prepare.subprocess, "run", side_effect=fake_curl(content)
        ), contextlib.redirect_stdout(io.StringIO()):
            prepare.prepare_models(destination)

    def test_downloads_are_verified(self):
        destination = self.root / "models"
        self.prepare_with(b"good", destination)
        self.assertEqual((destination / "model.onnx").read_bytes(), b"good")

    def test_existing_destination_is_untouched(self):
        destination = self.root / "models"
        self.prepare_with(b"good", destination)
        with self.assertRaises(FileExistsError):
            self.prepare_with(b"other", destination)
        self.assertEqual((destination / "model.onnx").read_bytes(), b"good")

    def test_hash_mismatch_fails(self):
        with self.assertRaisesRegex(ValueError, "hash mismatch"):
            self.prepare_with(b"corrupt", self.root / "models")

    def test_failed_download_propagates(self):
        error = subprocess.CalledProcessError(22, "curl")
        with patch.object(prepare.subprocess, "run", side_effect=error), \
                self.assertRaises(subprocess.CalledProcessError):
            prepare.prepare_models(self.root / "models")

    def test_incomplete_manifest_fails_before_creating_directory(self):
        destination = self.root / "models"
        with patch.object(prepare, "URLS", {}), self.assertRaises(ValueError):
            prepare.prepare_models(destination)
        self.assertFalse(destination.exists())

    def test_cli_reports_failure(self):
        with patch.object(sys, "argv", ["prepare", str(self.root)]), \
                patch("sys.stderr"):
            self.assertEqual(prepare.main(), 1)


class ManifestTests(unittest.TestCase):
    def test_every_pinned_model_has_a_download_url(self):
        self.assertEqual(set(validator.MODELS), set(prepare.URLS))


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
            engines = validator.PROBE_ENGINE_COUNT
            log = "Initializing PP-OCRv3 models (\n" * engines
            log += (
                "OCR execution provider order: cpu (GPU providers disabled)\n"
            )
            for profile, name in validator.PROFILES.items():
                marker = f"Initializing PP-OCRv3 {profile} rec model at"
                log += f"{marker} '{root / name}'\n" * engines
            first_model = root / next(iter(models))

            def assert_rejected(model_dir=root, ort_path=ort, text=log):
                with self.assertRaises(validator.ValidationError):
                    validator.validate_prerequisites(model_dir, ort_path, text)

            with patch.object(validator, "MODELS", models):
                validator.validate_prerequisites(root, ort, log)
                bad_logs = (
                    "",
                    log + "japanese rec model failed to initialize",
                    log.replace("latin rec model", "other rec model"),
                )
                for bad_log in bad_logs:
                    with self.subTest(log=bad_log[:50]):
                        assert_rejected(text=bad_log)
                assert_rejected(ort_path=root / "missing.so")
                first_model.write_bytes(b"corrupt")
                assert_rejected()
                first_model.unlink()
                assert_rejected()
                first_model.write_bytes(b"model")
                (root / validator.PROFILES["cjk"]).write_bytes(b"corrupt")
                with self.assertRaisesRegex(
                    validator.ValidationError, "model hash mismatch"
                ):
                    validator.validate_prerequisites(root, ort, log)


if __name__ == "__main__":
    unittest.main()
