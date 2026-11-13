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
    fixtures = []
    languages = []
    for name, threshold in validator.THRESHOLDS.items():
        fixtures.append({
            "name": name, "language": name,
            "expected_text": validator.EXPECTED_TEXT[name],
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
        "avg_strict_cer": 0.0, "avg_similarity": 1.0, "pass_rate": 1.0, "avg_infer_ms": 1.0,
        "tesseract_replacement_count": 0, "per_language": languages, "fixtures": fixtures,
    }
    return {"engine": "PP-OCRv3", "modes": [{"mode": "hybrid"}, mode, {"mode": "strict-gain"}]}


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
                    fixture["expected_text"] = fixture["actual_text"] = "ALTERED"
                else:
                    fixture["actual_text"] = "WRONG OUTPUT"
                    if kind == "forged similarity":
                        fixture["strict_cer"] = validator.strict_cer(
                            fixture["expected_text"], fixture["actual_text"])
                self.assert_rejected(report)

    def test_strict_cer_unicode_and_float32_rounding(self):
        self.assertEqual(validator.strict_cer("지금 떠나야 해", "지금떠나야해"), 0.25)
        self.assertEqual(validator.strict_cer("今出発しよう", "今出発しょう"),
                         validator.float32(1 / 6))

    def test_threshold_cannot_be_lowered(self):
        self.mode["fixtures"][0]["min_similarity"] = 0.89
        self.assert_rejected()

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
                self.assert_rejected(report)

    def test_rejects_malformed_fields_and_untrue_pass(self):
        cases = [("fixtures", field, value) for field, value in (
            ("similarity", float("nan")), ("similarity", 1.01), ("similarity", True),
            ("cer", -1), ("infer_ms", 0.5), ("meets_threshold", False),
            ("used_tesseract", True), ("actual_text", None),
        )] + [("per_language", field, value) for field, value in (
            ("fixture_count", True), ("pass_rate", 0.5), ("language", "deu"),
        )]
        for section, field, value in cases:
            with self.subTest(section=section, field=field, value=value):
                report = passing_report()
                report["modes"][1][section][0][field] = value
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
        result = subprocess.run(
            [sys.executable, validator.__file__]
            + [f"/missing/{name}" for name in ("report.json", "log", "models", "ort")],
            capture_output=True, text=True, check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("validation failed", result.stderr)


class PrerequisiteTests(unittest.TestCase):
    def test_models_runtime_and_profile_initialization_required(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            ort = root / "ort.so"
            ort.write_bytes(b"runtime")
            models = {name: hashlib.sha256(b"model").hexdigest() for name in validator.MODELS}
            for name in models:
                (root / name).write_bytes(b"model")
            engines = validator.PROBE_ENGINE_COUNT
            log = "Initializing PP-OCRv3 models (\n" * engines
            log += "OCR execution provider order: cpu (GPU providers disabled)\n"
            for profile, name in validator.PROFILES.items():
                log += f"Initializing PP-OCRv3 {profile} rec model at '{root / name}'\n" * engines
            first_model = root / next(iter(models))

            def assert_rejected(ort_path=ort, text=log):
                with self.assertRaises(validator.ValidationError):
                    validator.validate_prerequisites(root, ort_path, text)

            with patch.object(validator, "MODELS", models):
                validator.validate_prerequisites(root, ort, log)
                for bad_log in ("", log + "japanese rec model failed to initialize",
                                log.replace("latin rec model", "other rec model")):
                    with self.subTest(log=bad_log[:50]):
                        assert_rejected(text=bad_log)
                assert_rejected(ort_path=root / "missing.so")
                first_model.write_bytes(b"corrupt")
                assert_rejected()
                first_model.unlink()
                assert_rejected()


class ModelPreparationTests(unittest.TestCase):
    def setUp(self):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.destination = Path(directory.name) / "models"
        for name, value in (("MODELS", {"model.onnx": hashlib.sha256(b"good").hexdigest()}),
                            ("URLS", {"model.onnx": "https://example.invalid/model"})):
            patcher = patch.object(prepare, name, value)
            patcher.start()
            self.addCleanup(patcher.stop)

    def prepare_with(self, content):
        def fake_curl(args, check):
            Path(args[args.index("--output") + 1]).write_bytes(content)
        with patch.object(prepare.subprocess, "run", side_effect=fake_curl), \
                contextlib.redirect_stdout(io.StringIO()):
            prepare.prepare_models(self.destination)

    def test_verified_download_and_existing_destination_is_untouched(self):
        self.prepare_with(b"good")
        with self.assertRaises(FileExistsError):
            self.prepare_with(b"other")
        self.assertEqual((self.destination / "model.onnx").read_bytes(), b"good")

    def test_hash_mismatch_and_failed_download_fail(self):
        with self.assertRaisesRegex(ValueError, "hash mismatch"):
            self.prepare_with(b"corrupt")
        error = subprocess.CalledProcessError(22, "curl")
        with patch.object(prepare.subprocess, "run", side_effect=error), \
                self.assertRaises(subprocess.CalledProcessError):
            prepare.prepare_models(self.destination.with_name("other"))

    def test_ir10_upstream_is_verified_then_lowered(self):
        upstream, lowered = b"\x08\x0arest", b"\x08\x09rest"
        pins = {"MODELS": {"model.onnx": hashlib.sha256(lowered).hexdigest()},
                "IR10_UPSTREAM": {"model.onnx": hashlib.sha256(upstream).hexdigest()}}
        with patch.multiple(prepare, **pins):
            with self.assertRaisesRegex(ValueError, "upstream model hash mismatch"):
                self.prepare_with(lowered)
            self.destination = self.destination.with_name("lowered")
            self.prepare_with(upstream)
        self.assertEqual((self.destination / "model.onnx").read_bytes(), lowered)
        bad = self.destination / "bad.onnx"
        bad.write_bytes(b"\x08\x8a\x00")
        with self.assertRaisesRegex(ValueError, "ir_version encoding"):
            prepare.lower_ir_version(bad)

    def test_manifest_mismatch_fails_before_creating_directory(self):
        with patch.object(prepare, "URLS", {}), self.assertRaises(ValueError):
            prepare.prepare_models(self.destination)
        self.assertFalse(self.destination.exists())


class ManifestTests(unittest.TestCase):
    def test_every_pinned_model_has_a_download_url(self):
        self.assertEqual(set(validator.MODELS), set(prepare.URLS))


if __name__ == "__main__":
    unittest.main()
