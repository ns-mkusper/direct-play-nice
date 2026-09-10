"""Local utility/contract tests; no running Arr or DPN required.

  python3 -m unittest discover -s tests/e2e/servarr -p 'test_*.py' -v

HTTP fixtures below test the client utility, not an imitation Arr integration.
The actual E2E driver only targets the real app in its disposable pod.
"""

import contextlib
import hashlib
import http.server
import importlib.util
import json
import os
from pathlib import Path
import sqlite3
import tempfile
import threading
import unittest
from unittest import mock
import urllib.error


SPEC = importlib.util.spec_from_file_location("servarr_harness", Path(__file__).with_name("harness.py"))
harness = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(harness)


class PollTests(unittest.TestCase):
    def fake_time(self):
        current = [0.0]
        waits = []
        def wait(seconds):
            waits.append(seconds)
            current[0] += seconds
        return lambda: current[0], wait, waits

    def test_returns_immediately_on_condition(self):
        clock, wait, waits = self.fake_time()
        self.assertEqual(harness.poll(lambda: {"done": True}, "done", clock=clock, wait=wait), {"done": True})
        self.assertEqual(waits, [])

    def test_retries_transport_error_then_returns_result(self):
        clock, wait, waits = self.fake_time()
        condition = mock.Mock(side_effect=[urllib.error.URLError("starting"), None, {"status": "completed"}])
        result = harness.poll(condition, "ready", interval=0.5, clock=clock, wait=wait)
        self.assertEqual(result["status"], "completed")
        self.assertEqual(waits, [0.5, 0.5])

    def test_timeout_is_bounded_and_has_context(self):
        clock, wait, waits = self.fake_time()
        with self.assertRaisesRegex(harness.HarnessError, "import command"):
            harness.poll(lambda: False, "import command", timeout=1.1, interval=0.5, clock=clock, wait=wait)
        self.assertAlmostEqual(sum(waits), 1.1)

    def test_assertion_is_not_swallowed(self):
        with self.assertRaisesRegex(harness.HarnessError, "bad import"):
            harness.poll(mock.Mock(side_effect=harness.HarnessError("bad import")), "done")


class FilesTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)

    def test_hardlinks_and_atomic_replacement(self):
        source = self.root / "source.mkv"
        source.write_bytes(b"original")
        source.chmod(0o644)
        destination = self.root / "library.mkv"
        os.link(source, destination)
        before = harness.file_snapshot(source)
        self.assertEqual(before["sha256"], hashlib.sha256(b"original").hexdigest())
        self.assertEqual(before["nlink"], 2)
        self.assertEqual(before["mode"], 0o644)
        self.assertTrue(harness.same_inode(before, harness.file_snapshot(destination)))
        temporary = self.root / "replacement.mkv"
        temporary.write_bytes(b"converted")
        temporary.replace(destination)
        self.assertFalse(harness.same_inode(before, harness.file_snapshot(destination)))
        self.assertEqual(source.read_bytes(), b"original")

    def test_missing_file(self):
        self.assertIsNone(harness.file_snapshot(self.root / "missing"))

    def test_event_reader_ignores_incomplete_final_line_only(self):
        path = self.root / "events.jsonl"
        self.assertEqual(harness.read_events(path), [])
        path.write_bytes(b'{"event":"Download"}\n{"event":')
        self.assertEqual(harness.read_events(path), [{"event": "Download"}])
        path.write_bytes(b'{bad json}\n')
        with self.assertRaises(json.JSONDecodeError):
            harness.read_events(path)

    def test_downloads_count_completed_events_not_start_records(self):
        path = self.root / "events.jsonl"
        records = [{"phase": "start", "event": "Download"},
                   {"phase": "end", "event": "Download", "returncode": 0},
                   {"phase": "end", "event": "Test", "returncode": 0}]
        path.write_text("".join(json.dumps(record) + "\n" for record in records))
        driver = object.__new__(harness.Harness)
        driver.events_file = path
        driver.args = harness.parse_args(["--app", "sonarr"])
        self.assertEqual(driver.downloads(), [harness.normalize_event(records[1], "sonarr")])

    def test_raw_wrapper_event_normalization(self):
        raw = {"phase": "end", "Sonarr_EventType": "Download", "Sonarr_IsUpgrade": "True",
               "Sonarr_EpisodeFile_Path": "/data/library/test.mkv", "returncode": 0}
        event = harness.normalize_event(raw, "sonarr")
        self.assertEqual(event["event"], "Download")
        self.assertIs(event["is_upgrade"], True)
        self.assertEqual(event["path"], "/data/library/test.mkv")

    def test_production_suffix_and_quality_policy(self):
        self.assertEqual(harness.converted_path("/data/Show.S01E01.HDTV-1080p.mkv"),
                         Path("/data/Show.S01E01.HDTV-480p.fixed.mp4"))
        self.assertEqual(harness.converted_path("/data/Show.360p.mkv"),
                         Path("/data/Show.360p.fixed.mp4"))

    def test_atomic_results_replace_valid_json(self):
        path = self.root / "result.json"
        harness.atomic_json(path, {"status": "running"})
        harness.atomic_json(path, {"status": "passed"})
        self.assertEqual(json.loads(path.read_text()), {"status": "passed"})
        self.assertFalse(path.with_suffix(".json.tmp").exists())


class OfflineSeedTests(unittest.TestCase):
    def setUp(self):
        self.db = sqlite3.connect(":memory:")
        self.addCleanup(self.db.close)

    def test_seed_filters_optional_version_specific_columns(self):
        self.db.execute('CREATE TABLE Series (Id INTEGER PRIMARY KEY, Title TEXT NOT NULL, Monitored INTEGER DEFAULT 1)')
        entity_id = harness.insert_metadata(self.db, "Series", {"Title": "fixture", "NewOptionalField": "value"})
        self.assertEqual(self.db.execute("SELECT * FROM Series").fetchall(), [(entity_id, "fixture", 1)])

    def test_unknown_required_column_fails_closed(self):
        self.db.execute('CREATE TABLE Series (Id INTEGER PRIMARY KEY, Title TEXT NOT NULL, NewRequired TEXT NOT NULL)')
        with self.assertRaisesRegex(harness.HarnessError, "mandatory column NewRequired"):
            harness.insert_metadata(self.db, "Series", {"Title": "fixture"})
        self.assertEqual(self.db.execute("SELECT count(*) FROM Series").fetchone()[0], 0)

    def test_import_history_cannot_be_seeded(self):
        for table in ["History", "EpisodeFiles", "MovieFiles", "Notifications", "Commands", 'Series"; DROP TABLE Series']:
            with self.subTest(table=table), self.assertRaisesRegex(harness.HarnessError, "Not a metadata table"):
                harness.insert_metadata(self.db, table, {})

    def test_missing_migration_fails_closed(self):
        with self.assertRaisesRegex(harness.HarnessError, "Missing table Episodes"):
            harness.insert_metadata(self.db, "Episodes", {})

    def test_running_app_is_rejected(self):
        with mock.patch.object(harness.socket, "socket") as socket_factory:
            socket_factory.return_value.__enter__.return_value.connect_ex.return_value = 0
            with self.assertRaisesRegex(harness.HarnessError, "Stop the app"):
                harness.assert_app_stopped("sonarr")

    def test_real_seed_transaction_sonarr_and_radarr(self):
        for app in ("sonarr", "radarr"):
            with self.subTest(app=app), tempfile.TemporaryDirectory() as temporary:
                root = Path(temporary)
                database = root / f"{app}.db"
                with sqlite3.connect(database) as db:
                    db.execute("CREATE TABLE QualityProfiles (Id INTEGER PRIMARY KEY)")
                    db.execute("INSERT INTO QualityProfiles VALUES (2)")
                    if app == "sonarr":
                        db.execute('CREATE TABLE Series (Id INTEGER PRIMARY KEY, Title TEXT NOT NULL, Path TEXT NOT NULL, QualityProfileId INTEGER NOT NULL)')
                        db.execute('CREATE TABLE Episodes (Id INTEGER PRIMARY KEY, SeriesId INTEGER NOT NULL, EpisodeFileId INTEGER NOT NULL, EpisodeNumber INTEGER NOT NULL)')
                    else:
                        db.execute('CREATE TABLE MovieMetadata (Id INTEGER PRIMARY KEY, Title TEXT NOT NULL, TmdbId INTEGER NOT NULL, OriginalLanguage INTEGER NOT NULL)')
                        db.execute('CREATE TABLE Movies (Id INTEGER PRIMARY KEY, MovieMetadataId INTEGER NOT NULL, MovieFileId INTEGER NOT NULL, Path TEXT NOT NULL, QualityProfileId INTEGER NOT NULL)')
                with mock.patch.object(harness, "assert_app_stopped"):
                    seed = harness.seed_offline(app, root / "config.xml", root / "data", os.getuid(), os.getgid())
                    self.assertTrue(seed["metadata_only"])
                    self.assertEqual(seed["entity_id"], 1)
                    self.assertEqual(json.loads((root / "data/seed.json").read_text()), seed)
                    with self.assertRaisesRegex(harness.HarnessError, "nonempty library"):
                        harness.seed_offline(app, root / "config.xml", root / "data", os.getuid(), os.getgid())


@contextlib.contextmanager
def http_fixture(response, status=200):
    requests = []
    class Handler(http.server.BaseHTTPRequestHandler):
        def do_GET(self):
            requests.append({"path": self.path, "key": self.headers.get("X-Api-Key")})
            self.send_response(status)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(response).encode())
        def log_message(self, *args):
            pass
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        yield f"http://127.0.0.1:{server.server_port}/", requests
    finally:
        server.shutdown()
        server.server_close()
        thread.join(timeout=3)


class ApiTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.config = self.root / "config.xml"
        self.config.write_text("<Config><ApiKey>unit-secret</ApiKey></Config>")

    def test_client_auth_query_and_secret_free_transcript(self):
        with http_fixture({"id": 3}) as (base, requests):
            api = harness.Api("sonarr", self.config, self.root)
            api.base = base
            self.assertEqual(api.get("episode", seriesId=7), {"id": 3})
            self.assertEqual(requests, [{"path": "/episode?seriesId=7", "key": "unit-secret"}])
        transcript = api.transcript.read_text()
        self.assertNotIn("unit-secret", transcript)
        self.assertEqual(json.loads(transcript)["status"], 200)

    def test_http_error_contains_response(self):
        with http_fixture({"error": "invalid import"}, 400) as (base, _):
            api = harness.Api("radarr", self.config, self.root)
            api.base = base
            with self.assertRaisesRegex(harness.HarnessError, "HTTP 400.*invalid import"):
                api.get("command")
        self.assertEqual(json.loads(api.transcript.read_text())["status"], 400)

    def test_requires_api_key(self):
        self.config.write_text("<Config/>")
        with self.assertRaisesRegex(harness.HarnessError, "Missing ApiKey"):
            harness.Api("sonarr", self.config, self.root)

    def test_command_waits_for_completion(self):
        api = harness.Api("sonarr", self.config, self.root)
        api.request = mock.Mock(return_value={"id": 42})
        api.get = mock.Mock(side_effect=[{"status": "started"}, {"status": "completed", "id": 42}])
        result = api.command("ManualImport", files=[{"path": "/data/downloads/file.mkv"}], importMode="copy")
        self.assertEqual(result["id"], 42)
        self.assertEqual(api.get.call_count, 2)
        api.request.assert_called_once_with("POST", "command", {
            "name": "ManualImport", "files": [{"path": "/data/downloads/file.mkv"}], "importMode": "copy"})

    def test_failed_command_fails_immediately(self):
        api = harness.Api("radarr", self.config, self.root)
        api.request = mock.Mock(return_value={"id": 42})
        api.get = mock.Mock(return_value={"status": "failed", "message": "bad import"})
        with self.assertRaisesRegex(harness.HarnessError, "ManualImport command failed"):
            api.command("ManualImport", files=[])
        self.assertEqual(api.get.call_count, 1)


class MediaTests(unittest.TestCase):
    def probe(self, codec="h264", color="blue"):
        return json.dumps({"streams": [
            {"codec_type": "video", "codec_name": codec, "width": 640, "height": 360},
            {"codec_type": "audio", "channels": 2, "tags": {"language": "eng"}}],
            "format": {"duration": "4.02"}}).encode()

    def test_decode_verification_checks_all_streams_and_content(self):
        with mock.patch.object(harness, "run_process", side_effect=[self.probe(), b"", bytes([0, 0, 252])]) as run:
            result = harness.verify_media("/data/library/file.mkv", "h264", "blue")
        self.assertEqual(result["decoded_rgb"], [0, 0, 252])
        self.assertIn("-xerror", run.call_args_list[1].args[0])
        self.assertIn("0:a", run.call_args_list[1].args[0])

    def test_decoded_wrong_content_fails_even_with_correct_codec(self):
        with mock.patch.object(harness, "run_process", side_effect=[self.probe(), b"", bytes([254, 0, 0])]):
            with self.assertRaisesRegex(harness.HarnessError, "not blue"):
                harness.verify_media("file.mkv", "h264", "blue")

    def test_wrong_codec_fails(self):
        with mock.patch.object(harness, "run_process", return_value=self.probe("mpeg4")):
            with self.assertRaisesRegex(harness.HarnessError, "Unexpected video streams"):
                harness.verify_media("file.mkv", "h264", "blue")

    def test_generator_uses_local_synthetic_media_not_external_input(self):
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "fixture.mkv"
            def generate(argv):
                self.assertIn("lavfi", argv)
                self.assertIn("mpeg4", argv)
                self.assertIn("language=eng", argv)
                path.write_bytes(b"fixture")
            with mock.patch.object(harness, "run_process", side_effect=generate):
                result = harness.generate_fixture(path, "red", os.getuid(), os.getgid())
            self.assertEqual(result["mode"], 0o644)


class SafetyTests(unittest.TestCase):
    def test_hotio_group_writable_output_is_allowed_but_world_write_is_not(self):
        driver = object.__new__(harness.Harness)
        driver.args = harness.parse_args(["--app", "sonarr"])
        driver.verify_permissions({"uid": 1000, "gid": 1000, "mode": 0o664})
        with self.assertRaisesRegex(harness.HarnessError, "Unsafe"):
            driver.verify_permissions({"uid": 1000, "gid": 1000, "mode": 0o666})
        with self.assertRaisesRegex(harness.HarnessError, "ownership"):
            driver.verify_permissions({"uid": 0, "gid": 1000, "mode": 0o644})

    def test_ephemeral_opt_in_required(self):
        with mock.patch.dict(os.environ, {}, clear=True):
            with self.assertRaisesRegex(harness.HarnessError, "DPN_E2E_EPHEMERAL"):
                harness.main(["--app", "sonarr"])

    def test_root_hook_user_rejected(self):
        with mock.patch.dict(os.environ, {"DPN_E2E_EPHEMERAL": "1"}):
            with self.assertRaisesRegex(harness.HarnessError, "non-root"):
                harness.main(["--app", "radarr", "--uid", "0"])

    def test_relative_paths_rejected(self):
        with mock.patch.dict(os.environ, {"DPN_E2E_EPHEMERAL": "1"}):
            with self.assertRaisesRegex(harness.HarnessError, "absolute pod paths"):
                harness.main(["--app", "sonarr", "--data", "relative"])


if __name__ == "__main__":
    unittest.main()
