#!/usr/bin/env python3
"""Real Servarr import E2E driver; Python standard library only.

Run *inside* the disposable app pod (normally via kubectl exec):
  DPN_E2E_EPHEMERAL=1 python3 /opt/e2e/harness.py --app sonarr

Bootstrap the app once, stop it, then run the same command with --seed-offline
before restarting it. Official AddSeriesService/AddMovieService unconditionally
fetch Skyhook; their cloud request builders hard-code the HTTPS hosts. Therefore
only initial metadata is seeded offline, in an empty, migrated SQLite database.
No file, history, command, or notification rows are seeded. All imports, upgrades,
notifications, and rescans subsequently go through the real app's HTTP API.
Source references (review against the image versions when updating pins):
  Sonarr/Sonarr: src/NzbDrone.Core/Tv/AddSeriesService.cs
  Radarr/Radarr: src/NzbDrone.Core/Movies/AddMovieService.cs
  {Sonarr,Radarr}: src/NzbDrone.Common/Cloud/*CloudRequestBuilder.cs
  {Sonarr,Radarr}: src/NzbDrone.Core/MediaFiles/*Import/Manual/ManualImportService.cs

The /opt/e2e/hook.py wrapper inherits real CustomScript environment and
runs the PR-built DPN. This is a derived test image, NOT an official Arr image;
its underlying pinned hotio application binary and s6 entrypoint are retained.
Wrapper JSONL uses phase=start/end, invocation, uid, gid, time, raw sonarr/radarr
variables, returncode, log, mode, before, after. Normalized event/is_upgrade/path
are optional: this harness derives them from raw environment fields. before and
after are file_snapshot() of the imported path immediately before/after child
execution (after is null when successful .fixed.mp4 conversion removes input).
Logs are /data/hook-<uuid>.log. /data/hook-mode selects normal, fail (real DPN with
invalid config; this tests pre-conversion parse failure, not mid-conversion
rollback), or false (/bin/false), only for real Download events.
Production-like config is CPU, 480p, 1M, output .fixed.mp4, validation enabled.
We record stale Arr paths immediately after import, issue an explicit real API
rescan, and REQUIRE Arr to associate the converted output before upgrading.
This repair is measured, not silently represented as hook-side reconciliation.

Require DPN_E2E_EPHEMERAL=1 and loopback HTTP; never target a live installation.
Results and API transcripts persist even on failure under /data. The six import
scenarios invoke DPN only through the real Arr wrapper; the separate original-
audio regression runs the DPN audit directly through a read-only loopback proxy.
"""

from __future__ import annotations

import argparse
import contextlib
import hashlib
import http.server
import json
import os
import re
from pathlib import Path
import socket
import sqlite3
import stat
import subprocess
import sys
import tempfile
import threading
import time
import traceback
import urllib.error
import urllib.parse
import urllib.request
import xml.etree.ElementTree as ET
from datetime import datetime, timezone


TITLE = "DPN E2E Fixture"
EXTERNAL_ID = 990000001
# (fixture name, Arr language ID/name, audio tracks); each title is independent.
POLICY_CASES = [
    ("english-eng", 1, "English", ("eng",)),
    ("english-jpn", 1, "English", ("jpn",)),
    ("japanese-eng", 8, "Japanese", ("eng",)),
    ("japanese-jpn", 8, "Japanese", ("jpn",)),
    ("japanese-both", 8, "Japanese", ("eng", "jpn")),
    ("korean-eng", 21, "Korean", ("eng",)),
    ("korean-kor", 21, "Korean", ("kor",)),
    ("korean-both", 21, "Korean", ("eng", "kor")),
    ("polish-eng", 12, "Polish", ("eng",)),
    ("polish-pol", 12, "Polish", ("pol",)),
    ("polish-both", 12, "Polish", ("eng", "pol")),
    ("unknown-eng", 0, "Unknown", ("eng",)),
    # This real Japanese title's API response is deliberately altered by the
    # read-only audit proxy to test conflicting ID/name handling.
    ("conflict-eng", 8, "Japanese", ("eng",)),
]
# name, original ID/name, current audio, current subtitles, required audio,
# required subtitles, synthetic candidate audio, expected audit outcome.
DEFAULT_POLICY_CASES = [
    ("subtitle-compliant", 8, "Japanese", ("jpn",), ("eng",), (), ("eng",), ("jpn",), "compliant"),
    ("absent-compliant", 8, "Japanese", ("eng",), (), ("eng",), (), ("eng",), "compliant"),
    ("japanese-retained", 8, "Japanese", ("jpn",), (), (), ("eng",), ("jpn",), "selected"),
    ("japanese-dropped", 8, "Japanese", ("jpn",), (), (), ("eng",), ("eng",), "rejected"),
    ("absent-not-demanded", 8, "Japanese", ("eng",), (), (), ("eng",), ("eng",), "selected"),
    ("korean-retained", 21, "Korean", ("kor",), (), (), ("eng",), ("kor",), "selected"),
    ("polish-retained", 12, "Polish", ("pol",), (), (), ("eng",), ("pol",), "selected"),
    ("english-retained", 1, "English", ("eng",), (), (), ("eng",), ("eng",), "selected"),
    ("english-dropped", 1, "English", ("eng",), (), (), ("eng",), ("jpn",), "rejected"),
    ("unknown-replacement", 0, "Unknown", ("jpn",), (), (), ("eng",), ("jpn",), "invalid"),
    ("conflict-replacement", 8, "Japanese", ("jpn",), (), (), ("eng",), ("jpn",), "invalid"),
]
AUDIO_FREQUENCIES = {"eng": 440, "jpn": 550, "kor": 660, "pol": 770}
COLORS = {
    "blue": (0, 0, 255), "red": (255, 0, 0), "green": (0, 128, 0),
    "yellow": (255, 255, 0), "magenta": (255, 0, 255), "cyan": (0, 255, 255),
}


class HarnessError(RuntimeError):
    pass


def require(condition, message):
    if not condition:
        raise HarnessError(message)


def poll(predicate, description, timeout=180, interval=0.25, clock=time.monotonic,
         wait=time.sleep):
    """Poll an observable condition; never hide assertion/programming errors."""
    deadline = clock() + timeout
    last_error = None
    while True:
        try:
            result = predicate()
            if result:
                return result
        except (urllib.error.URLError, TimeoutError, ConnectionError) as exc:
            last_error = str(exc)
        remaining = deadline - clock()
        if remaining <= 0:
            raise HarnessError(f"Timed out waiting for {description}; last transport error: {last_error}")
        wait(min(interval, remaining))


def file_snapshot(path):
    path = Path(path)
    if not path.exists():
        return None
    info = path.stat()
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return {"sha256": digest.hexdigest(), "size": info.st_size,
            "inode": info.st_ino, "device": info.st_dev, "nlink": info.st_nlink,
            "uid": info.st_uid, "gid": info.st_gid, "mode": stat.S_IMODE(info.st_mode)}


def same_inode(left, right):
    return (left["device"], left["inode"]) == (right["device"], right["inode"])


def read_events(path):
    """A writer may be between writes; ignore only an unterminated final line."""
    try:
        content = Path(path).read_bytes()
    except FileNotFoundError:
        return []
    return [json.loads(line) for line in content.splitlines(keepends=True)
            if line.endswith(b"\n") and line.strip()]


def normalize_event(record, app):
    event = dict(record)
    lowered = {key.lower(): value for key, value in record.items()}
    event.setdefault("event", lowered.get(f"{app}_eventtype", ""))
    event.setdefault("is_upgrade", str(lowered.get(f"{app}_isupgrade", "false")).lower() == "true")
    path = lowered.get(f"{app}_moviefile_path") or lowered.get(f"{app}_episodefile_path")
    event.setdefault("path", path or lowered.get(f"{app}_episodefile_paths", "").split("|")[0])
    return event


def converted_path(path):
    """Mirror only the fixed test configuration's filename policy (480p)."""
    path = Path(path)
    stem = path.stem
    matches = list(re.finditer(r"(?<![A-Za-z0-9])(360|480|720|1080|1440|2160)[pP](?![A-Za-z0-9])", stem))
    if matches and int(matches[-1].group(1)) >= 480:
        match = matches[-1]
        stem = stem[:match.start()] + "480p" + stem[match.end():]
    return path.with_name(stem + ".fixed.mp4")


def atomic_json(path, data):
    path = Path(path)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(data, indent=2, sort_keys=True) + "\n")
    temporary.replace(path)


def owned_directory(path, uid, gid):
    path = Path(path)
    path.mkdir(parents=True, exist_ok=True)
    if os.geteuid() == 0:
        os.chown(path, uid, gid)
    path.chmod(0o755)
    return path


def run_process(argv, timeout=120):
    result = subprocess.run([str(arg) for arg in argv], capture_output=True, timeout=timeout)
    if result.returncode:
        raise HarnessError(f"Command failed ({result.returncode}): {argv!r}\n"
                           + result.stderr.decode(errors="replace")[-12000:])
    return result.stdout


def generate_fixture(path, color, uid, gid, languages=("eng",), subtitles=()):
    """All fixture generation occurs in the app pod; no downloaded media."""
    path = Path(path)
    owned_directory(path.parent, uid, gid)
    require(color in COLORS, f"Unknown color: {color}")
    require(languages and len(set(languages)) == len(languages)
            and all(language in AUDIO_FREQUENCIES for language in languages), "Invalid fixture languages")
    argv = ["ffmpeg", "-hide_banner", "-loglevel", "error", "-nostdin", "-y",
            "-f", "lavfi", "-i", f"color=c={color}:s=640x360:r=24:d=4"]
    require(len(set(subtitles)) == len(subtitles) and all(tag == "eng" for tag in subtitles),
            "Invalid fixture subtitle languages")
    with tempfile.TemporaryDirectory(prefix="dpn-e2e-subtitle-") as temp:
        for language in languages:
            argv += ["-f", "lavfi", "-i", f"sine=frequency={AUDIO_FREQUENCIES[language]}:sample_rate=48000:duration=4"]
        for index, language in enumerate(subtitles):
            subtitle = Path(temp) / f"subtitle-{index}.srt"
            subtitle.write_text("1\n00:00:00,000 --> 00:00:04,000\nSynthetic subtitle\n", encoding="utf-8")
            argv += ["-f", "srt", "-i", subtitle]
        argv += ["-map", "0:v:0"]
        for index in range(len(languages)):
            argv += ["-map", f"{index + 1}:a:0"]
        for index in range(len(subtitles)):
            argv += ["-map", f"{len(languages) + index + 1}:s:0"]
        argv += ["-c:v", "mpeg4", "-q:v", "3", "-threads", "1", "-pix_fmt", "yuv420p", "-c:a", "aac", "-ac", "2"]
        if subtitles:
            argv += ["-c:s", "srt"]
        for index, language in enumerate(languages):
            argv += [f"-metadata:s:a:{index}", f"language={language}"]
        for index, language in enumerate(subtitles):
            argv += [f"-metadata:s:s:{index}", f"language={language}"]
        run_process(argv + ["-shortest", path])
    if os.geteuid() == 0:
        os.chown(path, uid, gid)
    path.chmod(0o644)
    return file_snapshot(path)


def verify_media(path, codec, color, languages=("eng",), subtitles=()):
    probe = json.loads(run_process(["ffprobe", "-v", "error", "-show_streams",
                                   "-show_format", "-of", "json", path]))
    videos = [s for s in probe["streams"] if s["codec_type"] == "video"]
    audios = [s for s in probe["streams"] if s["codec_type"] == "audio"]
    require(len(videos) == 1 and videos[0]["codec_name"] == codec,
            f"Unexpected video streams at {path}: {videos}")
    require((videos[0]["width"], videos[0]["height"]) == (640, 360), "Wrong output dimensions")
    actual_languages = [audio.get("tags", {}).get("language") for audio in audios]
    require(actual_languages == list(languages) and all(audio.get("channels") == 2 for audio in audios),
            f"Unexpected audio tracks at {path}: {actual_languages}")
    actual_subtitles = [s.get("tags", {}).get("language") for s in probe["streams"] if s["codec_type"] == "subtitle"]
    require(actual_subtitles == list(subtitles), f"Unexpected subtitle tracks at {path}: {actual_subtitles}")
    require(3.8 <= float(probe["format"]["duration"]) <= 4.5, "Truncated or extended output")
    # Decode every video and audio packet, not just the container header.
    run_process(["ffmpeg", "-v", "error", "-xerror", "-nostdin", "-i", path,
                 "-map", "0:v", "-map", "0:a", "-f", "null", "-"])
    rgb = run_process(["ffmpeg", "-v", "error", "-nostdin", "-ss", "1", "-i", path,
                       "-map", "0:v:0", "-vf", "scale=1:1", "-frames:v", "1",
                       "-pix_fmt", "rgb24", "-f", "rawvideo", "-"])
    require(len(rgb) == 3 and all(abs(a - b) <= 25 for a, b in zip(rgb, COLORS[color])),
            f"Decoded content is not {color}: RGB={list(rgb)}")
    return {"codec": codec, "color": color, "decoded_rgb": list(rgb), "audio_languages": actual_languages,
            "subtitle_languages": actual_subtitles, "duration": float(probe["format"]["duration"]), "width": 640, "height": 360}


class Api:
    def __init__(self, app, config, data, timeout=180):
        self.base = f"http://127.0.0.1:{8989 if app == 'sonarr' else 7878}/api/v3/"
        self.key = ET.parse(config).getroot().findtext("ApiKey")
        require(bool(self.key), f"Missing ApiKey in {config}")
        self.transcript = Path(data) / "api-transcript.jsonl"
        self.timeout = timeout
        # Never route localhost requests through a CI runner's HTTP proxy.
        self.opener = urllib.request.build_opener(urllib.request.ProxyHandler({}))

    def request(self, method, endpoint, body=None, **query):
        url = self.base + endpoint
        if query:
            url += "?" + urllib.parse.urlencode(query)
        request = urllib.request.Request(url, method=method,
                                         headers={"X-Api-Key": self.key, "Content-Type": "application/json"},
                                         data=None if body is None else json.dumps(body).encode())
        record = {"method": method, "endpoint": endpoint, "query": query, "body": body}
        try:
            with self.opener.open(request, timeout=15) as response:
                raw = response.read()
                record["status"] = response.status
                result = json.loads(raw) if raw else None
                record["response"] = result
                return result
        except urllib.error.HTTPError as exc:
            record["status"] = exc.code
            record["response"] = exc.read().decode(errors="replace")
            raise HarnessError(f"{method} {endpoint}: HTTP {exc.code}: {record['response']}") from exc
        finally:
            with self.transcript.open("a") as log:
                log.write(json.dumps(record) + "\n")

    def get(self, endpoint, **query):
        return self.request("GET", endpoint, **query)

    def command(self, name, **arguments):
        command = self.request("POST", "command", {"name": name, **arguments})
        def completed():
            current = self.get(f"command/{command['id']}")
            status = current.get("status", "").lower()
            require(status not in {"failed", "aborted", "cancelled"},
                    f"{name} command failed: {current}")
            return current if status == "completed" else None
        return poll(completed, f"{name} command {command['id']}", self.timeout)


@contextlib.contextmanager
def audit_proxy(app, conflict_id, upstream=None, target=None, synthetic_releases=None):
    """Forward read-only Arr metadata; intercept only the selected synthetic release search."""
    prefix = f"/{app}"
    upstream = upstream or f"http://127.0.0.1:{8989 if app == 'sonarr' else 7878}"
    parsed_upstream = urllib.parse.urlsplit(upstream)
    require(parsed_upstream.scheme == "http" and parsed_upstream.hostname == "127.0.0.1"
            and parsed_upstream.username is None and not parsed_upstream.path
            and not parsed_upstream.query and not parsed_upstream.fragment,
            "Audit proxy must target loopback")
    calls, conflicts = [], []
    allowed = {"queue", "series", "episode", "episodefile", "movie", "moviefile", "history", "blocklist"}

    class NoRedirect(urllib.request.HTTPRedirectHandler):
        def redirect_request(self, req, fp, code, msg, headers, newurl):
            return None

    opener = urllib.request.build_opener(urllib.request.ProxyHandler({}), NoRedirect())

    class Handler(http.server.BaseHTTPRequestHandler):
        def log_message(self, *args):
            pass

        def handle_request(self):
            parsed = urllib.parse.urlsplit(self.path)
            path = parsed.path
            relative = path[len(prefix):] if path.startswith(prefix + "/") else ""
            parts = relative.split("/")
            resource = parts[3] if len(parts) > 3 and parts[:3] == ["", "api", "v3"] else ""
            release_target = None if target is None else (target["episode_id"] if app == "sonarr" else target["entity_id"])
            release_key = "episodeId" if app == "sonarr" else "movieId"
            mock_release = (self.command == "GET" and relative == "/api/v3/release"
                            and synthetic_releases is not None and release_target is not None
                            and urllib.parse.parse_qs(parsed.query) == {release_key: [str(release_target)]})
            allowed_request = ((self.command == "GET" and resource in allowed or mock_release) and not parsed.fragment
                               and all(part not in {".", ".."} for part in urllib.parse.unquote(relative).split("/")))
            calls.append({"method": self.command, "path": relative, "blocked": not allowed_request,
                          "mock_release": mock_release})
            if not allowed_request:
                self.send_error(403)
                return
            if mock_release:
                body = json.dumps(synthetic_releases).encode()
                self.send_response(200)
                self.send_header("Content-Type", "application/json")
                self.send_header("Content-Length", str(len(body)))
                self.end_headers()
                self.wfile.write(body)
                return
            url = upstream + relative + ("?" + parsed.query if parsed.query else "")
            request = urllib.request.Request(url, headers={"X-Api-Key": self.headers.get("X-Api-Key", "")})
            try:
                with opener.open(request, timeout=15) as response:
                    body, status = response.read(), response.status
                collection = "/api/v3/series" if app == "sonarr" else "/api/v3/movie"
                if relative == collection or relative == f"{collection}/{conflict_id}":
                    value = json.loads(body)
                    if relative == collection and target is not None:
                        require(isinstance(value, list), "Expected real Arr collection")
                        value = [item for item in value if item.get("id") == target["entity_id"]]
                        require(len(value) == 1, "Selected title is missing or duplicated in real Arr collection")
                    items = value if isinstance(value, list) else [value]
                    for item in items:
                        if item.get("id") == conflict_id:
                            require(item.get("originalLanguage") == {"id": 8, "name": "Japanese"},
                                    "Conflict source must be real Japanese metadata")
                            item["originalLanguage"] = {"id": 8, "name": "Korean"}
                            conflicts.append(conflict_id)
                    body = json.dumps(value).encode()
                self.send_response(status)
                self.send_header("Content-Type", "application/json")
                self.send_header("Content-Length", str(len(body)))
                self.end_headers()
                self.wfile.write(body)
            except (OSError, ValueError, HarnessError, urllib.error.URLError):
                self.send_error(502)

        do_GET = handle_request
        do_POST = handle_request
        do_PUT = handle_request
        do_DELETE = handle_request
        do_PATCH = handle_request
        do_HEAD = handle_request
        do_OPTIONS = handle_request
        do_TRACE = handle_request

    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        yield f"http://127.0.0.1:{server.server_port}{prefix}", calls, conflicts
    finally:
        server.shutdown()
        server.server_close()
        thread.join(timeout=5)


def policy_expected(case):
    tags = {"English": "eng", "Japanese": "jpn", "Korean": "kor", "Polish": "pol"}
    original = case["original_language"]["name"]
    if case["name"] in {"unknown-eng", "conflict-eng"}:
        return None
    require(original in tags, f"Unexpected policy fixture original: {original}")
    required = {"eng", tags[original]}
    return required, set(case["tracks"]), required - set(case["tracks"])


def check_policy_assessments(cases, records, audit_log, app):
    by_path = {record["path"]: record for record in records}
    require(len(by_path) == len(records), "Duplicate policy cache paths")
    results = []
    for case in cases:
        expected = policy_expected(case)
        record = by_path.pop(case["file_path"], None)
        if expected is None:
            target = case["episode_id"] if app == "sonarr" else case["entity_id"]
            noun = "episode" if app == "sonarr" else "movie"
            require(record is None, f"Invalid metadata was assessed: {case['name']}")
            require(re.search(rf"language audit skipping {noun} {target}: cannot establish original audio language", audit_log),
                    f"Missing fail-closed audit result: {case['name']}")
            results.append({"name": case["name"], "status": "skipped-invalid-metadata"})
            continue
        required, present, missing = expected
        require(record is not None, f"Missing policy assessment: {case['name']}")
        requirements, report = record["requirements"], record["report"]
        require(requirements["enabled"] and len(requirements["audio"]) == len(required)
                and set(requirements["audio"]) == required and not requirements["subtitles"],
                f"Wrong policy requirements: {case['name']}")
        require(set(report["present_audio"]) == present and set(report["missing_audio"]) == missing
                and not report["missing_subtitles"], f"Wrong policy assessment: {case['name']}")
        results.append({"name": case["name"], "status": "missing" if missing else "satisfied",
                        "required_audio": sorted(required), "present_audio": sorted(present),
                        "missing_audio": sorted(missing)})
    require(not by_path, f"Unexpected policy cache records: {list(by_path)}")
    return results


def check_default_policy(case, spec, title, records, audit_log, calls, conflicts):
    (name, _, _, tracks, subtitles, required_audio, required_subtitles, candidate_audio, expected) = spec
    require(len(records) == 1 and records[0]["path"] == case["file_path"],
            f"Expected exactly the selected assessment: {name}")
    requirements, report = records[0]["requirements"], records[0]["report"]
    require(requirements["enabled"] and set(requirements["audio"]) == set(required_audio)
            and set(requirements["subtitles"]) == set(required_subtitles),
            f"Wrong base requirements for default policy: {name}")
    require(set(report["present_audio"]) == set(tracks)
            and set(report["present_subtitles"]) == set(subtitles)
            and set(report["missing_audio"]) == set(required_audio) - set(tracks)
            and set(report["missing_subtitles"]) == set(required_subtitles) - set(subtitles),
            f"Wrong current media assessment: {name}")
    searches = [call for call in calls if call["path"] == "/api/v3/release"]
    expected_searches = int(expected in {"selected", "rejected"})
    require(calls and all(call["method"] == "GET" and not call["blocked"] for call in calls),
            f"Default audit attempted a write or forbidden request: {name}")
    require(len(searches) == expected_searches and all(call["mock_release"] for call in searches),
            f"Unexpected release search or non-synthetic response: {name}")
    if name == "conflict-replacement":
        require(conflicts and all(item == case["entity_id"] for item in conflicts), "Conflict was not injected")
    else:
        require(not conflicts, f"Unexpected conflict injection: {name}")
    if expected == "selected":
        require(f"would grab '{title}'" in audit_log, f"Synthetic candidate was not selected: {name}")
    else:
        require("would grab '" not in audit_log, f"Unexpected selected candidate: {name}")
    if expected == "invalid":
        require("cannot safely preserve current original audio" in audit_log,
                f"Invalid original metadata did not fail closed: {name}")
    counts = {"checked": 1, "missing": int(expected != "compliant"), "searched": expected_searches,
              "dry_run": int(expected == "selected"), "grabbed": 0, "no_candidate": int(expected == "rejected"),
              "skipped_no_candidate_cooldown": 0, "rejected_imports": 0, "errors": int(expected == "invalid")}
    summary = re.search(r"language audit complete: ([^\n]+)", audit_log)
    require(summary is not None, f"Missing default audit summary: {name}")
    observed = {key: int(value) for key, value in re.findall(r"([a-z_]+)=(\d+)", summary.group(1))}
    require(observed == counts, f"Unexpected default audit summary for {name}: {observed}")
    return {"name": name, "status": "passed", "expected": expected,
            "current_audio": list(tracks), "current_subtitles": list(subtitles),
            "required_audio": list(required_audio), "required_subtitles": list(required_subtitles),
            "candidate_audio": list(candidate_audio), "candidate_source": "synthetic-proxy",
            "searches": len(searches), "summary": observed, "proxy_calls": calls}


def insert_metadata(db, table, values):
    """Accept known version-specific columns, reject unknown mandatory fields."""
    require(table in {"Series", "Episodes", "Movies", "MovieMetadata"}, "Not a metadata table")
    schema = db.execute(f'PRAGMA table_info("{table}")').fetchall()
    require(schema, f"Missing table {table}; app must finish migrations before offline seed")
    payload = {}
    for _, name, kind, not_null, default, primary_key in schema:
        if name in values:
            payload[name] = values[name]
        elif not_null and default is None and not primary_key:
            raise HarnessError(f"Unsupported {table} schema: mandatory column {name} ({kind})")
    columns = ",".join(f'"{key}"' for key in payload)
    placeholders = ",".join("?" for _ in payload)
    cursor = db.execute(f'INSERT INTO "{table}" ({columns}) VALUES ({placeholders})', list(payload.values()))
    return cursor.lastrowid


def assert_app_stopped(app):
    port = 8989 if app == "sonarr" else 7878
    with socket.socket() as connection:
        connection.settimeout(0.25)
        require(connection.connect_ex(("127.0.0.1", port)) != 0, "Stop the app before offline seed")
    for entry in Path("/proc").iterdir():
        if not entry.name.isdigit() or int(entry.name) == os.getpid():
            continue
        try:
            executable = (entry / "comm").read_text().strip().lower()
        except (FileNotFoundError, PermissionError, ProcessLookupError):
            continue
        require(executable != app, f"{app} process {entry.name} is still running")


def seed_offline(app, config, data, uid, gid):
    assert_app_stopped(app)
    data = owned_directory(data, uid, gid)
    database = Path(config).parent / f"{app}.db"
    require(database.is_file(), f"Missing initialized DB: {database}")
    library = owned_directory(data / "library", uid, gid)
    destination = owned_directory(library / (TITLE if app == "sonarr" else f"{TITLE} (2020)"), uid, gid)
    now = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M:%S")
    with sqlite3.connect(f"file:{database}?mode=rw", uri=True, timeout=1) as db:
        # Empty-library guard and an exclusive transaction are intentional: this
        # mode is bootstrap-only, not an alternative way to operate a live Arr.
        db.execute("BEGIN EXCLUSIVE")
        entity_table = "Series" if app == "sonarr" else "Movies"
        require(db.execute(f'SELECT count(*) FROM "{entity_table}"').fetchone()[0] == 0,
                "Offline seed refuses a nonempty library")
        profile = db.execute('SELECT "Id" FROM "QualityProfiles" ORDER BY "Id" LIMIT 1').fetchone()
        require(profile is not None, "Quality profiles are not initialized")
        common = {"Title": TITLE, "CleanTitle": "dpne2efixture", "SortTitle": "dpn e2e fixture",
                  "OriginalTitle": TITLE, "CleanOriginalTitle": "dpne2efixture",
                  "TitleSlug": "dpn-e2e-fixture", "TvdbId": EXTERNAL_ID, "TmdbId": EXTERNAL_ID,
                  "TvRageId": 0, "TvMazeId": 0, "ImdbId": "", "Overview": "Synthetic E2E metadata",
                  "Path": str(destination), "Monitored": 1, "QualityProfileId": profile[0],
                  "SeasonFolder": 1, "LastInfoSync": now, "LastDiskSync": now, "Runtime": 1,
                  "SeriesType": 0, "Status": 2 if app == "sonarr" else 3,
                  "BacklogSetting": 0, "UseSceneNumbering": 0, "Year": 2020,
                  "Images": "[]", "Genres": "[]", "Actors": "[]", "Tags": "[]", "Ratings": "{}",
                  "Keywords": "[]", "MalIds": "[]", "AniListIds": "[]", "Recommendations": "[]",
                  "OriginalLanguage": 1, "Added": now, "FirstAired": "2020-01-01 00:00:00",
                  "AirTime": "00:00", "Network": "E2E", "Certification": "", "MonitorNewItems": 0,
                  "Seasons": '[{"seasonNumber":1,"monitored":true}]', "LanguageProfileId": 1,
                  "MinimumAvailability": 0, "MovieFileId": 0, "IsDaily": 0}
        if app == "sonarr":
            entity_id = insert_metadata(db, "Series", common)
            episode_id = insert_metadata(db, "Episodes", {
                "SeriesId": entity_id, "TvdbId": EXTERNAL_ID + 1, "TvDbEpisodeId": EXTERNAL_ID + 1,
                "SeasonNumber": 1, "EpisodeNumber": 1, "Title": "Synthetic Pilot",
                "Overview": "Generated color and tone", "Monitored": 1, "Ignored": 0,
                "EpisodeFileId": 0, "AirDate": "2020-01-01", "AirDateUtc": "2020-01-01 00:00:00",
                "Runtime": 1, "Images": "[]", "Ratings": "{}", "AbsoluteEpisodeNumber": 1,
                "UnverifiedSceneNumbering": 0})
        else:
            require(db.execute('SELECT count(*) FROM "MovieMetadata"').fetchone()[0] == 0,
                    "Offline seed refuses existing movie metadata")
            metadata_id = insert_metadata(db, "MovieMetadata", common)
            entity_id = insert_metadata(db, "Movies", {**common, "MovieMetadataId": metadata_id})
            episode_id = None
        language_table = "Series" if app == "sonarr" else "MovieMetadata"
        columns = {row[1] for row in db.execute(f'PRAGMA table_info("{language_table}")')}
        require("OriginalLanguage" in columns, f"Missing {language_table}.OriginalLanguage")
        policy, default_policy = [], []
        specs = [(name, language_id, language_name, tracks, False)
                 for name, language_id, language_name, tracks in POLICY_CASES]
        specs += [(name, language_id, language_name, tracks, True)
                  for name, language_id, language_name, tracks, *_ in DEFAULT_POLICY_CASES]
        for index, (name, language_id, language_name, tracks, is_default) in enumerate(specs, 1):
            title = f"DPN Policy {name}"
            clean = "dpnpolicy" + name.replace("-", "")
            path = owned_directory(library / (title if app == "sonarr" else f"{title} (2020)"), uid, gid)
            external_id = EXTERNAL_ID + 100 + index * 2
            values = {**common, "Title": title, "OriginalTitle": title, "CleanTitle": clean,
                      "CleanOriginalTitle": clean, "SortTitle": title.lower(), "TitleSlug": f"dpn-policy-{name}",
                      "TvdbId": external_id, "TmdbId": external_id, "Path": str(path),
                      "OriginalLanguage": language_id}
            if app == "sonarr":
                policy_id = insert_metadata(db, "Series", values)
                policy_episode = insert_metadata(db, "Episodes", {
                    "SeriesId": policy_id, "TvdbId": external_id + 1, "TvDbEpisodeId": external_id + 1,
                    "SeasonNumber": 1, "EpisodeNumber": 1, "Title": "Synthetic Pilot",
                    "Overview": "Generated color and tones", "Monitored": 1, "Ignored": 0,
                    "EpisodeFileId": 0, "AirDate": "2020-01-01", "AirDateUtc": "2020-01-01 00:00:00",
                    "Runtime": 1, "Images": "[]", "Ratings": "{}", "AbsoluteEpisodeNumber": 1,
                    "UnverifiedSceneNumbering": 0})
            else:
                metadata_id = insert_metadata(db, "MovieMetadata", values)
                policy_id = insert_metadata(db, "Movies", {**values, "MovieMetadataId": metadata_id})
                policy_episode = None
            case = {"name": name, "entity_id": policy_id, "episode_id": policy_episode,
                    "title": title, "path": str(path), "external_id": external_id,
                    "original_language": {"id": language_id, "name": language_name},
                    "tracks": list(tracks)}
            (default_policy if is_default else policy).append(case)
        db.commit()
    seed = {"app": app, "entity_id": entity_id, "episode_id": episode_id,
            "path": str(destination), "title": TITLE, "external_id": EXTERNAL_ID,
            "metadata_only": True, "policy": policy, "default_policy": default_policy}
    atomic_json(data / "seed.json", seed)
    return seed


class Harness:
    def __init__(self, args):
        self.args = args
        self.data = owned_directory(args.data, args.uid, args.gid)
        self.api = Api(args.app, args.config, self.data, args.timeout)
        self.sonarr = args.app == "sonarr"
        self.entity_kind = "series" if self.sonarr else "movie"
        self.file_kind = "episodefile" if self.sonarr else "moviefile"
        self.events_file = self.data / "hook-events.jsonl"
        self.results = {"app": args.app, "status": "running", "cases": [], "started_at": time.time()}

    def mode(self, value):
        require(value in {"normal", "fail", "false"}, "Unknown hook mode")
        (self.data / "hook-mode").write_text(value + "\n")
        (self.data / "hook-mode").chmod(0o644)

    def downloads(self):
        return [event for raw in read_events(self.events_file)
                for event in [normalize_event(raw, self.args.app)]
                if event.get("event", "").lower() == "download"
                and event.get("phase", "end") == "end"]

    def configure(self):
        self.mode("normal")
        status = poll(lambda: self.api.get("system/status"), "app startup", self.args.timeout)
        self.results["version"] = status.get("version")
        self.seed = json.loads((self.data / "seed.json").read_text())
        require(self.seed["app"] == self.args.app and self.seed["metadata_only"], "Wrong seed manifest")
        self.entity_id = self.seed["entity_id"]
        entity = self.api.get(f"{self.entity_kind}/{self.entity_id}")
        require(entity["title"] == TITLE and entity["path"] == self.seed["path"], "Wrong Arr-tracked entity")
        require(not self.downloads(), "Use a fresh pod: hook events already exist")
        require(not self.tracked_files(), "Use a fresh pod: imported media already exists")
        library = str(self.data / "library")
        if not any(root["path"] == library for root in self.api.get("rootfolder")):
            self.api.request("POST", "rootfolder", {"path": library})
        media = self.api.get("config/mediamanagement")
        media.update({"copyUsingHardlinks": True, "setPermissionsLinux": True,
                      "chmodFolder": "755", "chownGroup": str(self.args.gid),
                      "recycleBin": "", "skipFreeSpaceCheckWhenImporting": True,
                      "minimumFreeSpaceWhenImporting": 100})
        self.api.request("PUT", f"config/mediamanagement/{media['id']}", media)
        naming = self.api.get("config/naming")
        naming["renameEpisodes" if self.sonarr else "renameMovies"] = False
        self.api.request("PUT", f"config/naming/{naming['id']}", naming)
        definitions = self.api.get("qualitydefinition")
        self.qualities = {entry["quality"]["name"]: entry["quality"] for entry in definitions}
        require("HDTV-720p" in self.qualities and "HDTV-1080p" in self.qualities,
                "Expected built-in quality definitions are missing")
        self.run_original_audio_policy()
        self.run_default_original_audio_policy()
        schema = next(item for item in self.api.get("notification/schema")
                      if item["implementation"] == "CustomScript")
        schema.pop("id", None)
        for key in list(schema):
            if key.startswith("on") and isinstance(schema[key], bool):
                schema[key] = False
        schema.update({"name": "DPN real import E2E", "onDownload": True, "onUpgrade": True, "tags": []})
        for field in schema["fields"]:
            if field["name"].lower() == "path":
                field["value"] = "/opt/e2e/hook.py"
            elif field["name"].lower() == "arguments":
                field["value"] = ""
        self.notification = self.api.request("POST", "notification", schema)

    def import_policy_fixture(self, case, subtitles=()):
        entity = self.api.get(f"{self.entity_kind}/{case['entity_id']}")
        require(entity["id"] == case["entity_id"] and entity["title"] == case["title"]
                and entity["path"] == case["path"]
                and entity.get("originalLanguage") == case["original_language"],
                f"Wrong real Arr metadata for {case['name']}")
        if self.sonarr:
            episode = self.api.get(f"episode/{case['episode_id']}")
            require(episode["id"] == case["episode_id"] and episode["seriesId"] == case["entity_id"],
                    f"Wrong series association for {case['name']}")
        token = "S01E01" if self.sonarr else "2020"
        filename = f"DPN.Policy.{case['name']}.{token}.HDTV-720p.mkv"
        source = self.data / "downloads" / f"policy-{case['name']}" / filename
        before = generate_fixture(source, "blue", self.args.uid, self.args.gid, case["tracks"], subtitles)
        verify_media(source, "mpeg4", "blue", case["tracks"], subtitles)
        payload = {"path": str(source), "folderName": source.parent.name,
                   "quality": {"quality": self.qualities["HDTV-720p"],
                               "revision": {"version": 1, "real": 0, "isRepack": False}},
                   "languages": [{"id": 1, "name": "English"}], "releaseGroup": "E2E", "indexerFlags": 0}
        if self.sonarr:
            payload.update({"seriesId": case["entity_id"], "episodeIds": [case["episode_id"]]})
        else:
            payload["movieId"] = case["entity_id"]
        self.api.command("ManualImport", files=[payload], importMode="copy")
        key = "seriesId" if self.sonarr else "movieId"
        files = self.api.get(self.file_kind, **{key: case["entity_id"]})
        require(len(files) == 1, f"Expected one policy file for {case['name']}: {files}")
        file = files[0]
        path = Path(file.get("path") or str(Path(case["path"]) / file["relativePath"]))
        require(path.is_relative_to(Path(case["path"])) and path.is_file(),
                f"Policy file is missing or outside its title: {case['name']}")
        if self.sonarr:
            episode = self.api.get(f"episode/{case['episode_id']}")
            require(episode["hasFile"] and episode["episodeFileId"] == file["id"],
                    f"Policy episode association is stale: {case['name']}")
        else:
            entity = self.api.get(f"movie/{case['entity_id']}")
            require(entity["hasFile"] and entity["movieFile"]["id"] == file["id"],
                    f"Policy movie association is stale: {case['name']}")
        verify_media(path, "mpeg4", "blue", case["tracks"], subtitles)
        require(file_snapshot(source)["sha256"] == before["sha256"], "Policy import changed source")
        case["file_path"] = str(path)
        case["before_audit"] = file_snapshot(path)
        case["source_path"] = str(source)
        case["source_before_audit"] = file_snapshot(source)

    def run_original_audio_policy(self):
        require(not self.api.get("indexer") and not self.api.get("downloadclient"),
                "Policy E2E requires no indexers or download clients")
        require(not self.api.get("notification"), "Policy imports require an empty notification list")
        cases = self.seed["policy"]
        require(len(cases) == len(POLICY_CASES), "Incomplete policy seed")
        for case in cases:
            self.import_policy_fixture(case)
        require(not self.downloads(), "Policy fixtures unexpectedly invoked a Download hook")

        conflict = next(case for case in cases if case["name"] == "conflict-eng")
        cache = self.data / "policy-audit.json"
        require(not cache.exists(), "Policy audit requires a fresh assessment cache")
        env = {"PATH": os.environ.get("PATH", "/usr/bin:/bin"), "HOME": str(self.data),
               "RUST_LOG": "info", "DIRECT_PLAY_NICE_LANGUAGE_CACHE": str(cache),
               f"DIRECT_PLAY_NICE_{self.args.app.upper()}_API_KEY": self.api.key}
        with audit_proxy(self.args.app, conflict["entity_id"]) as (url, calls, conflicts):
            argv = ["/opt/dpn/direct_play_nice", "--servarr-language-audit",
                    "--servarr-language-audit-scope", "latest-missing",
                    "--servarr-language-audit-max-searches", "0", "--servarr-language-dry-run",
                    "--servarr-preserve-original-audio", "--required-audio-languages", "eng",
                    "--servarr-api-url", url]
            completed = subprocess.run(argv, env=env, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                                       timeout=self.args.timeout)
        log = completed.stdout.decode(errors="replace").replace(self.api.key, "[redacted]")
        (self.data / "policy-audit.log").write_text(log)
        atomic_json(self.data / "policy-proxy.json", {"calls": calls, "conflict_injections": len(conflicts)})
        require(completed.returncode == 0, f"Policy audit exited {completed.returncode}; inspect policy-audit.log")
        require(conflicts and all(item == conflict["entity_id"] for item in conflicts),
                "Conflict response was not exercised")
        require(calls and not any(call["blocked"] for call in calls), "Audit attempted a search or disallowed action")
        require(cache.is_file(), "Policy audit did not write its assessment cache")
        assessments = json.loads(cache.read_text())
        results = check_policy_assessments(cases, assessments, log, self.args.app)
        missing_count = sum(result["status"] == "missing" for result in results)
        invalid_count = sum(result["status"] == "skipped-invalid-metadata" for result in results)
        summary_pattern = (rf"language audit complete: checked={len(cases)}, missing={missing_count}, "
                           rf"searched=0, dry_run=0, grabbed=0, no_candidate=0, "
                           rf"skipped_no_candidate_cooldown=0, rejected_imports=0, errors={invalid_count}\b")
        require(re.search(summary_pattern, log), "Unexpected policy audit summary; inspect policy-audit.log")
        for case in cases:
            require(file_snapshot(case["file_path"]) == case["before_audit"],
                    f"Policy audit changed media: {case['name']}")
        self.results["original_audio_policy"] = {"status": "passed", "cases": results,
                                                  "proxy_calls": calls, "conflict_injections": len(conflicts)}
        atomic_json(self.data / "e2e-results.json", self.results)
        print(f"PASS {self.args.app}: original audio policy ({len(results)} cases)", flush=True)

    def run_default_original_audio_policy(self):
        cases = self.seed["default_policy"]
        require(len(cases) == len(DEFAULT_POLICY_CASES), "Incomplete default-policy seed")
        results = []
        for case, spec in zip(cases, DEFAULT_POLICY_CASES):
            (name, language_id, language_name, tracks, subtitles, required_audio,
             required_subtitles, candidate_audio, expected) = spec
            require(case["name"] == name and case["tracks"] == list(tracks)
                    and case["original_language"] == {"id": language_id, "name": language_name},
                    f"Wrong default-policy seed: {name}")
            self.import_policy_fixture(case, subtitles)
            title = f"DPN Synthetic {name} candidate"
            language_names = {"eng": "English", "jpn": "Japanese", "kor": "Korean", "pol": "Polish"}
            release = {"title": title, "guid": f"synthetic-{name}", "indexerId": 990001,
                       "languages": [{"name": language_names[tag]} for tag in candidate_audio],
                       "subtitleLanguages": [{"name": "English"}], "rejected": False}
            if self.sonarr:
                release.update({"episodeIds": [case["episode_id"]], "seriesTitle": case["title"]})
            else:
                release["movieId"] = case["entity_id"]
            cache = self.data / f"default-policy-{name}.json"
            require(not cache.exists(), f"Default policy cache already exists: {name}")
            env = {"PATH": os.environ.get("PATH", "/usr/bin:/bin"), "HOME": str(self.data),
                   "RUST_LOG": "info", "DIRECT_PLAY_NICE_LANGUAGE_CACHE": str(cache),
                   f"DIRECT_PLAY_NICE_{self.args.app.upper()}_API_KEY": self.api.key}
            conflict_id = case["entity_id"] if name == "conflict-replacement" else None
            with audit_proxy(self.args.app, conflict_id, target=case, synthetic_releases=[release]) as (url, calls, conflicts):
                argv = ["/opt/dpn/direct_play_nice", "--servarr-language-audit",
                        "--servarr-language-audit-scope", "latest-missing",
                        "--servarr-language-audit-max-searches", "1", "--servarr-language-dry-run",
                        "--servarr-language-candidate-policy", "strict", "--servarr-api-url", url]
                if required_audio:
                    argv += ["--required-audio-languages", ",".join(required_audio)]
                if required_subtitles:
                    argv += ["--required-subtitle-languages", ",".join(required_subtitles)]
                completed = subprocess.run(argv, env=env, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                                           timeout=self.args.timeout)
            log = completed.stdout.decode(errors="replace").replace(self.api.key, "[redacted]")
            (self.data / f"default-policy-{name}.log").write_text(log)
            atomic_json(self.data / f"default-policy-{name}-proxy.json",
                        {"calls": calls, "conflict_injections": len(conflicts), "candidate": release})
            require(completed.returncode == 0, f"Default audit exited {completed.returncode}: {name}")
            require(cache.is_file(), f"Default audit did not write assessment: {name}")
            assessments = json.loads(cache.read_text())
            result = check_default_policy(case, spec, title, assessments, log, calls, conflicts)
            require(file_snapshot(case["file_path"]) == case["before_audit"]
                    and file_snapshot(case["source_path"]) == case["source_before_audit"],
                    f"Default audit changed media: {name}")
            results.append(result)
            self.results["default_original_audio_policy"] = {"status": "running", "cases": results}
            atomic_json(self.data / "e2e-results.json", self.results)
            print(f"PASS {self.args.app}: default policy {name}", flush=True)
        require(not self.downloads() and not self.api.get("indexer") and not self.api.get("downloadclient"),
                "Default policy run changed notifications, indexers, or download clients")
        self.results["default_original_audio_policy"] = {"status": "passed", "cases": results}
        atomic_json(self.data / "e2e-results.json", self.results)

    def upgrades_enabled(self, enabled):
        self.mode("normal")  # Provider validation may send a real Test event.
        self.notification["onDownload"] = True
        self.notification["onUpgrade"] = enabled
        self.notification = self.api.request("PUT", f"notification/{self.notification['id']}", self.notification)
        saved = self.api.get(f"notification/{self.notification['id']}")
        require(saved["onUpgrade"] is enabled and saved["onDownload"], "Notification flags were not persisted")

    def tracked_files(self):
        key = "seriesId" if self.sonarr else "movieId"
        return self.api.get(self.file_kind, **{key: self.entity_id})

    def tracked_path(self, require_exists=True):
        files = self.tracked_files()
        require(len(files) == 1, f"Expected exactly one tracked file, got {files}")
        file = files[0]
        path = Path(file.get("path") or str(Path(self.seed["path"]) / file["relativePath"]))
        require(path.is_relative_to(Path(self.seed["path"])), f"Tracked file escaped fixture library: {path}")
        if require_exists:
            require(path.is_file(), f"Arr tracks a missing file: {path}")
        if self.sonarr:
            episode = self.api.get(f"episode/{self.seed['episode_id']}")
            require(episode["hasFile"] and episode["episodeFileId"] == file["id"], "Episode file association is stale")
        else:
            movie = self.api.get(f"movie/{self.entity_id}")
            require(movie["hasFile"] and movie["movieFile"]["id"] == file["id"], "Movie file association is stale")
        return file, path

    def import_history(self):
        key = "seriesIds" if self.sonarr else "movieIds"
        history = self.api.get("history", page=1, pageSize=100, sortKey="date", sortDirection="descending",
                               **{key: self.entity_id})
        # Both APIs serialize HistoryEventType.DownloadFolderImported as this
        # enum string (older versions return its underlying value, 3).
        entity_key = "seriesId" if self.sonarr else "movieId"
        return [row for row in history["records"]
                if row["eventType"] in ("downloadFolderImported", 3)
                and row.get(entity_key) == self.entity_id]

    def rescan(self):
        return self.api.command("RescanSeries" if self.sonarr else "RescanMovie",
                                **{("seriesId" if self.sonarr else "movieId"): self.entity_id})

    def quiet(self, expected, duration=2):
        deadline = time.monotonic() + duration
        def observed():
            require(len(self.downloads()) == expected, "Unexpected/repeated Download hook invocation")
            self.tracked_path()  # Repeatedly observe Arr, not an unconditional sleep.
            return time.monotonic() >= deadline
        poll(observed, "no additional Download events after completed command", duration + 10)

    def verify_permissions(self, snapshot):
        require(snapshot["uid"] == self.args.uid and snapshot["gid"] == self.args.gid,
                f"Wrong media ownership: {snapshot}")
        require(snapshot["mode"] & 0o600 == 0o600 and not snapshot["mode"] & 0o002,
                f"Unsafe or unwritable media permissions: {snapshot}")

    def import_case(self, name, color, quality, revision, upgrade, mode="normal", hook=True):
        started = time.monotonic()
        self.mode(mode)
        before_events = len(self.downloads())
        before_history = {row["id"] for row in self.import_history()}
        old_files = self.tracked_files()
        token = "S01E01" if self.sonarr else "2020"
        filename = f"DPN.E2E.Fixture.{token}.{quality}.v{revision}.{name}.mkv"
        source = self.data / "downloads" / name / filename
        source_before = generate_fixture(source, color, self.args.uid, self.args.gid)
        self.verify_permissions(source_before)
        verify_media(source, "mpeg4", color)
        payload = {"path": str(source), "folderName": source.parent.name,
                   "quality": {"quality": self.qualities[quality],
                               "revision": {"version": revision, "real": 0, "isRepack": False}},
                   "languages": [{"id": 1, "name": "English"}], "releaseGroup": "E2E", "indexerFlags": 0}
        if self.sonarr:
            payload.update({"seriesId": self.entity_id, "episodeIds": [self.seed["episode_id"]]})
        else:
            payload["movieId"] = self.entity_id
        command = self.api.command("ManualImport", files=[payload], importMode="copy")
        events = self.downloads()[before_events:]
        if hook:
            events = poll(lambda: self.downloads()[before_events:], f"{name} completed Download hook", self.args.timeout)
            require(len(events) == 1, f"Expected one real Download event: {events}")
            event = events[0]
            require(event["is_upgrade"] is upgrade, f"Wrong real isUpgrade flag: {event}")
            require(event["mode"] == mode, f"Wrong hook mode: {event}")
            require(event["uid"] == self.args.uid and event["gid"] == self.args.gid, "Hook did not run as app user")
            require((event["returncode"] == 0) is (mode == "normal"), f"Unexpected hook result: {event}")
            require(event["before"]["sha256"] == source_before["sha256"], "Hook did not receive imported fixture")
            require(same_inode(event["before"], source_before) and event["before"]["nlink"] >= 2,
                    "Arr copy import did not create a hardlink before invoking hook")
            self.verify_permissions(event["before"])
        else:
            require(not events, "Disabled OnUpgrade still invoked Download hook")
        imported_file, imported_path = self.tracked_path(require_exists=False)
        file = imported_file
        require(not old_files or file["id"] != old_files[0]["id"], "Upgrade did not replace Arr's tracked file")
        require(file["quality"]["quality"]["id"] == self.qualities[quality]["id"], "Arr did not track chosen import quality")
        require(file["quality"]["revision"]["version"] == revision, "Arr did not track revision")
        added_history = [row for row in self.import_history() if row["id"] not in before_history]
        require(len(added_history) == 1, f"Manual import did not create exactly one real import history event: {added_history}")
        converted = hook and mode == "normal"
        original_path = Path(events[0]["path"]) if hook else imported_path
        output = converted_path(original_path) if converted else original_path
        require(output.is_file(), f"Missing expected production output: {output}")
        if converted:
            require(not original_path.exists(), f"Successful conversion left original library input: {original_path}")
            require(events[0]["after"] is None, "Successful suffix conversion did not remove imported input")
        immediate_tracking = {"path": str(imported_path), "exists": imported_path.is_file(),
                              "file_id": imported_file["id"], "matches_output": imported_path == output,
                              "quality": imported_file["quality"]}
        # Persist the stale-path observation even if reconciliation subsequently fails.
        self.results["pending_case"] = {"name": name, "tracking_before_rescan": immediate_tracking,
                                        "expected_output": str(output)}
        atomic_json(self.data / "e2e-results.json", self.results)
        reconciliation = self.rescan()
        file, tracked_output = self.tracked_path()
        require(tracked_output == output, f"Arr rescan did not track converted output: {tracked_output} != {output}")
        after = file_snapshot(output)
        source_after = file_snapshot(source)
        require(source_after["sha256"] == source_before["sha256"] and same_inode(source_after, source_before),
                "DPN modified/replaced hardlinked download source")
        self.verify_permissions(after)
        if converted:
            require(after["sha256"] != source_before["sha256"] and not same_inode(after, source_after),
                    "Successful DPN hook did not atomically replace the library hardlink")
        else:
            require(after["sha256"] == source_before["sha256"] and same_inode(after, source_after),
                    "Disabled/failing hook did not preserve imported source")
            if hook:
                require(events[0]["after"] == events[0]["before"], "Failed hook changed source metadata/content")
        media = verify_media(output, "h264" if converted else "mpeg4", color)
        library_files = sorted(path for path in Path(self.seed["path"]).rglob("*")
                               if path.suffix.lower() in {".mkv", ".mp4"})
        require(library_files == [output], f"Orphaned/staging/backup media in library: {library_files}")
        leftovers = [str(path) for path in Path(self.seed["path"]).rglob("*")
                     if ".direct-play-nice.tmp" in path.name or ".direct-play-nice.bak" in path.name]
        require(not leftovers, f"Unclean DPN staging files: {leftovers}")
        expected_events = before_events + int(hook)
        # Actual repeated scans must leave one associated file and not rerun DPN.
        for _ in range(2):
            self.rescan()
            rescanned_file, rescanned_path = self.tracked_path()
            require(rescanned_file["id"] == file["id"] and rescanned_path == output,
                    "Rescan changed association or duplicated media")
            require(file_snapshot(output) == after, "Rescan modified media")
        self.quiet(expected_events)
        require({row["id"] for row in self.import_history()} == before_history | {added_history[0]["id"]},
                "Rescan generated duplicate import history")
        result = {"name": name, "status": "passed", "seconds": round(time.monotonic() - started, 3),
                  "command_id": command["id"], "is_upgrade": upgrade, "hook_enabled": hook,
                  "hook_mode": mode, "hook_events": events, "tracked_file_id": file["id"],
                  "output_path": str(output), "source_path": str(source), "source_before": source_before,
                  "source_after": source_after, "output": after, "media": media,
                  "history_id": added_history[0]["id"], "repeat_scans": 2,
                  "tracking_before_rescan": immediate_tracking,
                  "tracking_after_rescan": {"path": str(tracked_output), "quality": file["quality"]},
                  "reconciliation_command_id": reconciliation["id"],
                  "reconciliation_required": not immediate_tracking["matches_output"]}
        self.results.pop("pending_case", None)
        self.results["cases"].append(result)
        atomic_json(self.data / "e2e-results.json", self.results)
        print(f"PASS {self.args.app}: {name}", flush=True)

    def run(self):
        try:
            self.configure()
            self.import_case("initial", "blue", "HDTV-720p", 1, False)
            self.import_case("upgrade", "red", "HDTV-1080p", 1, True)
            self.upgrades_enabled(False)
            self.import_case("upgrade-disabled", "green", "HDTV-1080p", 2, True, hook=False)
            self.upgrades_enabled(True)
            self.import_case("upgrade-restored", "yellow", "HDTV-1080p", 3, True)
            self.import_case("dpn-failure", "magenta", "HDTV-1080p", 4, True, mode="fail")
            # A successful real provider Test clears any notification backoff
            # before independently exercising the false-hook control.
            self.mode("normal")
            self.api.request("POST", "notification/test", self.notification)
            self.import_case("false-hook", "cyan", "HDTV-1080p", 5, True, mode="false")
            self.results["status"] = "passed"
        except Exception as exc:
            self.results.update({"status": "failed", "error": str(exc), "traceback": traceback.format_exc()})
            raise
        finally:
            self.mode("normal")
            self.results["finished_at"] = time.time()
            atomic_json(self.data / "e2e-results.json", self.results)


def parse_args(argv=None):
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--app", choices=("sonarr", "radarr"), required=True)
    parser.add_argument("--seed-offline", action="store_true")
    parser.add_argument("--config", default="/config/config.xml")
    parser.add_argument("--data", default="/data")
    parser.add_argument("--uid", type=int, default=1000)
    parser.add_argument("--gid", type=int, default=1000)
    parser.add_argument("--timeout", type=float, default=180)
    return parser.parse_args(argv)


def main(argv=None):
    args = parse_args(argv)
    require(os.environ.get("DPN_E2E_EPHEMERAL") == "1", "Set DPN_E2E_EPHEMERAL=1 only in the disposable test pod")
    require(Path(args.data).is_absolute() and Path(args.config).is_absolute(), "Use absolute pod paths")
    require(args.uid != 0 and args.gid != 0, "App/hooks must run non-root")
    if args.seed_offline:
        print(json.dumps(seed_offline(args.app, args.config, args.data, args.uid, args.gid), indent=2))
    else:
        Harness(args).run()


if __name__ == "__main__":
    main()
