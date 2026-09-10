#!/usr/bin/env python3
"""Observe real Arr hook invocations without manufacturing their environment."""
import json
import os
from pathlib import Path
import subprocess
import sys
import time
import uuid

from harness import file_snapshot


def main():
    invocation = uuid.uuid4().hex
    # Only record fields needed for assertions, never API keys or entire envs.
    fields = ("eventtype", "isupgrade", "moviefile_path", "episodefile_path",
              "episodefile_paths", "moviefile_sourcepath", "episodefile_sourcepath")
    event = {key: value for key, value in os.environ.items()
             if any(key.lower() == f"{app}_{field}"
                    for app in ("sonarr", "radarr") for field in fields)}
    root = Path("/data")
    lowered = {key.lower(): value for key, value in os.environ.items()}
    app = "sonarr" if "sonarr_eventtype" in lowered else "radarr"
    path = lowered.get(f"{app}_moviefile_path") or lowered.get(f"{app}_episodefile_path")
    if not path:
        path = lowered.get(f"{app}_episodefile_paths", "").split("|")[0]
    mode_file = root / "hook-mode"
    mode = mode_file.read_text().strip() if mode_file.exists() else "normal"
    event.update(invocation=invocation, uid=os.getuid(), gid=os.getgid(), time=time.time(),
                 event=lowered.get(f"{app}_eventtype"),
                 is_upgrade=lowered.get(f"{app}_isupgrade", "false").lower() == "true",
                 path=path, mode=mode, before=file_snapshot(path) if path else None)
    log = root / f"hook-{invocation}.log"

    def record(phase, **extra):
        line = json.dumps(dict(event, phase=phase, **extra)) + "\n"
        with (root / "hook-events.jsonl").open("a") as stream:
            stream.write(line)

    record("start")
    env = {key: value for key, value in os.environ.items()
           if key.upper() not in ("PLEX_URL", "PLEX_TOKEN")}
    env["DIRECT_PLAY_NICE_LOCK_DIR"] = "/data/dpn-locks"
    # Fault modes affect only real media events, never provider Test validation.
    mode = mode if event["event"] == "Download" else "normal"
    config = "/opt/e2e/fail.toml" if mode == "fail" else "/opt/e2e/dpn.toml"
    argv = (["/bin/false"] if mode == "false" else
            ["/opt/dpn/direct_play_nice", "--config-file", config])
    with log.open("w") as stream:
        stream.write(json.dumps({"argv": argv}) + "\n")
        stream.flush()
        try:
            result = subprocess.run(
                argv,
                env=env, stdout=stream, stderr=subprocess.STDOUT, timeout=120,
            )
            code = result.returncode
        except subprocess.TimeoutExpired:
            code = 124
    record("end", returncode=code, log=str(log),
           after=file_snapshot(path) if path else None)
    return code


if __name__ == "__main__":
    sys.exit(main())
