# Sonarr/Radarr Integration

`direct-play-nice` can be wired as a custom script in Sonarr/Radarr pipelines.

## High-level flow

1. Arr service imports media.
2. Custom script invokes `direct_play_nice`.
3. Successful conversion output replaces source according to configured behavior.

## Event behavior

The binary auto-detects Sonarr/Radarr custom-script invocations:

- `sonarr_eventtype=Download` and `radarr_eventtype=Download` trigger conversion.
- Non-download events (for example `Test`, `Grab`, `Rename`) exit cleanly.

## Naming and replacement notes

- Use `--servarr-output-extension` and `--servarr-output-suffix` to control
  output naming.
- `--delete-source` applies to direct CLI usage.
- In Sonarr/Radarr mode, replacement/rollback logic is handled by integration flow.
- `--servarr-output-extension match-input` keeps the source container.

Example command in Sonarr custom script:

```bash
/path/to/direct_play_nice --config-file /path/to/direct-play-nice-sonarr.toml
```

## Practical wrapper pattern

For GPU OCR environments, keep a stable wrapper script as the command Sonarr
or Radarr calls. This keeps runtime paths and OCR flags centralized.

Example wrapper:

```bash
#!/usr/bin/env bash
set -euo pipefail

export ORT_DYLIB_PATH=\"/opt/onnxruntime/lib/libonnxruntime.so\"
export LD_LIBRARY_PATH=\"/opt/onnxruntime/lib:${LD_LIBRARY_PATH:-}\"

exec /path/to/direct_play_nice --config-file /path/to/direct-play-nice-sonarr.toml
```

This avoids drift between manual shell runs and Arr-triggered runs.

For service-specific script setup, see the Servarr docs:

- <https://wiki.servarr.com/sonarr/custom-scripts>
- <https://wiki.servarr.com/radarr/custom-scripts>
