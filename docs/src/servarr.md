# Sonarr/Radarr Integration

`direct-play-nice` can run as a custom script integration for Sonarr and
Radarr workflows.

## High-level flow

1. Arr service finishes download/import.
2. Custom script runs `direct_play_nice`.
3. Output replaces source when conversion succeeds.

Refer to the root `README.md` for current environment variables and examples,
including sidecar naming and source-deletion behavior.
