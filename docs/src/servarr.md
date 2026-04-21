# Sonarr/Radarr Integration

`direct-play-nice` can be wired as a custom script in Sonarr/Radarr pipelines.

## High-level flow

1. Arr service imports media.
2. Custom script invokes `direct_play_nice`.
3. Successful conversion output replaces source according to configured behavior.

## Naming and replacement notes

- Use `--servarr-output-extension` and `--servarr-output-suffix` to control output naming.
- `--delete-source` applies to direct CLI usage.
- In Sonarr/Radarr mode, replacement/rollback logic is handled by integration flow.

For service-specific script setup, see the Servarr docs:

- <https://wiki.servarr.com/sonarr/custom-scripts>
- <https://wiki.servarr.com/radarr/custom-scripts>
