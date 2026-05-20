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

## Optional language mismatch replacement

Language checks are off by default. When enabled on a Sonarr/Radarr `Download`
event, `direct-play-nice` inspects the imported file before conversion. If any
configured audio or subtitle language is missing, conversion is skipped and DPN
asks Sonarr/Radarr for manual-search release results. It only grabs a specific
replacement release when the returned release metadata or configured candidate
policy indicates the required languages. Existing-file/cutoff rejections may be
overridden for language upgrades, but unrelated rejection reasons are still
honored. If no verified candidate is available, DPN leaves the current file
untouched and does not request a blind redownload.

### CLI examples

Dry-run first while tuning your matching rules. This inspects the imported file,
queries Arr for replacement candidates when requirements are missing, and reports
what would happen without grabbing or blocklisting anything:

```bash
/path/to/direct_play_nice \
  --servarr-language-check \
  --servarr-language-dry-run \
  --servarr-language-candidate-policy custom-format-or-title \
  --required-audio-languages eng,jpn \
  --required-subtitle-languages eng \
  --servarr-api-url http://127.0.0.1:8989 \
  --servarr-api-key "$SONARR_API_KEY"
```

After dry-run output looks correct, remove `--servarr-language-dry-run` to allow
DPN to grab the selected replacement and blocklist the old history item:

```bash
/path/to/direct_play_nice \
  --servarr-language-check \
  --servarr-language-candidate-policy custom-format-or-title \
  --required-audio-languages eng,jpn \
  --required-subtitle-languages eng \
  --servarr-api-url http://127.0.0.1:8989 \
  --servarr-api-key "$SONARR_API_KEY"
```

### Config-file example

In a Sonarr/Radarr custom script, prefer keeping the command short and putting
policy in the DPN config file:

```bash
/path/to/direct_play_nice --config-file /path/to/direct-play-nice-sonarr.toml
```

```toml
servarr_language_check = true
servarr_language_audit = true
servarr_language_audit_lookback_days = 30
servarr_language_audit_max_searches = 20
required_audio_languages = "eng,jpn"
required_subtitle_languages = "eng"
servarr_api_url = "http://127.0.0.1:8989"
servarr_api_key = "..."

# Recommended while tuning rules. Logs the selected candidate but does not grab
# or blocklist anything.
servarr_language_dry_run = true

# strict only trusts explicit Arr language/subtitle metadata. custom-format-or-title
# also trusts matching custom formats and strong tokens like Dual-Audio/Multi-Subs.
servarr_language_candidate_policy = "custom-format-or-title"
```

For Radarr, use the Radarr URL/key instead:

```toml
servarr_api_url = "http://127.0.0.1:7878"
servarr_api_key = "..."
```

`servarr_api_url` and `servarr_api_key` can also be supplied with CLI flags or
environment variables. Supported env names include `SONARR_URL`,
`SONARR_API_KEY`, `RADARR_URL`, and `RADARR_API_KEY`.

Before grabbing a verified replacement, DPN marks the current Arr history item as
failed so the old release is blocklisted. It resolves that history item from the
Arr download id when available, or from `DIRECT_PLAY_NICE_SONARR_HISTORY_ID` /
`DIRECT_PLAY_NICE_RADARR_HISTORY_ID` if your wrapper provides it.

For Radarr, a language-better file can be stuck as a completed queue item when
Radarr considers it a quality downgrade. Audit mode checks those pending imports;
when the pending file satisfies DPN's language policy, apply mode deletes the old
movie-file entry, posts Radarr manual import, and removes the completed queue
item without deleting the downloaded replacement.

Candidate policies control how much DPN infers before a replacement is grabbed:

- `strict`: only explicit Arr language/subtitle metadata.
- `custom-format`: explicit metadata plus matching Arr custom format names such
  as `Anime-multi-audio` or `anime-multi-sub`.
- `custom-format-or-title`: custom formats plus strong title tokens such as
  `Dual-Audio`, `Multi-Audio`, `Multi-Subs`, or `MSubs`.
- `title-guess`: looser title matching. Use dry-run first.

DPN always treats the already-imported file differently from candidate releases:
actual file compliance is based on FFmpeg stream metadata, then cached by DPN.
The best-effort cache defaults to
`$XDG_CACHE_HOME/direct-play-nice/servarr-language-cache.json` or
`~/.cache/direct-play-nice/servarr-language-cache.json`; override it with
`DIRECT_PLAY_NICE_LANGUAGE_CACHE`.

## Periodic language audit

The Download-event hook only runs when Arr imports a file. To catch delayed dubs
or subtitles that appear days later, run an audit from cron/systemd/launchd with
no Arr custom-script environment variables:

```bash
/path/to/direct_play_nice \
  --config-file /path/to/direct-play-nice-sonarr.toml \
  --servarr-language-audit \
  --servarr-language-audit-lookback-days 30 \
  --servarr-language-audit-max-searches 20 \
  --servarr-language-dry-run
```

Audit mode queries recent Sonarr/Radarr imports, inspects the actual imported
file language metadata, updates DPN's cache, and release-searches only
missing-language items up to `--servarr-language-audit-max-searches`. In Radarr
mode it also checks completed pending imports that Radarr refused for quality
hierarchy reasons. Keep dry-run enabled while reviewing reports; remove
`--servarr-language-dry-run` only when you want DPN to grab selected
language-upgrade candidates, blocklist the old history item, and force-import
eligible Radarr pending replacements.

This feature assumes DPN is the authority for language upgrades. To avoid Arr
and DPN fighting each other, keep ordinary Arr quality-only upgrades conservative
or disabled for libraries where DPN should make language-first replacement
decisions.

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
