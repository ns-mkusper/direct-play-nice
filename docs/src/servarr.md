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
  --required-audio-languages eng \
  --servarr-api-url http://127.0.0.1:8989 \
  --servarr-api-key "$SONARR_API_KEY"
```

After dry-run output looks correct, remove `--servarr-language-dry-run` to allow
DPN to grab the selected replacement and blocklist the old history item:

```bash
/path/to/direct_play_nice \
  --servarr-language-check \
  --servarr-language-candidate-policy custom-format-or-title \
  --required-audio-languages eng \
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
servarr_language_audit_scope = "history"
servarr_language_audit_lookback_days = 30
servarr_language_audit_max_searches = 20
# Optional: limit a Sonarr audit to specific episode IDs.
# servarr_language_audit_episode_ids = "123,456"
required_audio_languages = "eng"
# Leave empty unless subtitle completeness is a goal.
required_subtitle_languages = ""
# Optional: for trusted English-native libraries, retag untagged audio before
# deciding the file is missing English audio.
servarr_untagged_audio_language = "eng"
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

The default examples above require English audio only. Extra audio languages are
allowed; a file with English plus Japanese audio still satisfies
`required_audio_languages = "eng"`. Use stricter audio requirements such as
`eng,jpn` only for libraries where preserving the original language alongside the
dub is required. Leave `required_subtitle_languages` empty if missing subtitles
are acceptable.

Untagged streams are handled conservatively. By default, `und`/empty language
metadata does not satisfy a required language. If a library is known to be
English-native, set `servarr_untagged_audio_language = "eng"` or pass
`--servarr-untagged-audio-language eng` to remux unknown audio streams with an
English tag before DPN searches for replacement releases. The remux uses stream
copy, writes through a temporary file, and never overwrites an explicit
non-unknown language tag. `servarr_language_dry_run = true` only reports the
retag action. Use `servarr_untagged_subtitle_language` only for trusted subtitle
streams; DPN does not run speech recognition or globally infer `und = eng`.

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

By default, audit mode uses `--servarr-language-audit-scope history`: it queries
recent Sonarr/Radarr imports, inspects the actual imported file language
metadata, updates DPN's cache, and release-searches only missing-language items
up to `--servarr-language-audit-max-searches`. Use
`--servarr-language-audit-scope inventory` with Sonarr to inspect the current
library inventory instead of only recent import history. Inventory scope walks
series episode files, checks current media metadata, then uses each missing
item's latest import history entry before any apply-mode grab/blocklist action.
For focused Sonarr batches, pass `--servarr-language-audit-episode-ids` with a
comma-separated episode ID list.

In Radarr mode DPN also checks completed pending imports that Radarr refused for
quality hierarchy reasons. Keep dry-run enabled while reviewing reports; remove
`--servarr-language-dry-run` only when you want DPN to grab selected
language-upgrade candidates, blocklist the old history item, and force-import
eligible Radarr pending replacements.

This feature assumes DPN is the authority for language upgrades. To avoid Arr
and DPN fighting each other, keep ordinary Arr quality-only upgrades conservative
or disabled for libraries where DPN should make language-first replacement
decisions.

## Safe language upgrade runbook

Language replacement is intentionally opt-in and should be rolled out in small,
observable batches. A safe operator workflow is:

1. **Start with a dry-run history audit.** This catches delayed dubs/subs for
   recent imports without scanning the whole library:

   ```bash
   /path/to/direct_play_nice \
     --config-file /path/to/direct-play-nice-sonarr.toml \
     --servarr-language-audit \
     --servarr-language-audit-scope history \
     --servarr-language-audit-lookback-days 30 \
     --servarr-language-audit-max-searches 20 \
     --servarr-language-dry-run
   ```

2. **Use inventory dry-run for backlog discovery.** This is Sonarr-only and can
   be slow on large libraries, so keep the search cap low while tuning:

   ```bash
   /path/to/direct_play_nice \
     --config-file /path/to/direct-play-nice-sonarr.toml \
     --servarr-language-audit \
     --servarr-language-audit-scope inventory \
     --servarr-language-audit-max-searches 25 \
     --servarr-language-dry-run
   ```

3. **Review candidate evidence before apply mode.** Prefer candidates with
   explicit Arr language metadata or strong custom-format/title evidence such as
   `Anime-multi-audio`, `anime-multi-sub`, `Dual-Audio`, `Multi-Audio`, or
   `Multi-Subs`. Treat unrelated rejections such as blocked indexers, unknown
   series, or seed/availability failures as blockers. Do not add subtitle
   requirements unless missing subtitles should trigger redownloads.

4. **Apply only a focused batch.** Use Sonarr episode IDs from the dry-run logs
   to constrain the destructive run. Keep `--servarr-language-audit-max-searches`
   at or below the number of intended items:

   ```bash
   /path/to/direct_play_nice \
     --config-file /path/to/direct-play-nice-sonarr.toml \
     --servarr-language-audit \
     --servarr-language-audit-scope inventory \
     --servarr-language-audit-episode-ids "12345,12346,12347" \
     --servarr-language-audit-max-searches 3
   ```

5. **Watch Arr's queue and history.** A successful Sonarr language replacement
   usually goes `grabbed` → `downloadFolderImported`; the imported file should
   then show the required audio/subtitle languages in Arr media info. Some
   candidates may remain queued/downloading for a while, and failed alternates in
   history do not necessarily mean the current queued candidate failed.

6. **Verify and repeat.** Re-run the same command with
   `--servarr-language-dry-run`. Completed items should no longer be searched;
   remaining missing items should either show a queued candidate, no candidate,
   or a rejection reason to fix before another apply batch.

For Radarr, keep the same dry-run-first workflow. In apply mode, DPN may
force-import completed pending replacements that satisfy the language policy by
deleting the old Radarr movie-file entry, posting manual import, and removing the
stale completed queue item.

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
