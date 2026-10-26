# Plex Refresh

To avoid the "Plex dance" (manual library rescans after conversion),
`direct_play_nice` can trigger a targeted Plex refresh automatically.

## CLI options

- `--plex-refresh`
- `--plex-url <PLEX_URL>` (default: `http://127.0.0.1:32400`)
- `--plex-token <PLEX_TOKEN>`
- `--plex-remove-replaced-versions [BOOL]` (default: `true`)

Environment variable equivalents:

- `DIRECT_PLAY_NICE_PLEX_REFRESH=true`
- `DIRECT_PLAY_NICE_PLEX_URL=http://127.0.0.1:32400`
- `DIRECT_PLAY_NICE_PLEX_TOKEN=...` (or `PLEX_TOKEN`)
- `DIRECT_PLAY_NICE_PLEX_REMOVE_REPLACED_VERSIONS=false`

Example:

```bash
direct_play_nice \
  --plex-refresh \
  --plex-url http://127.0.0.1:32400 \
  --plex-token "$PLEX_TOKEN" \
  input.mkv output.mp4
```

## Config file equivalent

```toml
[plex]
refresh = true
url = "http://127.0.0.1:32400"
token = "YOUR_TOKEN"
remove_replaced_versions = true
```

## Replaced media versions

When a conversion replaces a file (for example `Episode.mkv` becomes
`Episode.fixed.mp4` and the original is removed), Plex keeps the original as a
second media version marked deleted unless "Empty trash automatically after
every scan" is enabled. Clients can pick that version and report the item as
unavailable.

With `remove_replaced_versions` enabled (the default), `direct_play_nice`
waits for the refresh to finish (up to three minutes), finds the item through
the section's folder view, and deletes the media version that points at the
replaced file with `DELETE /library/metadata/{id}/media/{mediaId}`. A version
is removed only when:

- one of its parts is the file that was replaced,
- none of its files exist on disk, and
- the item still has another version that is not marked deleted and whose
  files exist.

Whole items are never deleted and the section trash is never emptied. Any
problem during this step is logged as a warning and does not fail the
conversion. Deleting media versions requires the server setting "Allow media
deletion" (enabled by default).

Need a token? See Plex support:
<https://support.plex.tv/articles/204059436-finding-an-authentication-token-x-plex-token/>
