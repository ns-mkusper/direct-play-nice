# Plex Refresh

To avoid the "Plex dance" (manual library rescans after conversion),
`direct_play_nice` can trigger a targeted Plex refresh automatically.

## CLI options

- `--plex-refresh`
- `--plex-url <PLEX_URL>` (default: `http://127.0.0.1:32400`)
- `--plex-token <PLEX_TOKEN>`

Environment variable equivalents:

- `DIRECT_PLAY_NICE_PLEX_REFRESH=true`
- `DIRECT_PLAY_NICE_PLEX_URL=http://127.0.0.1:32400`
- `DIRECT_PLAY_NICE_PLEX_TOKEN=...` (or `PLEX_TOKEN`)

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
```

Need a token? See Plex support:
<https://support.plex.tv/articles/204059436-finding-an-authentication-token-x-plex-token/>
