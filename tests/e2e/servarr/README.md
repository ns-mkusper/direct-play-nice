# Servarr kind end-to-end tests

This suite runs real Sonarr and Radarr processes in a disposable kind cluster.
It imports synthetic media through their HTTP APIs, rather than setting Arr
variables and launching DPN from the test driver. Arr invokes the Custom Script
wrapper, which records the actual event and runs the PR-built DPN executable.

## Run

Install Docker, kind v0.31.0, kubectl, and Python 3, then run from the repo:

```bash
python3 -m unittest discover -s tests/e2e/servarr -p 'test_*.py' -v
bash tests/e2e/servarr/run.sh
```

A containerized development shell using its host's Docker socket can set
`E2E_DOCKER_HOST_GATEWAY=host.docker.internal` when kind's published loopback
port is not reachable directly. TLS certificate verification remains enabled.

The runner creates a unique cluster and private kubeconfig, and deletes the
cluster on exit. It never uses an existing Kubernetes context, host media mount,
Plex token, download client, indexer, or GPU. Do not point this suite at a live
Arr installation. Configuration and synthetic media use separate `emptyDir`
volumes; the library and downloads share `/data` so hardlinks are possible.
Arr and its hook run as UID/GID 1000 using the upstream image's service setup.

## Images and builds

Neither Arr project publishes an official Docker image. `images.env` pins the
multi-architecture digests of maintained hotio release images listed by the
Servarr installation docs. The test images retain their real application and
init system and add the current checkout's DPN binary, FFmpeg, and Python.
DPN is built natively against Alpine's musl and FFmpeg 8, matching the runtime
rather than copying a glibc executable into an incompatible container.

The build follows the repository's untracked `Cargo.lock` convention. Each run
records its resolved lockfile in the debug artifacts. Image updates should be
reviewed together with metadata seed compatibility and both apps' E2E results.

## Bootstrap boundary

The real Add Series/Movie APIs fetch metadata from public services even when
metadata is supplied. To avoid that dependency, the suite starts Arr to migrate
an empty database, stops the app, and seeds only one synthetic series/episode
or movie's metadata. The seeder refuses running apps and nonempty libraries.
No media-file, history, notification, or command records are manufactured.

After restart, configuration, Custom Script registration, manual imports,
upgrades, and rescans go through the real Arr API. This tests the import-to-hook
boundary; it does not test indexer searches, metadata providers, or downloading.
Synthetic four-second videos have distinct colors so decoded-content checks
can distinguish the upgrade from the previous version.

## Assertions

For both apps, the suite exercises initial import, upgrade, upgrade with the
hook disabled, and upgrade after reenabling it. Successful processing must
produce a decodable H.264 MP4, remove the replaced library source, and leave Arr
tracking one valid file after rescanning. The results record stale tracking
immediately after the hook and issue an explicit API rescan; this is not a claim
that DPN automatically updates Arr's file association. It checks the real
`Download` event, upgrade flag, non-root ownership, and hardlink inode/content
behavior: replacing
the library hardlink must not alter or delete the download copy.

Failure controls run DPN with an invalid config or return a nonzero hook status;
the imported source must remain available. These controls test hook failure
handling, not mid-encode corruption or atomic promotion rollback, which remain
covered by the focused CLI regressions in `tests/cli_servarr.rs`.
Repeated rescans must not duplicate files or trigger repeated conversion.

## CI and diagnostics

`.github/workflows/servarr-e2e.yml` runs on pull requests, main pushes, and manual
dispatch on an isolated GitHub-hosted Linux runner. The driver uses bounded
polling of actual command and hook completion, not fixed sleeps as a substitute
for readiness. Failure still collects app logs, hook events and output, API
transcripts, result JSON, and dependency versions under
`artifacts/servarr-e2e`. The private kubeconfig and Arr credential file are not
uploaded. Set `E2E_ARTIFACTS` to change the artifact directory.
