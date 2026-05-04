# Release Process

Use this process when cutting a new source and binary release.

## Prepare the Release PR

1. Pick the next version.

   Prerelease versions use SemVer suffixes such as `0.1.0-beta.3`,
   `0.1.0-alpha.12`, or `0.1.0-rc.1`. GitHub and crates.io treat those as
   prereleases. Stable releases do not have a suffix, for example `0.1.0`.

2. Update `Cargo.toml`.

   ```toml
   version = "0.1.0-beta.3"
   ```

3. Add a concrete section to `CHANGELOG.md`.

   Do not leave release notes only under `[Unreleased]`. The binary release
   workflow extracts the section that matches the tag.

   ```md
   ## [0.1.0-beta.3] - 2026-04-29

   ### Highlights

   - Added or changed something user-visible.
   - Fixed something release-worthy.
   ```

4. Open a PR with the version bump and changelog entry.

   The release-readiness workflow checks whether release metadata will be
   updated on merge and verifies that `CHANGELOG.md` already contains concrete
   notes for the exact `Cargo.toml` version.

## Merge and Verify

1. Merge the release PR to `main`.

2. Wait for the merge pipelines to pass:

   - `Continuous Deployment`
   - `Benchmarks (Post-Merge)`

3. Confirm the release tag exists.

   ```bash
   git fetch --tags origin
   git rev-parse v0.1.0-beta.3
   ```

## Publish or Rerun Binaries

Use the `Release` workflow when binaries need to be built or repaired.

1. Open GitHub Actions.

2. Run the `Release` workflow manually.

3. Enter the exact tag, including the leading `v`.

   ```text
   v0.1.0-beta.3
   ```

4. Wait for all release jobs to pass.

   Expected platform archives:

   - `direct_play_nice-aarch64-apple-darwin.tar.xz`
   - `direct_play_nice-x86_64-apple-darwin.tar.xz`
   - `direct_play_nice-x86_64-unknown-linux-gnu.tar.xz`
   - `direct_play_nice-x86_64-pc-windows-msvc.zip`

   The workflow also publishes checksums, installers, `dist-manifest.json`, and
   the source archive.

## Verify the Published Release

Check the GitHub release:

```bash
gh release view v0.1.0-beta.3 --json tagName,isDraft,isPrerelease,publishedAt,assets
```

Verify:

- `isDraft` is `false`.
- `isPrerelease` matches the version suffix.
- The release body contains the matching `CHANGELOG.md` section.
- All expected binary archives and checksum files are present.

Check crates.io:

```bash
curl -sS https://crates.io/api/v1/crates/direct_play_nice/0.1.0-beta.3
```

## Notes

- Manual `Release` workflow reruns update an existing GitHub release and replace
  assets with the same names.
- The workflow resolves the GitHub release target from the requested tag, not
  from the branch used to dispatch the workflow.
- Release-workflow benchmarks are skipped for manual binary reruns. The
  post-merge benchmark pipeline remains the release gate.
