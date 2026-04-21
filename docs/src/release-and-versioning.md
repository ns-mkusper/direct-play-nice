# Release Notes and Versioning

This project uses `release-plz` plus Conventional Commits for version bumps and
automated publication flow.

On merges to `main`, CI updates release metadata and then performs release
actions configured in `.github/workflows/cd.yml` and `release-plz.toml`.

For maintainers, run local sanity checks before merge:

```bash
cargo check
cargo test
cargo doc --no-deps
```
