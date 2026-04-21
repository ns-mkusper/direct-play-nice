# Release Notes and Versioning

This repository uses `release-plz` and Conventional Commits.

## Automation

On merge to `main`, CI updates release metadata and runs release steps defined in:

- `.github/workflows/cd.yml`
- `release-plz.toml`

## Local maintainer checks

```bash
cargo check
cargo test
cargo doc --no-deps
```
