# Release Notes and Versioning

This repository uses `release-plz` and Conventional Commits.

## Automation

On merge to `main`, CI updates release metadata and runs release steps defined in:

- `.github/workflows/cd.yml`
- `release-plz.toml`

## Version bump rules

`release-plz` follows Conventional Commit semantics:

- `feat:` -> minor bump
- `fix:` -> patch bump
- `!` or `BREAKING CHANGE` footer -> major bump

## Local maintainer checks

```bash
cargo check
cargo test
cargo doc --no-deps
```
