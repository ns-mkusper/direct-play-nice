#!/usr/bin/env bash
set -euo pipefail

crate_version="$(awk -F'"' '/^version = / { print $2; exit }' Cargo.toml)"

if [[ -z "$crate_version" ]]; then
  echo "::error title=Release changelog check failed::Unable to parse crate version from Cargo.toml"
  exit 1
fi

notes_file="$(mktemp)"
trap 'rm -f "$notes_file"' EXIT

if ! bash scripts/extract_changelog_release_notes.sh "v${crate_version}" "$notes_file"; then
  echo "::error title=Release changelog check failed::CHANGELOG.md must contain concrete notes for v${crate_version} before release."
  echo "Add a section like: ## [${crate_version}] - YYYY-MM-DD"
  exit 1
fi

echo "Found concrete CHANGELOG.md notes for v${crate_version}."
