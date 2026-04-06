#!/usr/bin/env bash
set -euo pipefail

out_file="${1:-RELEASE_NOTES.md}"

if ! command -v git-cliff >/dev/null 2>&1; then
  echo "git-cliff is required to generate release notes" >&2
  exit 1
fi

latest_tag="$(git describe --tags --abbrev=0 2>/dev/null || true)"

if [[ -n "$latest_tag" ]]; then
  range="${latest_tag}..HEAD"
  git cliff "$range" --config cliff.toml --output "$out_file"
  echo "Generated release notes from ${latest_tag} to HEAD -> ${out_file}"
else
  git cliff --config cliff.toml --output "$out_file"
  echo "Generated release notes from repository start to HEAD -> ${out_file}"
fi
