#!/usr/bin/env bash
set -euo pipefail

tag="${1:?usage: extract_changelog_release_notes.sh <tag> <output-file>}"
out_file="${2:?usage: extract_changelog_release_notes.sh <tag> <output-file>}"
version="${tag#v}"
tmp_file="$(mktemp)"
trimmed_file="$(mktemp)"
trap 'rm -f "$tmp_file" "$trimmed_file"' EXIT

awk -v version="$version" '
  BEGIN { in_section = 0; found = 0 }
  $0 ~ "^## \\[" version "\\]" {
    in_section = 1
    found = 1
    next
  }
  in_section && $0 ~ "^## \\[" {
    exit
  }
  in_section {
    print
  }
  END {
    if (!found) {
      exit 42
    }
  }
' CHANGELOG.md > "$tmp_file"

awk '
  NF { seen = 1 }
  seen { lines[++n] = $0 }
  END {
    while (n > 0 && lines[n] == "") {
      n--
    }
    for (i = 1; i <= n; i++) {
      print lines[i]
    }
  }
' "$tmp_file" > "$trimmed_file"

if [[ ! -s "$trimmed_file" ]]; then
  echo "No changelog notes found for $tag" >&2
  exit 1
fi

if grep -Eq 'No changes yet|📋 Pending' "$trimmed_file"; then
  echo "Refusing to use placeholder changelog notes for $tag" >&2
  exit 1
fi

if ! grep -Eq '^- ' "$trimmed_file"; then
  echo "No release-note bullets found for $tag" >&2
  exit 1
fi

cp "$trimmed_file" "$out_file"
