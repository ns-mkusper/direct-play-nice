#!/usr/bin/env python3
"""Build a stable fingerprint for the repository's vcpkg inputs."""

from __future__ import annotations

import argparse
import hashlib
import json
import subprocess
from pathlib import Path
from typing import Any

import tomllib


COMPANION_FILES = (
    "vcpkg.json",
    "vcpkg-configuration.json",
    "VCPKG_DEPS_LIST.txt",
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Emit a stable hash for the vcpkg inputs used by CI."
    )
    parser.add_argument(
        "--repo-root",
        default=".",
        help="Repository root used to resolve Cargo.toml and optional companion files.",
    )
    parser.add_argument(
        "--cargo-toml",
        default="Cargo.toml",
        help="Cargo.toml path relative to --repo-root.",
    )
    parser.add_argument(
        "--target",
        help="Optional target triple whose vcpkg target metadata should be included.",
    )
    parser.add_argument(
        "--git-treeish",
        help="Optional git tree-ish used to read files from the repository instead of disk.",
    )
    parser.add_argument(
        "--print-payload",
        action="store_true",
        help="Print the normalized payload instead of the hash.",
    )
    return parser.parse_args()


def read_text(repo_root: Path, rel_path: str, treeish: str | None, required: bool) -> str | None:
    if treeish is None:
        full_path = repo_root / rel_path
        if not full_path.exists():
            if required:
                raise FileNotFoundError(full_path)
            return None
        return full_path.read_text(encoding="utf-8")

    result = subprocess.run(
        ["git", "-C", str(repo_root), "show", f"{treeish}:{rel_path}"],
        check=False,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        if required:
            raise RuntimeError(
                f"unable to read '{rel_path}' from git tree '{treeish}': {result.stderr.strip()}"
            )
        return None
    return result.stdout


def normalize(value: Any) -> Any:
    if isinstance(value, dict):
        return {key: normalize(value[key]) for key in sorted(value)}
    if isinstance(value, list):
        normalized = [normalize(item) for item in value]
        if all(not isinstance(item, (dict, list)) for item in normalized):
            return sorted(normalized, key=lambda item: json.dumps(item, sort_keys=True))
        return normalized
    return value


def build_payload(
    repo_root: Path,
    cargo_toml: str,
    target: str | None,
    treeish: str | None,
) -> dict[str, Any]:
    cargo_doc = tomllib.loads(read_text(repo_root, cargo_toml, treeish, required=True) or "")
    vcpkg_meta = cargo_doc.get("package", {}).get("metadata", {}).get("vcpkg", {})
    payload: dict[str, Any] = {
        "cargo_vcpkg": {
            "git": vcpkg_meta.get("git"),
            "rev": vcpkg_meta.get("rev"),
            "dependencies": vcpkg_meta.get("dependencies", []),
        }
    }

    targets = vcpkg_meta.get("target", {})
    if target:
        payload["cargo_vcpkg"]["target"] = {target: targets.get(target, {})}

    companion_payload: dict[str, Any] = {}
    for companion in COMPANION_FILES:
        text = read_text(repo_root, companion, treeish, required=False)
        if text is None:
            continue
        if companion.endswith(".json"):
            companion_payload[companion] = json.loads(text)
        else:
            companion_payload[companion] = [
                line.rstrip()
                for line in text.splitlines()
                if line.strip() and not line.lstrip().startswith("#")
            ]

    if companion_payload:
        payload["companion_files"] = companion_payload

    return normalize(payload)


def main() -> int:
    args = parse_args()
    repo_root = Path(args.repo_root).resolve()
    payload = build_payload(repo_root, args.cargo_toml, args.target, args.git_treeish)

    if args.print_payload:
        print(json.dumps(payload, indent=2, sort_keys=True))
        return 0

    encoded = json.dumps(payload, sort_keys=True, separators=(",", ":")).encode("utf-8")
    print(hashlib.sha256(encoded).hexdigest())
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
