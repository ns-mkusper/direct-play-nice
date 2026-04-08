#!/usr/bin/env python3
"""Compare two SRT files using word-level Levenshtein similarity."""

from __future__ import annotations

import re
import sys
from typing import Iterable, List, Tuple

TIMESTAMP_RE = re.compile(r"\d{2}:\d{2}:\d{2},\d{3}\s+-->\s+\d{2}:\d{2}:\d{2},\d{3}")
INDEX_RE = re.compile(r"^\d+$")


def parse_srt(path: str) -> Tuple[List[str], int]:
    tokens: List[str] = []
    cue_count = 0
    with open(path, "r", encoding="utf-8", errors="ignore") as handle:
        for raw in handle:
            line = raw.strip()
            if not line:
                continue
            if INDEX_RE.match(line):
                continue
            if TIMESTAMP_RE.search(line):
                cue_count += 1
                continue
            tokens.extend(line.split())
    return tokens, cue_count


def levenshtein(a: List[str], b: List[str]) -> int:
    if a == b:
        return 0
    if not a:
        return len(b)
    if not b:
        return len(a)
    prev = list(range(len(b) + 1))
    for i, token_a in enumerate(a, start=1):
        curr = [i]
        for j, token_b in enumerate(b, start=1):
            cost = 0 if token_a == token_b else 1
            curr.append(min(prev[j] + 1, curr[j - 1] + 1, prev[j - 1] + cost))
        prev = curr
    return prev[-1]


def similarity(a: List[str], b: List[str]) -> float:
    if not a and not b:
        return 1.0
    denom = max(len(a), len(b))
    if denom == 0:
        return 1.0
    dist = levenshtein(a, b)
    return 1.0 - (dist / denom)


def main(argv: List[str]) -> int:
    if len(argv) != 3:
        print("Usage: compare_srt.py <reference.srt> <hypothesis.srt>")
        return 2
    ref_path, hyp_path = argv[1], argv[2]
    ref_tokens, ref_cues = parse_srt(ref_path)
    hyp_tokens, hyp_cues = parse_srt(hyp_path)
    score = similarity(ref_tokens, hyp_tokens)

    print(f"Reference cues: {ref_cues}")
    print(f"Hypothesis cues: {hyp_cues}")
    print(f"Reference tokens: {len(ref_tokens)}")
    print(f"Hypothesis tokens: {len(hyp_tokens)}")
    print(f"Word-level Levenshtein similarity: {score:.4f}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
