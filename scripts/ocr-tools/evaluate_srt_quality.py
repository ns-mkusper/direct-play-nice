#!/usr/bin/env python3
"""Evaluate OCR subtitle text against a reference SRT.

Reports cue counts, CER, WER, similarities, spacing/long-token metrics, and a
small set of worst aligned line pairs. The comparison is text-focused: timing is
ignored except for parsing cues.
"""

from __future__ import annotations

import argparse
import json
import re
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Iterable

TIMESTAMP_RE = re.compile(r"\d{2}:\d{2}:\d{2},\d{3}\s+-->\s+\d{2}:\d{2}:\d{2},\d{3}")
INDEX_RE = re.compile(r"^\d+$")
WORD_RE = re.compile(r"[\w']+|[^\w\s]", re.UNICODE)
SPACE_RE = re.compile(r"\s+")


@dataclass
class SrtText:
    cues: list[str]
    lines: list[str]
    text: str
    words: list[str]
    chars: list[str]


@dataclass
class TextMetrics:
    reference_cues: int
    hypothesis_cues: int
    reference_words: int
    hypothesis_words: int
    reference_chars: int
    hypothesis_chars: int
    wer: float
    cer: float
    word_similarity: float
    char_similarity: float
    hypothesis_space_rate: float | None
    hypothesis_long_token_count: int
    hypothesis_max_token_len: int
    worst_lines: list[dict[str, object]]


def parse_srt(path: Path) -> SrtText:
    cues: list[str] = []
    current: list[str] = []
    for raw in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        line = raw.strip()
        if not line:
            if current:
                cues.append(" ".join(current))
                current = []
            continue
        if INDEX_RE.match(line) or TIMESTAMP_RE.search(line):
            continue
        current.append(line)
    if current:
        cues.append(" ".join(current))
    lines = [line for cue in cues for line in cue.splitlines() if line.strip()]
    text = normalize_text(" ".join(cues))
    return SrtText(
        cues=cues,
        lines=[normalize_text(line) for line in lines],
        text=text,
        words=tokenize_words(text),
        chars=list(text),
    )


def normalize_text(text: str) -> str:
    text = text.replace("…", "...")
    text = text.replace("“", '"').replace("”", '"')
    text = text.replace("‘", "'").replace("’", "'")
    return SPACE_RE.sub(" ", text).strip()


def tokenize_words(text: str) -> list[str]:
    return [tok.lower() for tok in WORD_RE.findall(text)]


def levenshtein(a: list[str], b: list[str]) -> int:
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


def error_rate(ref: list[str], hyp: list[str]) -> float:
    if not ref:
        return 0.0 if not hyp else 1.0
    return levenshtein(ref, hyp) / len(ref)


def similarity(ref: list[str], hyp: list[str]) -> float:
    denom = max(len(ref), len(hyp), 1)
    return max(0.0, 1.0 - levenshtein(ref, hyp) / denom)


def line_similarity(a: str, b: str) -> float:
    return similarity(tokenize_words(a), tokenize_words(b))


def worst_line_pairs(ref: SrtText, hyp: SrtText, limit: int) -> list[dict[str, object]]:
    pairs = []
    for idx, (r, h) in enumerate(zip(ref.cues, hyp.cues), start=1):
        score = line_similarity(normalize_text(r), normalize_text(h))
        pairs.append({"index": idx, "similarity": score, "reference": r, "hypothesis": h})
    pairs.sort(key=lambda item: item["similarity"])
    return pairs[:limit]


def hypothesis_spacing_metrics(text: str) -> tuple[float | None, int, int]:
    chars = len(text)
    space_rate = (text.count(" ") / chars) if chars else None
    tokens = text.split()
    long_count = sum(1 for tok in tokens if len(tok) >= 14)
    max_len = max((len(tok) for tok in tokens), default=0)
    return space_rate, long_count, max_len


def evaluate(reference: Path, hypothesis: Path, worst_lines: int) -> TextMetrics:
    ref = parse_srt(reference)
    hyp = parse_srt(hypothesis)
    space_rate, long_count, max_len = hypothesis_spacing_metrics(hyp.text)
    wer = error_rate(ref.words, hyp.words)
    cer = error_rate(ref.chars, hyp.chars)
    return TextMetrics(
        reference_cues=len(ref.cues),
        hypothesis_cues=len(hyp.cues),
        reference_words=len(ref.words),
        hypothesis_words=len(hyp.words),
        reference_chars=len(ref.chars),
        hypothesis_chars=len(hyp.chars),
        wer=wer,
        cer=cer,
        word_similarity=1.0 - min(wer, 1.0),
        char_similarity=1.0 - min(cer, 1.0),
        hypothesis_space_rate=space_rate,
        hypothesis_long_token_count=long_count,
        hypothesis_max_token_len=max_len,
        worst_lines=worst_line_pairs(ref, hyp, worst_lines),
    )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("reference", type=Path)
    parser.add_argument("hypothesis", type=Path)
    parser.add_argument("--json-out", type=Path)
    parser.add_argument("--max-cer", type=float)
    parser.add_argument("--max-wer", type=float)
    parser.add_argument("--worst-lines", type=int, default=8)
    args = parser.parse_args()

    metrics = evaluate(args.reference, args.hypothesis, args.worst_lines)
    payload = asdict(metrics)
    if args.json_out:
        args.json_out.parent.mkdir(parents=True, exist_ok=True)
        args.json_out.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n")

    print(f"Reference cues: {metrics.reference_cues}")
    print(f"Hypothesis cues: {metrics.hypothesis_cues}")
    print(f"WER: {metrics.wer:.4f}")
    print(f"CER: {metrics.cer:.4f}")
    print(f"Word similarity: {metrics.word_similarity:.4f}")
    print(f"Char similarity: {metrics.char_similarity:.4f}")
    if metrics.hypothesis_space_rate is not None:
        print(f"Hypothesis space rate: {metrics.hypothesis_space_rate:.4f}")
    print(f"Hypothesis long tokens: {metrics.hypothesis_long_token_count}")
    print(f"Hypothesis max token len: {metrics.hypothesis_max_token_len}")
    if metrics.worst_lines:
        print("Worst cue pairs:")
        for item in metrics.worst_lines:
            print(f"- #{item['index']} sim={item['similarity']:.3f}")
            print(f"  ref: {item['reference']}")
            print(f"  hyp: {item['hypothesis']}")

    failed = False
    if args.max_cer is not None and metrics.cer > args.max_cer:
        print(f"CER gate failed: {metrics.cer:.4f} > {args.max_cer:.4f}")
        failed = True
    if args.max_wer is not None and metrics.wer > args.max_wer:
        print(f"WER gate failed: {metrics.wer:.4f} > {args.max_wer:.4f}")
        failed = True
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
