#!/usr/bin/env python3
"""Evaluate OCR subtitle text against a reference SRT.

Reports cue counts, CER, WER, similarities, spacing/long-token metrics, and a
small set of worst aligned cue pairs. Cues are aligned by timestamp overlap by
default so a missing/extra subtitle does not cascade into a misleading whole-file
sequence mismatch.
"""

from __future__ import annotations

import argparse
import json
import re
from dataclasses import asdict, dataclass
from pathlib import Path

TIMESTAMP_RE = re.compile(
    r"(?P<start>\d{2}:\d{2}:\d{2},\d{3})\s+-->\s+(?P<end>\d{2}:\d{2}:\d{2},\d{3})"
)
INDEX_RE = re.compile(r"^\d+$")
WORD_RE = re.compile(r"[\w']+|[^\w\s]", re.UNICODE)
SPACE_RE = re.compile(r"\s+")


@dataclass
class Cue:
    start_ms: int | None
    end_ms: int | None
    text: str


@dataclass
class SrtText:
    cues: list[str]
    timed_cues: list[Cue]
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
    aligned_by_time: bool
    worst_lines: list[dict[str, object]]


def timestamp_ms(value: str) -> int:
    hh, mm, rest = value.split(":")
    ss, ms = rest.split(",")
    return ((int(hh) * 60 + int(mm)) * 60 + int(ss)) * 1000 + int(ms)


def parse_srt(path: Path) -> SrtText:
    timed_cues: list[Cue] = []
    current: list[str] = []
    current_start: int | None = None
    current_end: int | None = None

    def flush() -> None:
        nonlocal current, current_start, current_end
        if current:
            timed_cues.append(Cue(current_start, current_end, " ".join(current)))
            current = []
            current_start = None
            current_end = None

    for raw in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        line = raw.strip()
        if not line:
            flush()
            continue
        if INDEX_RE.match(line):
            continue
        if m := TIMESTAMP_RE.search(line):
            flush()
            current_start = timestamp_ms(m.group("start"))
            current_end = timestamp_ms(m.group("end"))
            continue
        current.append(line)
    flush()

    cues = [cue.text for cue in timed_cues]
    lines = [line for cue in cues for line in cue.splitlines() if line.strip()]
    text = normalize_text(" ".join(cues))
    return SrtText(
        cues=cues,
        timed_cues=timed_cues,
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


def cue_overlap(a: Cue, b: Cue) -> int:
    if a.start_ms is None or a.end_ms is None or b.start_ms is None or b.end_ms is None:
        return 0
    return max(0, min(a.end_ms, b.end_ms) - max(a.start_ms, b.start_ms))


def cue_start_delta(a: Cue, b: Cue) -> int:
    if a.start_ms is None or b.start_ms is None:
        return 10**9
    return abs(a.start_ms - b.start_ms)


def align_by_time(ref: SrtText, hyp: SrtText) -> tuple[list[str], list[str], list[dict[str, object]]]:
    if not ref.timed_cues or not hyp.timed_cues:
        return ref.cues, hyp.cues, []
    aligned_ref: list[str] = []
    aligned_hyp: list[str] = []
    pairs: list[dict[str, object]] = []
    used: set[int] = set()
    for idx, ref_cue in enumerate(ref.timed_cues, start=1):
        best_idx: int | None = None
        best_key = (-1, -10**9)
        for hyp_idx, hyp_cue in enumerate(hyp.timed_cues):
            if hyp_idx in used:
                continue
            overlap = cue_overlap(ref_cue, hyp_cue)
            delta = cue_start_delta(ref_cue, hyp_cue)
            if overlap == 0 and delta > 350:
                continue
            key = (overlap, -delta)
            if key > best_key:
                best_key = key
                best_idx = hyp_idx
        hyp_text = ""
        if best_idx is not None:
            used.add(best_idx)
            hyp_text = hyp.timed_cues[best_idx].text
        aligned_ref.append(ref_cue.text)
        aligned_hyp.append(hyp_text)
        score = line_similarity(normalize_text(ref_cue.text), normalize_text(hyp_text))
        pairs.append(
            {
                "index": idx,
                "similarity": score,
                "reference": ref_cue.text,
                "hypothesis": hyp_text,
                "start_ms": ref_cue.start_ms,
            }
        )
    for hyp_idx, hyp_cue in enumerate(hyp.timed_cues):
        if hyp_idx not in used:
            aligned_ref.append("")
            aligned_hyp.append(hyp_cue.text)
            pairs.append(
                {
                    "index": None,
                    "similarity": 0.0,
                    "reference": "",
                    "hypothesis": hyp_cue.text,
                    "start_ms": hyp_cue.start_ms,
                }
            )
    return aligned_ref, aligned_hyp, pairs


def worst_line_pairs(
    ref: SrtText, hyp: SrtText, limit: int, use_time_alignment: bool
) -> list[dict[str, object]]:
    if use_time_alignment:
        _, _, pairs = align_by_time(ref, hyp)
    else:
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


def evaluate(reference: Path, hypothesis: Path, worst_lines: int, use_time_alignment: bool) -> TextMetrics:
    ref = parse_srt(reference)
    hyp = parse_srt(hypothesis)
    if use_time_alignment:
        ref_cues, hyp_cues, _ = align_by_time(ref, hyp)
        ref_text = normalize_text(" ".join(ref_cues))
        hyp_text = normalize_text(" ".join(hyp_cues))
        ref_words = tokenize_words(ref_text)
        hyp_words = tokenize_words(hyp_text)
        ref_chars = list(ref_text)
        hyp_chars = list(hyp_text)
    else:
        ref_words, hyp_words = ref.words, hyp.words
        ref_chars, hyp_chars = ref.chars, hyp.chars
    space_rate, long_count, max_len = hypothesis_spacing_metrics(hyp.text)
    wer = error_rate(ref_words, hyp_words)
    cer = error_rate(ref_chars, hyp_chars)
    return TextMetrics(
        reference_cues=len(ref.cues),
        hypothesis_cues=len(hyp.cues),
        reference_words=len(ref_words),
        hypothesis_words=len(hyp_words),
        reference_chars=len(ref_chars),
        hypothesis_chars=len(hyp_chars),
        wer=wer,
        cer=cer,
        word_similarity=1.0 - min(wer, 1.0),
        char_similarity=1.0 - min(cer, 1.0),
        hypothesis_space_rate=space_rate,
        hypothesis_long_token_count=long_count,
        hypothesis_max_token_len=max_len,
        aligned_by_time=use_time_alignment,
        worst_lines=worst_line_pairs(ref, hyp, worst_lines, use_time_alignment),
    )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("reference", type=Path)
    parser.add_argument("hypothesis", type=Path)
    parser.add_argument("--json-out", type=Path)
    parser.add_argument("--max-cer", type=float)
    parser.add_argument("--max-wer", type=float)
    parser.add_argument(
        "--strict",
        action="store_true",
        help="Use production OCR acceptance gates: CER <= 0.02 and WER <= 0.08.",
    )
    parser.add_argument(
        "--sequence-align",
        action="store_true",
        help="Compare cue sequence order instead of timestamp-overlap alignment.",
    )
    parser.add_argument("--worst-lines", type=int, default=8)
    args = parser.parse_args()

    if args.strict:
        args.max_cer = 0.02 if args.max_cer is None else args.max_cer
        args.max_wer = 0.08 if args.max_wer is None else args.max_wer
    metrics = evaluate(
        args.reference,
        args.hypothesis,
        args.worst_lines,
        use_time_alignment=not args.sequence_align,
    )
    payload = asdict(metrics)
    if args.json_out:
        args.json_out.parent.mkdir(parents=True, exist_ok=True)
        args.json_out.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n")

    print(f"Reference cues: {metrics.reference_cues}")
    print(f"Hypothesis cues: {metrics.hypothesis_cues}")
    print(f"Aligned by time: {metrics.aligned_by_time}")
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
