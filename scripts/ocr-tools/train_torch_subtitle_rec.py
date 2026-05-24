#!/usr/bin/env python3
"""Train a small ONNX-compatible CTC subtitle recognizer on synthetic data.

This is a research/training utility for DPN subtitle OCR. It intentionally
exports a model with PaddleOCR-like CTC output shape `[N, T, C]` and ONNX
metadata key `character`, so DPN's existing PP-OCR recognizer wrapper can load
it via `DPN_OCR_REC_ENGLISH_MODEL` if the character order matches.
"""

from __future__ import annotations

import argparse
import math
import random
from pathlib import Path

import onnx
import torch
import torch.nn as nn
import torch.nn.functional as F
from PIL import Image
from torch.utils.data import DataLoader, Dataset

# Match PaddleOCR-style dictionary metadata: DPN/paddle-ocr-rs prepends blank
# and appends a space. Space is therefore represented by the last class.
CHARS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!\"#$%&'()*+,-./:;?@[]_{}…“”‘’"
SPACE_INDEX = len(CHARS) + 1
NUM_CLASSES = len(CHARS) + 2  # blank + chars + appended space


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--data-root", required=True, type=Path)
    p.add_argument("--output-dir", required=True, type=Path)
    p.add_argument("--epochs", type=int, default=4)
    p.add_argument("--batch-size", type=int, default=32)
    p.add_argument("--lr", type=float, default=1e-3)
    p.add_argument("--device", default="cuda" if torch.cuda.is_available() else "cpu")
    p.add_argument("--num-workers", type=int, default=2)
    p.add_argument("--max-width", type=int, default=640)
    p.add_argument("--seed", type=int, default=125)
    p.add_argument(
        "--init-checkpoint",
        type=Path,
        default=None,
        help="Optional best.pt checkpoint to initialize/fine-tune from.",
    )
    return p.parse_args()


def encode_label(text: str) -> list[int]:
    out: list[int] = []
    for ch in text:
        if ch == " ":
            out.append(SPACE_INDEX)
        elif ch in CHARS:
            out.append(CHARS.index(ch) + 1)
        # Drop unsupported chars rather than corrupting labels.
    return out


class RecDataset(Dataset):
    def __init__(self, root: Path, manifest_name: str, max_width: int = 384):
        self.root = root
        self.max_width = max_width
        self.items: list[tuple[Path, str]] = []
        manifest = root / manifest_name
        for line in manifest.read_text(encoding="utf-8").splitlines():
            if not line.strip() or "\t" not in line:
                continue
            rel, label = line.split("\t", 1)
            encoded = encode_label(label)
            if encoded:
                self.items.append((root / rel, label))
        if not self.items:
            raise RuntimeError(f"no items loaded from {manifest}")

    def __len__(self) -> int:
        return len(self.items)

    def __getitem__(self, idx: int):
        path, label = self.items[idx]
        img = Image.open(path).convert("RGB")
        w, h = img.size
        new_h = 48
        new_w = max(8, int(math.ceil(w * (new_h / max(h, 1)))))
        new_w = min(new_w, self.max_width)
        img = img.resize((new_w, new_h), Image.Resampling.BILINEAR)
        x = torch.ByteTensor(torch.ByteStorage.from_buffer(img.tobytes()))
        x = x.view(new_h, new_w, 3).permute(2, 0, 1).float() / 255.0
        x = (x - 0.5) / 0.5
        y = torch.tensor(encode_label(label), dtype=torch.long)
        return x, y, label


def collate(batch):
    xs, ys, labels = zip(*batch)
    max_w = max(x.shape[-1] for x in xs)
    padded = []
    for x in xs:
        if x.shape[-1] < max_w:
            x = F.pad(x, (0, max_w - x.shape[-1], 0, 0), value=1.0)
        padded.append(x)
    targets = torch.cat(list(ys))
    target_lengths = torch.tensor([len(y) for y in ys], dtype=torch.long)
    return torch.stack(padded), targets, target_lengths, labels


class SubtitleCtcRecognizer(nn.Module):
    def __init__(self, classes: int = NUM_CLASSES):
        super().__init__()
        self.features = nn.Sequential(
            nn.Conv2d(3, 48, 3, padding=1), nn.BatchNorm2d(48), nn.ReLU(True), nn.MaxPool2d((2, 2)),
            nn.Conv2d(48, 96, 3, padding=1), nn.BatchNorm2d(96), nn.ReLU(True), nn.MaxPool2d((2, 2)),
            nn.Conv2d(96, 160, 3, padding=1), nn.BatchNorm2d(160), nn.ReLU(True), nn.MaxPool2d((2, 1)),
            nn.Conv2d(160, 224, 3, padding=1), nn.BatchNorm2d(224), nn.ReLU(True), nn.MaxPool2d((2, 1)),
            nn.Conv2d(224, 256, 3, padding=1), nn.BatchNorm2d(256), nn.ReLU(True),
        )
        self.proj = nn.Conv2d(256, classes, kernel_size=1)

    def forward(self, x):
        x = self.features(x)
        x = x.mean(dim=2, keepdim=True)
        x = self.proj(x).squeeze(2).permute(0, 2, 1)
        return torch.softmax(x, dim=-1)


def greedy_decode(probs: torch.Tensor) -> str:
    ids = probs.argmax(-1).tolist()
    out = []
    last = 0
    for idx in ids:
        if idx != 0 and idx != last:
            if idx == SPACE_INDEX:
                out.append(" ")
            elif 1 <= idx <= len(CHARS):
                out.append(CHARS[idx - 1])
        last = idx
    return "".join(out).strip()


def cer(pred: str, truth: str) -> float:
    a, b = pred, truth
    dp = list(range(len(b) + 1))
    for i, ca in enumerate(a, 1):
        ndp = [i] + [0] * len(b)
        for j, cb in enumerate(b, 1):
            ndp[j] = min(dp[j] + 1, ndp[j - 1] + 1, dp[j - 1] + (ca != cb))
        dp = ndp
    return dp[-1] / max(len(b), 1)


def run_epoch(model, loader, optimizer, device: str, train: bool):
    model.train(train)
    total_loss = 0.0
    total_cer = 0.0
    n = 0
    ctc = nn.CTCLoss(blank=0, zero_infinity=True)
    for xs, targets, target_lengths, labels in loader:
        xs = xs.to(device)
        targets = targets.to(device)
        target_lengths = target_lengths.to(device)
        probs = model(xs)
        log_probs = probs.clamp_min(1e-8).log().permute(1, 0, 2)
        input_lengths = torch.full((xs.shape[0],), probs.shape[1], dtype=torch.long, device=device)
        loss = ctc(log_probs, targets, input_lengths, target_lengths)
        if train:
            optimizer.zero_grad(set_to_none=True)
            loss.backward()
            torch.nn.utils.clip_grad_norm_(model.parameters(), 5.0)
            optimizer.step()
        with torch.no_grad():
            decoded = [greedy_decode(p.cpu()) for p in probs]
            batch_cer = sum(cer(p, t) for p, t in zip(decoded, labels)) / len(labels)
        total_loss += float(loss.detach()) * xs.shape[0]
        total_cer += batch_cer * xs.shape[0]
        n += xs.shape[0]
    return total_loss / max(n, 1), total_cer / max(n, 1)


def export_onnx(model: nn.Module, path: Path, device: str):
    model.eval()
    dummy = torch.randn(1, 3, 48, 320, device=device)
    torch.onnx.export(
        model,
        dummy,
        path,
        input_names=["x"],
        output_names=["softmax_0.tmp_0"],
        dynamic_axes={"x": {0: "batch", 3: "width"}, "softmax_0.tmp_0": {0: "batch", 1: "time"}},
        opset_version=14,
    )
    onnx_model = onnx.load(path)
    onnx_model.ir_version = min(onnx_model.ir_version, 9)
    meta = onnx_model.metadata_props.add()
    meta.key = "character"
    meta.value = "\n".join(CHARS)
    onnx.save(onnx_model, path)


def main() -> None:
    args = parse_args()
    random.seed(args.seed)
    torch.manual_seed(args.seed)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    train_ds = RecDataset(args.data_root, "train.txt", args.max_width)
    val_ds = RecDataset(args.data_root, "val.txt", args.max_width)
    train_loader = DataLoader(train_ds, batch_size=args.batch_size, shuffle=True, num_workers=args.num_workers, collate_fn=collate)
    val_loader = DataLoader(val_ds, batch_size=args.batch_size, shuffle=False, num_workers=args.num_workers, collate_fn=collate)
    device = args.device if args.device == "cpu" or torch.cuda.is_available() else "cpu"
    model = SubtitleCtcRecognizer().to(device)
    if args.init_checkpoint is not None:
        checkpoint = torch.load(args.init_checkpoint, map_location=device)
        state = checkpoint.get("model", checkpoint)
        model.load_state_dict(state)
        print(f"loaded_checkpoint={args.init_checkpoint}", flush=True)
    optimizer = torch.optim.AdamW(model.parameters(), lr=args.lr, weight_decay=1e-4)
    best_cer = float("inf")
    for epoch in range(1, args.epochs + 1):
        train_loss, train_cer = run_epoch(model, train_loader, optimizer, device, True)
        val_loss, val_cer = run_epoch(model, val_loader, optimizer, device, False)
        print(f"epoch={epoch} train_loss={train_loss:.4f} train_cer={train_cer:.4f} val_loss={val_loss:.4f} val_cer={val_cer:.4f}", flush=True)
        if val_cer < best_cer:
            best_cer = val_cer
            torch.save({"model": model.state_dict(), "chars": CHARS, "space_index": SPACE_INDEX}, args.output_dir / "best.pt")
            export_onnx(model, args.output_dir / "subtitle_ctc_rec.onnx", device)
    print(f"best_cer={best_cer:.4f} onnx={args.output_dir / 'subtitle_ctc_rec.onnx'}")


if __name__ == "__main__":
    main()
