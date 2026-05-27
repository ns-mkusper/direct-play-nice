#!/usr/bin/env python3
"""Generate synthetic English bitmap-subtitle recognition data for PaddleOCR.

The output format is PaddleOCR recognition training format:

    relative/path.png<TAB>label

The renderer intentionally creates low-resolution, outlined, antialiased subtitle
line crops so a PP-OCR recognizer can be fine-tuned to preserve spaces on DVD/PGS
style subtitle bitmaps.
"""

from __future__ import annotations

import argparse
import random
import string
from pathlib import Path
from typing import Iterable

from PIL import Image, ImageDraw, ImageFilter, ImageFont

DEFAULT_FONTS = [
    "/usr/share/fonts/TTF/DejaVuSans-Bold.ttf",
    "/usr/share/fonts/TTF/DejaVuSans.ttf",
    "/usr/share/fonts/TTF/DejaVuSansCondensed.ttf",
    "/usr/share/fonts/TTF/DejaVuSansCondensed-Bold.ttf",
    "/usr/share/fonts/TTF/LiberationSans-Regular.ttf",
    "/usr/share/fonts/TTF/LiberationSans-Bold.ttf",
    "/usr/share/fonts/cantarell/Cantarell-VF.otf",
]

NAMES = [
    "Alice",
    "Boris",
    "Mira",
    "Tobias",
    "Lena",
    "Hugo",
    "Nora",
    "Iris",
    "Kai",
    "Juno",
    "Rin",
    "Milo",
]

NOUNS = [
    "city",
    "town",
    "road",
    "flower",
    "moon",
    "spirit",
    "machine",
    "mountain",
    "river",
    "gate",
    "lab",
    "store",
    "bar",
    "train",
    "ship",
    "forest",
    "desert",
    "storm",
]

VERBS = [
    "find",
    "leave",
    "watch",
    "follow",
    "believe",
    "remember",
    "forget",
    "protect",
    "hide",
    "wait",
    "fight",
    "listen",
    "return",
    "escape",
]

TEMPLATES = [
    "Where are you?",
    "Who are you?",
    "What are you doing here?",
    "I don't know what you mean.",
    "I haven't seen one in a long time.",
    "There's always a little truth in them.",
    "It's been over {num} years since they left.",
    "Why did you get caught, then?",
    "Let's meet again in {place}.",
    "If we meet again, I'll tell you everything.",
    "We were going to {place}.",
    "The {thing} was born from the great spirit.",
    "The scent of {thing}s was coming from this city.",
    "I was looking for cleaning supplies.",
    "You don't know what it takes to survive in this city.",
    "Maybe it would be okay if I saw her for just a bit.",
    "That's quite a piece you're packing there.",
    "Can I go out and look for it for a little while?",
    "You're just lying to yourself.",
    "We couldn't let you get away scot-free.",
    "Stop it! You don't have the right to shoot him!",
    "I thought {name} might want to come, too.",
    "This is why you and I would never work out.",
    "There has got to be something I can do.",
    "How low will you people sink?",
    "I had a dream where the {thing}s bloomed.",
    "As far as the eye could see, there was nothing.",
    "All I smell is the stench of the old days.",
    "These are good places to hide from the guards.",
    "In the city to the north, people still remember.",
    "Sit back and watch what happens next.",
    "We found {place} under a full moon.",
]

VALIDATION_LINES = [
    "Let us meet in paradise.",
    "If we meet again, that is.",
    "The book of the moon was never finished.",
    "But pride doesn't count for much now.",
    "Alice herself is the crowning achievement.",
    "It must have made you angry.",
    "She was pissing her pants.",
    "It was something serious.",
    "Were created from wolves.",
    "After a night in that stinkhole.",
    "On a raid at this hour?",
    "In the city to the north.",
    "To sneak in as far as the lab.",
    "As far as the eye could see.",
    "From a brain like yours.",
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-root", required=True, type=Path)
    parser.add_argument("--train-count", type=int, default=50000)
    parser.add_argument("--val-count", type=int, default=2000)
    parser.add_argument("--seed", type=int, default=125)
    parser.add_argument("--font", action="append", default=[])
    parser.add_argument("--max-width", type=int, default=640)
    parser.add_argument("--max-height", type=int, default=80)
    return parser.parse_args()


def available_fonts(extra: Iterable[str]) -> list[Path]:
    candidates = [Path(x) for x in extra] + [Path(x) for x in DEFAULT_FONTS]
    candidates += sorted(Path("/usr/share/fonts").glob("**/*Sans*.ttf"))[:40]
    fonts = []
    for path in candidates:
        if not path.is_file() or path in fonts:
            continue
        try:
            ImageFont.truetype(str(path), size=20)
        except OSError:
            continue
        fonts.append(path)
    if not fonts:
        raise SystemExit("No usable fonts found; pass --font /path/to/font.ttf")
    return fonts


def make_label(rng: random.Random) -> str:
    template = rng.choice(TEMPLATES)
    place = rng.choice(["Paradise", "the city", "the north", "the old road", "that town"])
    thing = rng.choice(NOUNS)
    label = template.format(
        name=rng.choice(NAMES),
        place=place,
        thing=thing,
        verb=rng.choice(VERBS),
        num=rng.choice([10, 20, 50, 100, 200]),
    )
    if rng.random() < 0.12:
        label = f"{rng.choice(NAMES)}, {label[0].lower()}{label[1:]}"
    if rng.random() < 0.08:
        label = label.replace(".", "...")
    return label


def fit_font(label: str, font_path: Path, max_width: int, rng: random.Random) -> ImageFont.FreeTypeFont:
    for size in range(rng.randint(30, 38), 13, -1):
        font = ImageFont.truetype(str(font_path), size=size)
        bbox = ImageDraw.Draw(Image.new("L", (1, 1))).textbbox((0, 0), label, font=font, stroke_width=2)
        if bbox[2] - bbox[0] <= max_width - 10:
            return font
    return ImageFont.truetype(str(font_path), size=14)


def render_label(label: str, font_path: Path, max_width: int, max_height: int, rng: random.Random) -> Image.Image:
    font = fit_font(label, font_path, max_width, rng)
    stroke = rng.choice([1, 2, 2, 3])
    draw0 = ImageDraw.Draw(Image.new("L", (1, 1)))
    bbox = draw0.textbbox((0, 0), label, font=font, stroke_width=stroke)
    text_w = bbox[2] - bbox[0]
    text_h = bbox[3] - bbox[1]
    pad_x = rng.randint(3, 12)
    pad_y = rng.randint(2, 8)
    width = min(max_width, text_w + 2 * pad_x)
    height = min(max_height, text_h + 2 * pad_y)
    image = Image.new("RGBA", (width, height), (0, 0, 0, 0))
    draw = ImageDraw.Draw(image)
    x = max(0, (width - text_w) // 2 - bbox[0])
    y = max(0, (height - text_h) // 2 - bbox[1])
    outline_alpha = rng.randint(190, 255)
    fill_alpha = rng.randint(210, 255)
    draw.text(
        (x, y),
        label,
        font=font,
        fill=(255, 255, 255, fill_alpha),
        stroke_width=stroke,
        stroke_fill=(0, 0, 0, outline_alpha),
    )
    if rng.random() < 0.35:
        image = image.filter(ImageFilter.GaussianBlur(radius=rng.uniform(0.15, 0.45)))
    bg = Image.new("RGB", image.size, (255, 255, 255))
    alpha = image.getchannel("A")
    # Simulate subtitle OCR PGM polarity: dark glyphs on light background.
    luma = Image.new("L", image.size, 255)
    dark = Image.new("L", image.size, 0)
    luma.paste(dark, mask=alpha)
    if rng.random() < 0.45:
        # Quantize to mimic bitmap palettes and DVD-ish edges.
        luma = luma.quantize(colors=rng.choice([4, 8, 16])).convert("L")
    scale = rng.choice([1, 1, 2])
    if scale > 1:
        luma = luma.resize((luma.width * scale, luma.height * scale), Image.Resampling.NEAREST)
    return luma.convert("RGB")


def write_split(root: Path, split: str, labels: list[str], fonts: list[Path], args: argparse.Namespace, rng: random.Random) -> None:
    img_dir = root / split
    img_dir.mkdir(parents=True, exist_ok=True)
    manifest = root / f"{split}.txt"
    with manifest.open("w", encoding="utf-8") as f:
        for idx, label in enumerate(labels):
            font = rng.choice(fonts)
            image = render_label(label, font, args.max_width, args.max_height, rng)
            rel = Path(split) / f"{idx:08d}.png"
            image.save(root / rel)
            f.write(f"{rel.as_posix()}\t{label}\n")


def main() -> None:
    args = parse_args()
    rng = random.Random(args.seed)
    root = args.output_root
    root.mkdir(parents=True, exist_ok=True)
    fonts = available_fonts(args.font)
    train = [make_label(rng) for _ in range(args.train_count)]
    val = list(VALIDATION_LINES)
    while len(val) < args.val_count:
        val.append(make_label(rng))
    write_split(root, "train", train, fonts, args, rng)
    write_split(root, "val", val[: args.val_count], fonts, args, rng)
    (root / "fonts.txt").write_text("\n".join(str(x) for x in fonts) + "\n", encoding="utf-8")
    print(f"wrote {len(train)} train and {min(len(val), args.val_count)} val examples to {root}")


if __name__ == "__main__":
    main()
