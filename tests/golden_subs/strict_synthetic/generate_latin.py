#!/usr/bin/env python3
"""Render anonymous Latin subtitle regression fixtures with Pillow and DejaVu Sans.

Run from any directory. The checked-in PNGs are the test inputs; regenerating
requires Pillow and /usr/share/fonts/truetype/dejavu/DejaVuSans.ttf.
"""
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont

FONT = "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
FIXTURES = {
    "latin_french": "Je prépare le café.",
    "latin_spanish": "El corazón late.",
    "latin_german": "Bären mögen Honig.",
    "latin_french_evangile": "C'est l'Évangile.",
    "latin_french_qu_a": "Je crois qu'à midi.",
    "latin_spanish_inverted": "¡Vamos! ¿Qué pasa?",
    "latin_french_short_words": "Il y a de la lumière à Paris.",
}


def main():
    for name, text in FIXTURES.items():
        # At 32 px the capital accent is a separate four-pixel band, which the
        # PGS cropper conservatively treats as a second line before recognition.
        font = ImageFont.truetype(FONT, 24 if name == "latin_french_evangile" else 32)
        image = Image.new("L", (500, 80), 255)
        ImageDraw.Draw(image).text((20, 15), text, font=font, fill=0)
        image.save(Path(__file__).parent / f"{name}.png")


if __name__ == "__main__":
    main()
