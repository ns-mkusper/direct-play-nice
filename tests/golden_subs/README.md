# Golden Subtitle OCR Fixtures

Golden subtitle OCR fixtures live here.

Each fixture should include:

- `name.png` (bitmap subtitle image)
- `name.json` (expected OCR metadata)

Example JSON schema:

```json
{
  "expected_text": "HELLO WORLD",
  "expected_bbox": [100, 800, 400, 860],
  "expected_color_rgb": [255, 255, 0],
  "is_italic": false
}
```

Enable the golden suite with `DPN_OCR_GOLDEN=1 cargo test`.

## Multilingual Fixtures

`tests/golden_subs/multilang` contains pre-rendered subtitle bitmap fixtures for
English, Spanish, French, Korean, and Japanese.

Each multilingual fixture includes:

- `name.png` (rendered subtitle bitmap image)
- `name.json` with schema:

```json
{
  "language": "eng",
  "expected_text": "WE SHOULD LEAVE NOW",
  "min_similarity": 0.9,
  "max_infer_ms": 2500
}
```

Run with:

```bash
DPN_OCR_MULTILANG_FIXTURES=1 cargo test \
  subtitle_ocr::tests::test_multilang_prerendered_fixture_accuracy_and_performance \
  -- --nocapture
```
