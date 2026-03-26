Golden subtitle OCR fixtures live here.

Each fixture should include:
- `name.png` (bitmap subtitle image)
- `name.json` (expected OCR metadata)

Example JSON schema:

```
{
  "expected_text": "HELLO WORLD",
  "expected_bbox": [100, 800, 400, 860],
  "expected_color_rgb": [255, 255, 0],
  "is_italic": false
}
```

Enable the golden suite with `DPN_OCR_GOLDEN=1 cargo test`.
