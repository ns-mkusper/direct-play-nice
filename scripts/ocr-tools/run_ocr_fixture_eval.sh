#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  run_ocr_fixture_eval.sh \
    --bin /path/to/direct_play_nice \
    --fixtures /path/to/fixture_dir \
    --run-dir /path/to/output_dir \
    [--ocr-engine pp-ocr-v3]

Writes:
  fixture_eval.json
  fixture_eval.md
EOF
}

BIN=""
FIXTURES=""
RUN_DIR=""
OCR_ENGINE="pp-ocr-v3"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --bin)
      BIN="$2"; shift 2 ;;
    --fixtures)
      FIXTURES="$2"; shift 2 ;;
    --run-dir)
      RUN_DIR="$2"; shift 2 ;;
    --ocr-engine)
      OCR_ENGINE="$2"; shift 2 ;;
    --help|-h)
      usage; exit 0 ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      exit 2 ;;
  esac
done

if [[ -z "$BIN" || -z "$FIXTURES" || -z "$RUN_DIR" ]]; then
  echo "Missing required arguments." >&2
  usage
  exit 2
fi
if [[ ! -x "$BIN" ]]; then
  echo "Binary is not executable: $BIN" >&2
  exit 2
fi
if [[ ! -d "$FIXTURES" ]]; then
  echo "Fixture directory not found: $FIXTURES" >&2
  exit 2
fi

mkdir -p "$RUN_DIR"
JSON_OUT="$RUN_DIR/fixture_eval.json"
MD_OUT="$RUN_DIR/fixture_eval.md"

"$BIN" \
  --probe-ocr-fixtures "$FIXTURES" \
  --ocr-engine "$OCR_ENGINE" \
  --output json >"$JSON_OUT"

"$BIN" \
  --probe-ocr-fixtures "$FIXTURES" \
  --ocr-engine "$OCR_ENGINE" \
  --output text >"$MD_OUT"

echo "Wrote:"
echo "  $JSON_OUT"
echo "  $MD_OUT"
