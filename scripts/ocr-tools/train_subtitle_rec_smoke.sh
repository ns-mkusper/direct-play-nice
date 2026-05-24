#!/usr/bin/env bash
set -euo pipefail

ROOT="${1:-/mnt/data2/benchmarks/direct_play_nice_ocr/training/synth_smoke}"
OUT="${2:-/mnt/data2/benchmarks/direct_play_nice_ocr/training/runs/smoke_$(date +%Y%m%d-%H%M%S)}"
IMAGE="${DPN_PADDLEOCR_TRAIN_IMAGE:-dpn-paddleocr-train:cu112}"
REPO="${DPN_REPO_DIR:-/mnt/data3/git/direct-play-nice}"

mkdir -p "$OUT"

docker run --rm --runtime=nvidia \
  -e NVIDIA_VISIBLE_DEVICES="${NVIDIA_VISIBLE_DEVICES:-all}" \
  -e CUDA_VISIBLE_DEVICES="${CUDA_VISIBLE_DEVICES:-0}" \
  -e LD_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu:/usr/local/cuda/lib64:/usr/local/cuda/compat \
  -v "$REPO:/work/direct-play-nice" \
  -v "$ROOT:/data" \
  -v "$OUT:/out" \
  "$IMAGE" bash -lc '
set -euo pipefail
python3 - <<"PY"
import paddle
print("paddle", paddle.__version__, "cuda", paddle.is_compiled_with_cuda(), "devices", paddle.device.cuda.device_count())
if paddle.device.cuda.device_count() > 0:
    paddle.set_device("gpu:0")
    print("device", paddle.device.get_device())
PY
cat >/out/README.txt <<"EOF"
Training container is functional. Next step is to mount PaddleOCR source/config and run a recognition fine-tune against /data/train.txt and /data/val.txt.
EOF
ls -lh /data/train.txt /data/val.txt
head -5 /data/train.txt
'

echo "wrote $OUT"
