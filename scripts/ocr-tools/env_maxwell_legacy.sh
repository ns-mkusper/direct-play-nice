#!/usr/bin/env bash
set -euo pipefail

# Source this file to configure a Maxwell-friendly OCR runtime:
#   source scripts/ocr-tools/env_maxwell_legacy.sh
# Optional overrides:
#   DPN_LEGACY_ORT_LIB=/opt/direct-play-nice/ort116-runtime/lib
#   DPN_LEGACY_CUDNN8_LIB=/opt/direct-play-nice/cudnn8-runtime/usr/lib
#   DPN_LEGACY_CUDA12_SITEPKG=/home/$USER/dpn_tmp/pydeps/lib/python3.14/site-packages/nvidia
#   DPN_LEGACY_OCR_DEVICES=0,1

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  echo "Run this script with 'source', not as an executable." >&2
  exit 2
fi

ort_lib="${DPN_LEGACY_ORT_LIB:-/opt/direct-play-nice/ort116-runtime/lib}"
cudnn8_lib="${DPN_LEGACY_CUDNN8_LIB:-/opt/direct-play-nice/cudnn8-runtime/usr/lib}"
cuda12_sitepkg="${DPN_LEGACY_CUDA12_SITEPKG:-$HOME/dpn_tmp/pydeps/lib/python3.14/site-packages/nvidia}"
ocr_devices="${DPN_LEGACY_OCR_DEVICES:-0,1}"

if [[ ! -f "$ort_lib/libonnxruntime.so" ]]; then
  echo "Missing libonnxruntime at: $ort_lib/libonnxruntime.so" >&2
  return 1
fi
if [[ ! -d "$cuda12_sitepkg" ]]; then
  echo "Missing CUDA12 python site-packages dir: $cuda12_sitepkg" >&2
  return 1
fi

cuda12_libs=()
while IFS= read -r libdir; do
  cuda12_libs+=("$libdir")
done < <(find "$cuda12_sitepkg" -maxdepth 2 -type d -name lib | sort)

if [[ "${#cuda12_libs[@]}" -eq 0 ]]; then
  echo "No CUDA12 lib directories found under: $cuda12_sitepkg" >&2
  return 1
fi

joined_cuda12="$(IFS=:; echo "${cuda12_libs[*]}")"

export ORT_DYLIB_PATH="$ort_lib/libonnxruntime.so"
export LD_LIBRARY_PATH="$cudnn8_lib:$joined_cuda12:$ort_lib:${LD_LIBRARY_PATH:-}"
export DPN_OCR_REQUIRE_GPU=1
export DPN_OCR_ALLOW_LEGACY_CUDA=1
export DPN_OCR_CUDA_DEVICES="$ocr_devices"
export DPN_OCR_JOBS_PER_GPU="${DPN_OCR_JOBS_PER_GPU:-1}"
export DPN_OCR_MAX_JOBS="${DPN_OCR_MAX_JOBS:-4}"
export DPN_MAX_JOBS="${DPN_MAX_JOBS:-1}"

echo "Configured Maxwell legacy OCR runtime:"
echo "  ORT_DYLIB_PATH=$ORT_DYLIB_PATH"
echo "  DPN_OCR_CUDA_DEVICES=$DPN_OCR_CUDA_DEVICES"
