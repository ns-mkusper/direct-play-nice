#!/usr/bin/env bash
set -euo pipefail

note() {
  printf '%s\n' "$*"
}

section() {
  printf '\n== %s ==\n' "$*"
}

section "direct-play-nice GPU environment check"
note "date: $(date -u +"%Y-%m-%dT%H:%M:%SZ")"
note "host: $(hostname)"
note "kernel: $(uname -r)"

container="no"
if [ -f /.dockerenv ]; then
  container="yes (/.dockerenv present)"
elif [ -r /proc/1/cgroup ] && grep -Eqi '(docker|containerd|podman|kubepods)' /proc/1/cgroup; then
  container="yes (cgroup indicates container)"
fi
note "container: ${container}"

section "NVIDIA tooling"
if command -v nvidia-smi >/dev/null 2>&1; then
  nvidia-smi || true
  note "-- query summary --"
  nvidia-smi --query-gpu=name,driver_version,utilization.gpu,power.draw --format=csv,noheader,nounits || true
else
  note "nvidia-smi: NOT FOUND"
fi

section "CUDA toolkit"
if command -v nvcc >/dev/null 2>&1; then
  nvcc --version || true
elif [ -r /usr/local/cuda/version.txt ]; then
  note "CUDA version file: /usr/local/cuda/version.txt"
  cat /usr/local/cuda/version.txt
elif [ -r /opt/cuda/version.txt ]; then
  note "CUDA version file: /opt/cuda/version.txt"
  cat /opt/cuda/version.txt
else
  note "CUDA toolkit not detected (nvcc missing, no version.txt found)"
fi

section "cuDNN"
if command -v ldconfig >/dev/null 2>&1; then
  cudnn_lines=$(ldconfig -p 2>/dev/null | grep -i 'libcudnn\.so' || true)
  if [ -n "$cudnn_lines" ]; then
    note "libcudnn found in ldconfig:"
    printf '%s\n' "$cudnn_lines"
  else
    note "libcudnn.so NOT FOUND in ldconfig"
  fi
else
  note "ldconfig not available"
fi

if command -v dpkg >/dev/null 2>&1; then
  note "-- dpkg cuDNN packages --"
  dpkg -l 2>/dev/null | grep -i cudnn || note "no cuDNN packages installed"
fi

if command -v apt-cache >/dev/null 2>&1; then
  note "-- apt-cache libcudnn (first 10) --"
  apt-cache search libcudnn | head -n 10 || true
fi

section "ONNX Runtime shared library"
ort_lib=""
for base in "$HOME/.cache/ort" "$HOME/.local/share/ort" "$HOME/.cache/onnxruntime" /usr/local/lib /usr/lib /usr/lib/x86_64-linux-gnu; do
  if [ -d "$base" ]; then
    match=$(find "$base" -maxdepth 5 -type f -name 'libonnxruntime.so*' 2>/dev/null | head -n 1 || true)
    if [ -n "$match" ]; then
      ort_lib="$match"
      break
    fi
  fi
  if [ -f "$base" ] && [[ "$base" == *libonnxruntime.so* ]]; then
    ort_lib="$base"
    break
  fi
done

if [ -n "$ort_lib" ]; then
  note "found libonnxruntime: $ort_lib"
  if command -v ldd >/dev/null 2>&1; then
    note "-- ldd missing deps --"
    ldd "$ort_lib" | grep -i 'not found' || note "no missing shared libraries reported"
  else
    note "ldd not available"
  fi
else
  note "libonnxruntime.so not found in common locations"
fi

section "NVIDIA container env"
note "NVIDIA_VISIBLE_DEVICES=${NVIDIA_VISIBLE_DEVICES:-<unset>}"
note "NVIDIA_DRIVER_CAPABILITIES=${NVIDIA_DRIVER_CAPABILITIES:-<unset>}"
note "NVIDIA_REQUIRE_CUDA=${NVIDIA_REQUIRE_CUDA:-<unset>}"
note "CUDA_VISIBLE_DEVICES=${CUDA_VISIBLE_DEVICES:-<unset>}"
note "LD_LIBRARY_PATH=${LD_LIBRARY_PATH:-<unset>}"
