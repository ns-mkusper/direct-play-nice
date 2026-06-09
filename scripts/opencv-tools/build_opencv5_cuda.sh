#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: build_opencv5_cuda.sh [options]

Build a minimal OpenCV 5 + CUDA install and the direct_play_nice CUDA OCR
preprocess shim.

Options:
  --prefix PATH       Install prefix (default: /opt/direct-play-nice/opencv5-cuda)
  --work-dir PATH     Source/build work directory (default: /tmp/dpn-opencv5-cuda-build)
  --tag TAG           OpenCV/OpenCV contrib tag (default: 5.0.0)
  --cuda-arch ARCH    CUDA architecture list (default: 5.2 for GTX 960/Maxwell)
  --jobs N            Build parallelism (default: nproc)
  --skip-opencv       Only build/install the shim against an existing prefix
  -h, --help          Show this help

Notes:
  - Use a CUDA 12.x compiler/runtime for Maxwell/Pascal/Volta systems. CUDA 13
    is intentionally not targeted by this script.
  - The resulting shim is installed at:
      $PREFIX/lib/libdpn_opencv5_cuda_preprocess.so
EOF
}

PREFIX="/opt/direct-play-nice/opencv5-cuda"
WORK_DIR="/tmp/dpn-opencv5-cuda-build"
TAG="5.0.0"
CUDA_ARCH="5.2"
JOBS="$(nproc 2>/dev/null || echo 2)"
SKIP_OPENCV=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --prefix|--work-dir|--tag|--cuda-arch|--jobs)
      if [[ $# -lt 2 || "$2" == --* ]]; then
        echo "Missing value for $1" >&2
        usage
        exit 2
      fi
      case "$1" in
        --prefix) PREFIX="$2" ;;
        --work-dir) WORK_DIR="$2" ;;
        --tag) TAG="$2" ;;
        --cuda-arch) CUDA_ARCH="$2" ;;
        --jobs) JOBS="$2" ;;
      esac
      shift 2
      ;;
    --skip-opencv) SKIP_OPENCV=1; shift ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown argument: $1" >&2; usage; exit 2 ;;
  esac
done

if [[ "${CUDA_HOME:-${CUDA_PATH:-}}" == *"cuda-13"* ]] || nvcc --version 2>/dev/null | grep -q 'release 13\.'; then
  echo "Refusing CUDA 13 for OpenCV CUDA build; use CUDA 12.x for Maxwell/Pascal/Volta compatibility." >&2
  exit 1
fi

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
mkdir -p "$WORK_DIR" "$PREFIX"

export PKG_CONFIG_PATH="$PREFIX/lib/pkgconfig:$PREFIX/lib64/pkgconfig:${PKG_CONFIG_PATH:-}"
export LD_LIBRARY_PATH="$PREFIX/lib:$PREFIX/lib64:${LD_LIBRARY_PATH:-}"

if [[ "$SKIP_OPENCV" != "1" ]]; then
  for tool in git cmake pkg-config c++; do
    command -v "$tool" >/dev/null 2>&1 || { echo "Missing required tool: $tool" >&2; exit 1; }
  done
  command -v nvcc >/dev/null 2>&1 || { echo "Missing nvcc; install/use CUDA 12.x before building OpenCV CUDA." >&2; exit 1; }

  if [[ ! -d "$WORK_DIR/opencv/.git" ]]; then
    git clone --depth 1 --branch "$TAG" https://github.com/opencv/opencv.git "$WORK_DIR/opencv"
  fi
  if [[ ! -d "$WORK_DIR/opencv_contrib/.git" ]]; then
    git clone --depth 1 --branch "$TAG" https://github.com/opencv/opencv_contrib.git "$WORK_DIR/opencv_contrib"
  fi

  cmake_compiler_args=()
  if [[ -n "${DPN_OPENCV5_CUDA_HOST_CC:-}" ]]; then
    cmake_compiler_args+=("-DCMAKE_C_COMPILER=${DPN_OPENCV5_CUDA_HOST_CC}")
  fi
  if [[ -n "${DPN_OPENCV5_CUDA_HOST_CXX:-}" ]]; then
    cmake_compiler_args+=(
      "-DCMAKE_CXX_COMPILER=${DPN_OPENCV5_CUDA_HOST_CXX}"
      "-DCUDA_HOST_COMPILER=${DPN_OPENCV5_CUDA_HOST_CXX}"
      "-DCMAKE_CUDA_HOST_COMPILER=${DPN_OPENCV5_CUDA_HOST_CXX}"
    )
  fi

  rm -rf "$WORK_DIR/build"

  cmake -S "$WORK_DIR/opencv" -B "$WORK_DIR/build" \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX="$PREFIX" \
    "${cmake_compiler_args[@]}" \
    -DOPENCV_EXTRA_MODULES_PATH="$WORK_DIR/opencv_contrib/modules" \
    -DOPENCV_GENERATE_PKGCONFIG=ON \
    -DBUILD_LIST=core,imgproc,cudev,cudaarithm,cudaimgproc,cudafilters \
    -DWITH_CUDA=ON \
    -DCUDA_ARCH_BIN="$CUDA_ARCH" \
    -DCUDA_ARCH_PTX= \
    -DCUDA_NVCC_FLAGS="--allow-unsupported-compiler" \
    -DWITH_CUDNN=OFF \
    -DWITH_NVCUVID=OFF \
    -DBUILD_TESTS=OFF \
    -DBUILD_PERF_TESTS=OFF \
    -DBUILD_EXAMPLES=OFF \
    -DBUILD_opencv_python_bindings_generator=OFF \
    -DBUILD_opencv_python_tests=OFF
  cmake --build "$WORK_DIR/build" --parallel "$JOBS"
  cmake --install "$WORK_DIR/build"
fi

pc_name=""
if pkg-config --exists opencv5; then
  pc_name="opencv5"
elif pkg-config --exists opencv4; then
  pc_name="opencv4"
else
  echo "Could not find opencv5/opencv4 via pkg-config under $PREFIX. Set PKG_CONFIG_PATH and retry." >&2
  exit 1
fi

cxxflags="$(pkg-config --cflags "$pc_name")"
libs="$(pkg-config --libs "$pc_name")"
mkdir -p "$PREFIX/lib"
shim_cxx="${DPN_OPENCV5_CUDA_HOST_CXX:-${CXX:-c++}}"
"$shim_cxx" -std=c++17 -O3 -DNDEBUG -fPIC -shared \
  $cxxflags \
  "$repo_root/scripts/opencv-tools/dpn_opencv5_cuda_preprocess.cpp" \
  -o "$PREFIX/lib/libdpn_opencv5_cuda_preprocess.so" \
  $libs

cat <<EOF
Installed OpenCV CUDA preprocess runtime:
  prefix: $PREFIX
  shim:   $PREFIX/lib/libdpn_opencv5_cuda_preprocess.so
  pkg:    $pc_name $(pkg-config --modversion "$pc_name")

Runtime environment:
  export LD_LIBRARY_PATH="$PREFIX/lib:$PREFIX/lib64:\${LD_LIBRARY_PATH:-}"
  export DPN_OPENCV5_CUDA_PREPROCESS_LIB="$PREFIX/lib/libdpn_opencv5_cuda_preprocess.so"
EOF
