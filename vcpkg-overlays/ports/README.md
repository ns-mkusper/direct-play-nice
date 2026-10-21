# vcpkg overlay ports

Local copies of upstream vcpkg ports with one deliberate change each. CI, the
Docker image, and the documented local build set `VCPKG_OVERLAY_PORTS` to this
directory. Refresh a copy from the pinned vcpkg commit in `Cargo.toml` when the
pin moves, then re-apply the change described here.

- `x264`: fetches the archive from the GitHub mirror; code.videolan.org answers
  archive downloads with an anti-bot page.
- `ffnvcodec`: held at 13.0 so NVENC keeps working on the 580 driver branch,
  the last one for Maxwell, Pascal, and Volta GPUs.
- `ffmpeg`: enables `cuda-llvm` and the `scale_cuda` filter on Linux when clang
  is available, so the CUDA resize backend has its filter. Upstream never
  compiles the CUDA kernels.
