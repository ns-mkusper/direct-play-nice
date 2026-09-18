# syntax=docker/dockerfile:1

# Build stage: statically links FFmpeg (vcpkg, same as release CI).
FROM rust:1-bookworm AS builder

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential cmake ninja-build nasm curl git pkg-config python3 \
    autoconf autoconf-archive automake libtool bison flex gettext libclang-dev \
    zip unzip tar libdrm-dev \
    && rm -rf /var/lib/apt/lists/*

RUN cargo install cargo-vcpkg

WORKDIR /src

# Build vcpkg dependencies against a stub crate first so the expensive FFmpeg
# build layer is cached until Cargo.toml changes. Cargo.lock is not tracked;
# Cargo resolves it inside the image, just as it does in release CI.
COPY Cargo.toml build.rs ./
RUN mkdir -p src benches \
    && echo 'fn main() {}' > src/main.rs \
    && echo 'fn main() {}' > benches/ocr_benchmark.rs \
    && echo 'fn main() {}' > benches/transcode_benchmark.rs \
    && echo 'fn main() {}' > benches/resize_benchmark.rs
ENV VCPKG_FEATURE_FLAGS=manifests,binarycaching
RUN cargo vcpkg --verbose build

COPY . .
# The stub layer above may leave stale fingerprints; touch real sources.
# No --locked: Cargo.lock is generated during the build, not tracked in Git.
RUN touch src/main.rs && cargo build --profile dist

# Runtime stage: VA-API drivers, ONNX Runtime, CA certs for model downloads.
FROM debian:trixie-slim AS runtime
ARG TARGETARCH

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates curl libva2 libva-drm2 va-driver-all \
    && rm -rf /var/lib/apt/lists/*

# ONNX Runtime shared library for subtitle OCR (version matching the ort pin).
ARG ORT_VERSION=1.16.3
RUN set -eux; \
    case "${TARGETARCH}" in \
      amd64) ort_arch=x64 ;; \
      arm64) ort_arch=aarch64 ;; \
      *) echo "unsupported arch: ${TARGETARCH}" && exit 1 ;; \
    esac; \
    curl -fsSL "https://github.com/microsoft/onnxruntime/releases/download/v${ORT_VERSION}/onnxruntime-linux-${ort_arch}-${ORT_VERSION}.tgz" \
      | tar -xz -C /opt; \
    mv "/opt/onnxruntime-linux-${ort_arch}-${ORT_VERSION}" /opt/onnxruntime

COPY --from=builder /src/target/dist/direct_play_nice /usr/local/bin/direct_play_nice

ENV ORT_DYLIB_PATH=/opt/onnxruntime/lib/libonnxruntime.so \
    LD_LIBRARY_PATH=/opt/onnxruntime/lib \
    DPN_OCR_MODEL_DIR=/config/models \
    NVIDIA_VISIBLE_DEVICES=all \
    NVIDIA_DRIVER_CAPABILITIES=compute,video,utility

VOLUME ["/config"]

ENTRYPOINT ["direct_play_nice"]
CMD ["--help"]
