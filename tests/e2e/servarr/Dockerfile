# syntax=docker/dockerfile:1
# Build against the same musl/FFmpeg major version as the pinned Arr images.
ARG APP_IMAGE
FROM alpine:3.23@sha256:fd791d74b68913cbb027c6546007b3f0d3bc45125f797758156952bc2d6daf40 AS build
RUN apk add --no-cache rust cargo build-base clang-dev pkgconf ffmpeg-dev libva-dev git ca-certificates
WORKDIR /src
COPY Cargo.toml build.rs ./
COPY .cargo .cargo
COPY src src
COPY benches benches
COPY config config
# FFMPEG_PKG_CONFIG_PATH tells build.rs an external FFmpeg is in use, so it
# skips the link arguments that only the vcpkg build needs.
ENV PKG_CONFIG_PATH=/usr/lib/pkgconfig \
    FFMPEG_PKG_CONFIG_PATH=/usr/lib/pkgconfig \
    RUSTFLAGS="-C target-feature=-crt-static"
# Cargo.lock is not tracked by this repository. Save the resolved lock in the
# image/artifacts so each run records exactly which dependency versions it used.
# Distro FFmpeg is shared; the ffmpeg-static default feature would pull its
# private dependencies into the link.
RUN cargo build --bin direct_play_nice --no-default-features && \
    mkdir -p /opt/dpn && cp target/debug/direct_play_nice Cargo.lock /opt/dpn/

FROM ${APP_IMAGE}
USER root
RUN apk add --no-cache ffmpeg python3 libgcc libstdc++ && mkdir -p /opt/e2e
COPY --from=build /opt/dpn /opt/dpn
COPY tests/e2e/servarr /opt/e2e
RUN chmod 755 /opt/e2e/hook.py && /opt/dpn/direct_play_nice --version
# Keep the image's actual Arr application and init/service-user setup.
# Kubernetes volumes below are disposable; never mount a host library here.
