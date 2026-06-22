# Architecture

`direct-play-nice` converts media by separating policy decisions from FFmpeg's
packet loop.

## Conversion Model

1. Runtime configuration is resolved from CLI arguments and optional config.
2. Device profiles are intersected to choose a target container, video codec,
   audio codec, resolution limits, bitrate limits, and H.264 constraints.
3. The input is probed for direct-play compatibility. FFmpeg's detected demuxer
   is the primary container signal; filename extension is used only to
   disambiguate MOV-family containers or as a fallback.
4. Conversion creates an explicit input-to-output stream map. Input stream
   indexes are used only for demuxed packets; output stream indexes are used
   only for encoded packets and muxer metadata.
5. Video, audio, and subtitle streams are decoded, transformed, encoded, and
   muxed through separate stream-processing paths.
6. Optional OCR post-processing handles bitmap subtitles and remuxes generated
   text subtitles into the final output.
7. Post-write verification checks H.264 constraints. Output validation is
   enabled by default: the final media file is reopened to verify expected
   stream codecs, stream hygiene, temporal consistency, and sampled visual
   statistics for obvious corruption such as repeated green-screen frames.
   The visual layer exists because a file can decode cleanly and still look
   dangerously wrong to users. Operators can tune how many frames are scanned,
   how often samples are inspected, and what sampled corruption ratio fails.

## FFmpeg Boundaries

Most FFmpeg operations use rsmpeg wrappers. Raw pointer access is isolated in
small helpers where possible:

- `ffmpeg_ext` contains metadata reads, packet allocation, buffer unref, and
  other narrow unsafe operations.
- `timestamp` contains shared timestamp selection, rescaling, and monotonic DTS
  adjustment.
- `pipeline_streams` owns per-packet video/audio/subtitle processing.
- `pipeline_codec` owns encoder setup and rate/profile options.
- `pipeline_assessment` owns direct-play compatibility explanations.

## Content Policy

Playable A/V streams are preserved by conversion. Attachments, data streams,
and attached pictures are metadata for direct-play targets and are skipped.
Extra video streams are governed by `--unsupported-video-policy`. Text
subtitles are converted when included; bitmap subtitles are deferred to OCR
unless subtitle processing is skipped.

Failures are intentionally policy-driven. Audio setup failures abort because
they would risk invalid audio. Subtitle stream failures follow
`--subtitle-failure-policy`: the default isolates the bad subtitle stream so
A/V conversion can still complete, while strict mode aborts. Hardware encoder
failures may retry with software when a safe fallback is available.
