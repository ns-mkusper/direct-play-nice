//! Timestamp helpers shared by video, audio, and subtitle processing.
//!
//! FFmpeg exposes several timestamp sources and sentinel values. These helpers
//! keep the pipeline's choices explicit: prefer best-effort frame timestamps,
//! preserve `AV_NOPTS_VALUE`, and only synthesize monotonic subtitle DTS when a
//! muxer would otherwise reject the stream.

use rsmpeg::avcodec::AVPacket;
use rsmpeg::avutil::AVFrame;
use rsmpeg::ffi;

/// Returns the best timestamp to feed an encoder for a decoded frame.
pub(crate) fn best_effort_frame_pts(frame: &AVFrame) -> i64 {
    if frame.best_effort_timestamp != ffi::AV_NOPTS_VALUE {
        frame.best_effort_timestamp
    } else {
        frame.pts
    }
}

/// Rescales a timestamp between FFmpeg time bases while preserving NOPTS.
pub(crate) fn rescale_timestamp(
    value: i64,
    source_time_base: ffi::AVRational,
    target_time_base: ffi::AVRational,
) -> i64 {
    if value == ffi::AV_NOPTS_VALUE {
        ffi::AV_NOPTS_VALUE
    } else {
        unsafe { ffi::av_rescale_q(value, source_time_base, target_time_base) }
    }
}

/// Chooses a subtitle PTS, synthesizing a stable fallback only when FFmpeg did
/// not provide one.
pub(crate) fn subtitle_pts(
    subtitle_pts: i64,
    packet_pts: i64,
    last_written_dts: Option<i64>,
) -> i64 {
    if subtitle_pts != ffi::AV_NOPTS_VALUE {
        subtitle_pts
    } else if packet_pts != ffi::AV_NOPTS_VALUE {
        packet_pts
    } else {
        last_written_dts.map(|prev| prev + 1).unwrap_or(0)
    }
}

/// Chooses a subtitle DTS from the demuxed packet or falls back to PTS.
pub(crate) fn subtitle_dts(packet_dts: i64, pts: i64) -> i64 {
    if packet_dts != ffi::AV_NOPTS_VALUE {
        packet_dts
    } else {
        pts
    }
}

/// Ensures packet DTS remains strictly increasing for muxers that reject
/// repeated subtitle timestamps. Returns the adjusted `(old, new)` pair when it
/// modifies the packet.
pub(crate) fn enforce_monotonic_dts(
    packet: &mut AVPacket,
    last_written_dts: Option<i64>,
) -> Option<(i64, i64)> {
    let previous = last_written_dts?;
    let current = packet.dts;
    if current > previous {
        return None;
    }

    let adjusted = previous + 1;
    packet.set_dts(adjusted);
    if packet.pts < adjusted {
        packet.set_pts(adjusted);
    }
    Some((current, adjusted))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rsmpeg::avcodec::AVPacket;

    #[test]
    fn enforce_monotonic_dts_allows_increasing_packets() {
        let mut packet = AVPacket::new();
        packet.set_dts(11);
        packet.set_pts(12);

        assert_eq!(enforce_monotonic_dts(&mut packet, Some(10)), None);
        assert_eq!(packet.dts, 11);
        assert_eq!(packet.pts, 12);
    }

    #[test]
    fn enforce_monotonic_dts_clamps_duplicate_dts_and_pts() {
        let mut packet = AVPacket::new();
        packet.set_dts(10);
        packet.set_pts(10);

        assert_eq!(enforce_monotonic_dts(&mut packet, Some(10)), Some((10, 11)));
        assert_eq!(packet.dts, 11);
        assert_eq!(packet.pts, 11);
    }

    #[test]
    fn enforce_monotonic_dts_preserves_later_pts() {
        let mut packet = AVPacket::new();
        packet.set_dts(10);
        packet.set_pts(15);

        assert_eq!(enforce_monotonic_dts(&mut packet, Some(10)), Some((10, 11)));
        assert_eq!(packet.dts, 11);
        assert_eq!(packet.pts, 15);
    }
}
