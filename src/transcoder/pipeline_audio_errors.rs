//! Per-stream accounting for audio packets the decoder rejects as invalid data.
//!
//! Damaged sources (for example Matroska files with a few corrupt FLAC frames)
//! make FFmpeg's audio decoders return `AVERROR_INVALIDDATA` for individual
//! packets. FFmpeg's own CLI skips such packets and keeps going. This module
//! decides whether a rejected packet may be dropped and when a stream has lost
//! too much audio for the output to be worth keeping.
//!
//! The same sources often lose whole Matroska clusters in the demuxer, which
//! leaves holes in the audio timestamps without any decoder error. Output audio
//! timestamps come from a running sample count, so the tracker also follows the
//! input timeline and reports holes to fill with silence; otherwise all audio
//! after a hole would play early relative to the video.

use anyhow::{bail, Result};

use crate::types::AudioDecodeErrorPolicy;

/// Timestamp holes up to this length (in seconds) are not filled. Matroska
/// stores timestamps in milliseconds, so frame starts jitter by a few samples;
/// small differences are carried over and filled once they add up.
pub(crate) const AUDIO_GAP_FILL_MIN_SECS: f64 = 0.05;
/// Timestamp jumps longer than this (in seconds) are treated as broken
/// timestamps rather than lost audio: the timeline is resynchronized without
/// inserting silence. Holes from lost clusters in damaged rips are a few
/// seconds long.
pub(crate) const AUDIO_GAP_FILL_MAX_SECS: f64 = 60.0;

/// Limits applied to invalid audio packets for every audio stream.
#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) struct AudioDecodeErrorLimits {
    pub(crate) policy: AudioDecodeErrorPolicy,
    /// Drops above this count fail the stream as soon as they happen.
    pub(crate) max_packets: u64,
    /// Drops above this share of the stream's packets fail the stream once all
    /// packets have been read.
    pub(crate) max_ratio: f64,
}

impl AudioDecodeErrorLimits {
    pub(crate) fn validate(&self) -> Result<()> {
        if !self.max_ratio.is_finite() || self.max_ratio < 0.0 || self.max_ratio > 1.0 {
            bail!(
                "--audio-decode-error-max-ratio must be between 0 and 1 (got {})",
                self.max_ratio
            );
        }
        Ok(())
    }
}

/// Outcome of recording one invalid audio packet.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) enum AudioDecodeErrorAction {
    /// Drop the packet and keep decoding the stream.
    Skip,
    /// Abort the conversion; the message explains which limit was hit.
    Fail(AudioDecodeErrorFailure),
}

/// Reason an invalid audio packet ends the conversion.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) enum AudioDecodeErrorFailure {
    /// `--audio-decode-error-policy=fail` rejects every invalid packet.
    StrictPolicy,
    /// The stream dropped more packets than `--audio-decode-error-max-packets`.
    TooManyPackets { dropped: u64, max_packets: u64 },
}

impl std::fmt::Display for AudioDecodeErrorFailure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::StrictPolicy => write!(f, "--audio-decode-error-policy=fail"),
            Self::TooManyPackets {
                dropped,
                max_packets,
            } => write!(
                f,
                "{} invalid packets dropped, above --audio-decode-error-max-packets={}",
                dropped, max_packets
            ),
        }
    }
}

/// Running per-stream counters for invalid audio packets.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub(crate) struct AudioDecodeErrorTracker {
    /// Packets sent to the decoder for this stream.
    pub(crate) packets: u64,
    /// Packets dropped because the decoder rejected them as invalid data.
    pub(crate) dropped: u64,
    /// Silence samples inserted in place of dropped packets.
    pub(crate) padded_samples: u64,
    /// Sample count of the last decoded frame; used to size the silence gap
    /// when a dropped packet carries no duration.
    pub(crate) last_frame_samples: i32,
    /// Expected start of the next decoded frame on the input timeline, in
    /// output samples. `None` until the first frame with a timestamp.
    pub(crate) next_sample: Option<i64>,
    /// Silence samples inserted for holes in the input timestamps.
    pub(crate) gap_filled_samples: u64,
}

/// What to do before queueing a decoded frame.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) enum AudioTimelineAction {
    /// The frame continues the timeline.
    Continue,
    /// Insert this many samples of silence before the frame.
    FillGap(i64),
    /// The timestamp jumped by this many samples; resynchronize without filling.
    Discontinuity(i64),
}

impl AudioDecodeErrorTracker {
    pub(crate) fn record_packet(&mut self) {
        self.packets += 1;
    }

    /// Records one invalid packet and decides whether conversion may continue.
    pub(crate) fn record_invalid_packet(
        &mut self,
        limits: &AudioDecodeErrorLimits,
    ) -> AudioDecodeErrorAction {
        self.dropped += 1;
        match limits.policy {
            AudioDecodeErrorPolicy::Fail => {
                AudioDecodeErrorAction::Fail(AudioDecodeErrorFailure::StrictPolicy)
            }
            AudioDecodeErrorPolicy::Tolerate if self.dropped > limits.max_packets => {
                AudioDecodeErrorAction::Fail(AudioDecodeErrorFailure::TooManyPackets {
                    dropped: self.dropped,
                    max_packets: limits.max_packets,
                })
            }
            AudioDecodeErrorPolicy::Tolerate => AudioDecodeErrorAction::Skip,
        }
    }

    /// Records silence inserted for a dropped packet so the timeline does not
    /// report the same samples again as a hole.
    pub(crate) fn record_padding(&mut self, samples: i64) {
        self.padded_samples += samples.max(0) as u64;
        if let Some(next) = self.next_sample.as_mut() {
            *next += samples.max(0);
        }
    }

    /// Places a decoded frame on the input timeline.
    ///
    /// `frame_start` is the frame timestamp in output samples (`None` when the
    /// frame has no timestamp), `nb_samples` its length, and `sample_rate` the
    /// output sample rate used to apply the gap limits.
    pub(crate) fn place_frame(
        &mut self,
        frame_start: Option<i64>,
        nb_samples: i32,
        sample_rate: i32,
    ) -> AudioTimelineAction {
        let nb_samples = i64::from(nb_samples.max(0));
        let (Some(start), Some(expected)) = (frame_start, self.next_sample) else {
            self.next_sample = frame_start
                .or(self.next_sample)
                .map(|start| start + nb_samples);
            return AudioTimelineAction::Continue;
        };
        let rate = f64::from(sample_rate.max(1));
        let min_gap = (AUDIO_GAP_FILL_MIN_SECS * rate) as i64;
        let max_gap = (AUDIO_GAP_FILL_MAX_SECS * rate) as i64;
        let gap = start - expected;
        if gap > max_gap || gap < -max_gap {
            self.next_sample = Some(start + nb_samples);
            AudioTimelineAction::Discontinuity(gap)
        } else if gap > min_gap {
            self.gap_filled_samples += gap as u64;
            self.next_sample = Some(start + nb_samples);
            AudioTimelineAction::FillGap(gap)
        } else {
            self.next_sample = Some(expected + nb_samples);
            AudioTimelineAction::Continue
        }
    }

    /// Share of this stream's packets that were dropped.
    pub(crate) fn drop_ratio(&self) -> f64 {
        if self.packets == 0 {
            0.0
        } else {
            self.dropped as f64 / self.packets as f64
        }
    }

    /// End-of-stream check of the drop share against `max_ratio`.
    pub(crate) fn exceeds_ratio(&self, limits: &AudioDecodeErrorLimits) -> bool {
        self.dropped > 0 && self.drop_ratio() > limits.max_ratio
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn limits(policy: AudioDecodeErrorPolicy) -> AudioDecodeErrorLimits {
        AudioDecodeErrorLimits {
            policy,
            max_packets: 3,
            max_ratio: 0.05,
        }
    }

    #[test]
    fn fail_policy_rejects_first_invalid_packet() {
        let mut tracker = AudioDecodeErrorTracker::default();
        tracker.record_packet();
        assert_eq!(
            tracker.record_invalid_packet(&limits(AudioDecodeErrorPolicy::Fail)),
            AudioDecodeErrorAction::Fail(AudioDecodeErrorFailure::StrictPolicy)
        );
        assert_eq!(tracker.dropped, 1);
    }

    #[test]
    fn tolerate_policy_skips_until_packet_cap_is_exceeded() {
        let limits = limits(AudioDecodeErrorPolicy::Tolerate);
        let mut tracker = AudioDecodeErrorTracker::default();
        for _ in 0..3 {
            assert_eq!(
                tracker.record_invalid_packet(&limits),
                AudioDecodeErrorAction::Skip
            );
        }
        assert_eq!(
            tracker.record_invalid_packet(&limits),
            AudioDecodeErrorAction::Fail(AudioDecodeErrorFailure::TooManyPackets {
                dropped: 4,
                max_packets: 3,
            })
        );
    }

    #[test]
    fn ratio_check_uses_share_of_stream_packets() {
        let limits = limits(AudioDecodeErrorPolicy::Tolerate);
        let mut tracker = AudioDecodeErrorTracker::default();
        for _ in 0..100 {
            tracker.record_packet();
        }
        assert!(!tracker.exceeds_ratio(&limits));

        for _ in 0..5 {
            tracker.record_invalid_packet(&limits);
        }
        assert!((tracker.drop_ratio() - 0.05).abs() < f64::EPSILON);
        assert!(!tracker.exceeds_ratio(&limits), "ratio equal to cap passes");

        tracker.record_invalid_packet(&limits);
        assert!(tracker.exceeds_ratio(&limits));
    }

    #[test]
    fn default_limits_accept_scattered_damage_and_reject_undecodable_streams() {
        let limits = AudioDecodeErrorLimits {
            policy: AudioDecodeErrorPolicy::Tolerate,
            max_packets: crate::ffmpeg_utils::DEFAULT_AUDIO_DECODE_ERROR_MAX_PACKETS,
            max_ratio: crate::ffmpeg_utils::DEFAULT_AUDIO_DECODE_ERROR_MAX_RATIO,
        };

        // 24-minute FLAC stream (~17k packets) with a few hundred bad frames.
        let mut damaged = AudioDecodeErrorTracker::default();
        for packet in 0..17_000 {
            damaged.record_packet();
            if packet % 45 == 0 {
                assert_eq!(
                    damaged.record_invalid_packet(&limits),
                    AudioDecodeErrorAction::Skip
                );
            }
        }
        assert!(!damaged.exceeds_ratio(&limits));

        // 24-minute AAC stream where most packets fail: rejected once the cap is hit.
        let mut undecodable = AudioDecodeErrorTracker::default();
        let mut failed_at = None;
        for packet in 0..67_000u64 {
            undecodable.record_packet();
            if packet % 8 == 0 {
                if let AudioDecodeErrorAction::Fail(_) = undecodable.record_invalid_packet(&limits)
                {
                    failed_at = Some(packet);
                    break;
                }
            }
        }
        assert!(failed_at.is_some());

        // Short clip with heavy damage stays under the packet cap but fails the ratio.
        let mut short_clip = AudioDecodeErrorTracker::default();
        for packet in 0..2_000 {
            short_clip.record_packet();
            if packet % 5 == 0 {
                short_clip.record_invalid_packet(&limits);
            }
        }
        assert!(short_clip.dropped <= limits.max_packets);
        assert!(short_clip.exceeds_ratio(&limits));
    }

    #[test]
    fn timeline_fills_holes_and_ignores_jitter() {
        let mut tracker = AudioDecodeErrorTracker::default();
        let rate = 48_000;
        assert_eq!(
            tracker.place_frame(Some(960), 4096, rate),
            AudioTimelineAction::Continue
        );
        // Millisecond rounding: a few samples late is not a hole.
        assert_eq!(
            tracker.place_frame(Some(960 + 4096 + 40), 4096, rate),
            AudioTimelineAction::Continue
        );
        assert_eq!(tracker.next_sample, Some(960 + 2 * 4096));
        // A lost two-second cluster is filled with exactly the missing samples.
        let after_hole = 960 + 2 * 4096 + 2 * rate as i64;
        assert_eq!(
            tracker.place_frame(Some(after_hole), 4096, rate),
            AudioTimelineAction::FillGap(2 * rate as i64)
        );
        assert_eq!(tracker.gap_filled_samples, 2 * rate as u64);
        assert_eq!(tracker.next_sample, Some(after_hole + 4096));
    }

    #[test]
    fn timeline_does_not_fill_broken_timestamp_jumps() {
        let mut tracker = AudioDecodeErrorTracker::default();
        let rate = 48_000;
        tracker.place_frame(Some(0), 1024, rate);
        let jump = 3_600 * rate as i64;
        assert_eq!(
            tracker.place_frame(Some(jump), 1024, rate),
            AudioTimelineAction::Discontinuity(jump - 1024)
        );
        assert_eq!(tracker.gap_filled_samples, 0);
        assert_eq!(tracker.next_sample, Some(jump + 1024));
    }

    #[test]
    fn timeline_accounts_for_padding_and_missing_timestamps() {
        let mut tracker = AudioDecodeErrorTracker::default();
        let rate = 48_000;
        tracker.place_frame(Some(0), 4096, rate);
        // A dropped packet padded by duration leaves no hole for the next frame.
        tracker.record_padding(4096);
        assert_eq!(
            tracker.place_frame(Some(8192), 4096, rate),
            AudioTimelineAction::Continue
        );
        assert_eq!(tracker.padded_samples, 4096);
        // Frames without timestamps extend the timeline.
        assert_eq!(
            tracker.place_frame(None, 4096, rate),
            AudioTimelineAction::Continue
        );
        assert_eq!(tracker.next_sample, Some(16_384));
    }

    #[test]
    fn ratio_is_zero_without_packets() {
        let tracker = AudioDecodeErrorTracker::default();
        assert_eq!(tracker.drop_ratio(), 0.0);
        assert!(!tracker.exceeds_ratio(&limits(AudioDecodeErrorPolicy::Tolerate)));
    }

    #[test]
    fn validate_rejects_out_of_range_ratio() {
        let mut limits = limits(AudioDecodeErrorPolicy::Tolerate);
        assert!(limits.validate().is_ok());
        limits.max_ratio = 0.0;
        assert!(limits.validate().is_ok());
        limits.max_ratio = 1.5;
        assert!(limits.validate().is_err());
        limits.max_ratio = f64::NAN;
        assert!(limits.validate().is_err());
    }
}
