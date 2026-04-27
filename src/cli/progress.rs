use log::info;
use rsmpeg::avutil::ra;
use rsmpeg::ffi;

/// Reports transcoding progress at coarse percentage intervals.
pub(crate) struct ProgressTracker {
    duration_us: i64,
    last_reported_percent: i64,
}

/// Implements behavior for `ProgressTracker`.
impl ProgressTracker {
    /// Builds a tracker for a media item duration in microseconds.
    pub(crate) fn new(duration_us: i64) -> Self {
        Self {
            duration_us: duration_us.max(1),
            last_reported_percent: -1,
        }
    }

    /// Emits progress logs based on packet timestamps.
    pub(crate) fn report(&mut self, pts: i64, time_base: ffi::AVRational) {
        if pts == ffi::AV_NOPTS_VALUE {
            return;
        }
        let current_us =
            unsafe { ffi::av_rescale_q(pts, time_base, ra(1, ffi::AV_TIME_BASE as i32)) };
        if current_us < 0 {
            return;
        }
        let percent = ((current_us * 100) / self.duration_us).clamp(0, 100);
        if percent >= self.last_reported_percent + 5
            || (percent == 100 && self.last_reported_percent < 100)
        {
            info!("Progress: {}%", percent);
            self.last_reported_percent = percent;
        }
    }

    /// Ensures the final `100%` progress line is emitted once.
    pub(crate) fn finish(&mut self) {
        if self.last_reported_percent < 100 {
            self.last_reported_percent = 100;
            info!("Progress: 100%");
        }
    }
}
