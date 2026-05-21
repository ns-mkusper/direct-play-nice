//! CUDA video resize filter graph support.

#[cfg(target_os = "linux")]
mod linux;
#[cfg(target_os = "linux")]
pub(crate) use linux::*;

#[cfg(not(target_os = "linux"))]
mod non_linux {
    use anyhow::{bail, Result};
    use rsmpeg::avcodec::AVCodecContext;
    use rsmpeg::avutil::AVFrame;
    use rsmpeg::ffi;

    use crate::types::ResizeQuality;

    pub(crate) fn cuda_resize_supported_for_quality(_quality: ResizeQuality) -> bool {
        false
    }

    pub(crate) fn scale_cuda_filter_available() -> bool {
        false
    }

    pub(crate) fn attach_cuda_encoder_frames_context(
        _encode_context: &mut AVCodecContext,
        _device: *mut ffi::AVBufferRef,
        _sw_format: ffi::AVPixelFormat,
    ) -> Result<()> {
        bail!("CUDA resize backend is only available on Linux builds")
    }

    pub(crate) struct CudaResizeFilter;

    impl CudaResizeFilter {
        pub(crate) fn is_compatible(
            &self,
            _source_width: i32,
            _source_height: i32,
            _target_width: i32,
            _target_height: i32,
            _quality: ResizeQuality,
        ) -> bool {
            false
        }

        pub(crate) fn new(
            _frame: &AVFrame,
            _source_time_base: ffi::AVRational,
            _target_width: i32,
            _target_height: i32,
            _quality: ResizeQuality,
        ) -> Result<Self> {
            bail!("CUDA resize backend is only available on Linux builds")
        }

        pub(crate) fn process_frame(&mut self, _frame: &AVFrame) -> Result<AVFrame> {
            bail!("CUDA resize backend is only available on Linux builds")
        }
    }
}

#[cfg(not(target_os = "linux"))]
pub(crate) use non_linux::*;
