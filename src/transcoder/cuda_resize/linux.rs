//! CUDA video resize filter graph support.

use anyhow::{anyhow, bail, Context, Result};
use log::{debug, trace};
use rsmpeg::avutil::AVFrame;
use rsmpeg::error::RsmpegError;
use rsmpeg::ffi;
use std::ffi::CString;
use std::ptr;

use crate::gpu::scale_cuda_filter_available;
use crate::transcoder::ffmpeg_diagnostics::av_error_to_string;
use crate::types::ResizeQuality;

pub(crate) fn cuda_resize_supported_for_quality(quality: ResizeQuality) -> bool {
    matches!(
        quality,
        ResizeQuality::Bilinear | ResizeQuality::Bicubic | ResizeQuality::Lanczos
    )
}

pub(crate) fn cuda_resize_interpolation(quality: ResizeQuality) -> Option<&'static str> {
    match quality {
        ResizeQuality::Bilinear => Some("bilinear"),
        ResizeQuality::Bicubic => Some("bicubic"),
        ResizeQuality::Lanczos => Some("lanczos"),
        ResizeQuality::FastBilinear | ResizeQuality::Spline => None,
    }
}

pub(crate) fn attach_cuda_encoder_frames_context(
    encode_context: &mut rsmpeg::avcodec::AVCodecContext,
    device: *mut ffi::AVBufferRef,
    sw_format: ffi::AVPixelFormat,
) -> Result<()> {
    unsafe {
        if device.is_null() {
            bail!("No CUDA device available for encoder hardware frames context");
        }
        let frames_ref = ffi::av_hwframe_ctx_alloc(device);
        if frames_ref.is_null() {
            bail!("Failed to allocate CUDA encoder hardware frames context");
        }
        let frames_ctx = (*frames_ref).data as *mut ffi::AVHWFramesContext;
        if frames_ctx.is_null() {
            unref_buffer(frames_ref);
            bail!("CUDA encoder hardware frames context has null data");
        }
        (*frames_ctx).format = ffi::AV_PIX_FMT_CUDA;
        (*frames_ctx).sw_format = sw_format;
        (*frames_ctx).width = encode_context.width;
        (*frames_ctx).height = encode_context.height;
        (*frames_ctx).initial_pool_size = 8;
        let init_ret = ffi::av_hwframe_ctx_init(frames_ref);
        if init_ret < 0 {
            unref_buffer(frames_ref);
            bail!(
                "Failed to initialize CUDA encoder hardware frames context: {}",
                av_error_to_string(init_ret)
            );
        }
        (*encode_context.as_mut_ptr()).hw_frames_ctx = frames_ref;
    }
    Ok(())
}

pub(crate) struct CudaResizeFilter {
    graph: *mut ffi::AVFilterGraph,
    buffersrc: *mut ffi::AVFilterContext,
    scale: *mut ffi::AVFilterContext,
    buffersink: *mut ffi::AVFilterContext,
    source_width: i32,
    source_height: i32,
    target_width: i32,
    target_height: i32,
    quality: ResizeQuality,
}

impl CudaResizeFilter {
    pub(crate) fn is_compatible(
        &self,
        source_width: i32,
        source_height: i32,
        target_width: i32,
        target_height: i32,
        quality: ResizeQuality,
    ) -> bool {
        self.source_width == source_width
            && self.source_height == source_height
            && self.target_width == target_width
            && self.target_height == target_height
            && self.quality == quality
    }

    pub(crate) fn new(
        frame: &AVFrame,
        source_time_base: ffi::AVRational,
        target_width: i32,
        target_height: i32,
        quality: ResizeQuality,
    ) -> Result<Self> {
        if frame.format != ffi::AV_PIX_FMT_CUDA {
            bail!(
                "CUDA resize requires CUDA input frames, got format {}",
                frame.format
            );
        }
        if unsafe { (*frame.as_ptr()).hw_frames_ctx.is_null() } {
            bail!("CUDA resize requires input frames with hw_frames_ctx");
        }
        let interpolation = cuda_resize_interpolation(quality).ok_or_else(|| {
            anyhow!(
                "resize quality '{}' is not supported by scale_cuda",
                quality
            )
        })?;
        if !scale_cuda_filter_available() {
            bail!("FFmpeg filter 'scale_cuda' is not available");
        }

        let source_width = frame.width;
        let source_height = frame.height;

        let graph = unsafe { ffi::avfilter_graph_alloc() };
        if graph.is_null() {
            bail!("Failed to allocate CUDA resize filter graph");
        }

        let mut filter = CudaResizeFilter {
            graph,
            buffersrc: ptr::null_mut(),
            scale: ptr::null_mut(),
            buffersink: ptr::null_mut(),
            source_width,
            source_height,
            target_width,
            target_height,
            quality,
        };

        if let Err(err) = filter.init(frame, source_time_base, interpolation) {
            drop(filter);
            return Err(err);
        }

        debug!(
            "Initialized CUDA resize filter graph: {}x{} -> {}x{} via scale_cuda interp_algo={}",
            source_width, source_height, target_width, target_height, interpolation
        );
        Ok(filter)
    }

    fn init(
        &mut self,
        frame: &AVFrame,
        source_time_base: ffi::AVRational,
        interpolation: &str,
    ) -> Result<()> {
        unsafe {
            let buffer = filter_by_name("buffer")?;
            let scale_cuda = filter_by_name("scale_cuda")?;
            let buffersink = filter_by_name("buffersink")?;

            let src_name = cstring("cuda_resize_src")?;
            let scale_name = cstring("cuda_resize_scale")?;
            let sink_name = cstring("cuda_resize_sink")?;
            self.buffersrc =
                ffi::avfilter_graph_alloc_filter(self.graph, buffer, src_name.as_ptr());
            if self.buffersrc.is_null() {
                bail!("Failed to allocate CUDA buffer source filter");
            }

            let params = ffi::av_buffersrc_parameters_alloc();
            if params.is_null() {
                bail!("Failed to allocate CUDA buffer source parameters");
            }
            (*params).format = ffi::AV_PIX_FMT_CUDA;
            (*params).time_base = source_time_base;
            (*params).width = self.source_width;
            (*params).height = self.source_height;
            (*params).sample_aspect_ratio = ffi::AVRational { num: 1, den: 1 };
            (*params).hw_frames_ctx = ffi::av_buffer_ref((*frame.as_ptr()).hw_frames_ctx);
            if (*params).hw_frames_ctx.is_null() {
                ffi::av_free(params.cast());
                bail!("Failed to reference input CUDA hw_frames_ctx for resize filter");
            }
            let params_result = ffi::av_buffersrc_parameters_set(self.buffersrc, params);
            ffi::av_buffer_unref(&mut (*params).hw_frames_ctx);
            ffi::av_free(params.cast());
            check_av(params_result, "setting CUDA buffer source parameters")?;
            check_av(
                ffi::avfilter_init_str(self.buffersrc, ptr::null()),
                "initializing CUDA buffer source",
            )?;

            let scale_args = cstring(format!(
                "w={}:h={}:interp_algo={}:format=yuv420p:passthrough=0",
                self.target_width, self.target_height, interpolation
            ))?;
            check_av(
                ffi::avfilter_graph_create_filter(
                    &mut self.scale,
                    scale_cuda,
                    scale_name.as_ptr(),
                    scale_args.as_ptr(),
                    ptr::null_mut(),
                    self.graph,
                ),
                "creating scale_cuda filter",
            )?;

            check_av(
                ffi::avfilter_graph_create_filter(
                    &mut self.buffersink,
                    buffersink,
                    sink_name.as_ptr(),
                    ptr::null(),
                    ptr::null_mut(),
                    self.graph,
                ),
                "creating CUDA buffer sink",
            )?;

            check_av(
                ffi::avfilter_link(self.buffersrc, 0, self.scale, 0),
                "linking CUDA buffer source to scale_cuda",
            )?;
            check_av(
                ffi::avfilter_link(self.scale, 0, self.buffersink, 0),
                "linking scale_cuda to buffer sink",
            )?;
            check_av(
                ffi::avfilter_graph_config(self.graph, ptr::null_mut()),
                "configuring CUDA resize filter graph",
            )?;
        }
        Ok(())
    }

    pub(crate) fn process_frame(&mut self, frame: &AVFrame) -> Result<AVFrame> {
        unsafe {
            trace!(
                "Submitting CUDA frame {}x{} to scale_cuda resize graph",
                frame.width,
                frame.height
            );
            check_av(
                ffi::av_buffersrc_add_frame_flags(
                    self.buffersrc,
                    frame.as_ptr() as *mut ffi::AVFrame,
                    ffi::AV_BUFFERSRC_FLAG_KEEP_REF as i32,
                ),
                "submitting frame to CUDA resize graph",
            )?;

            let mut output = AVFrame::new();
            match ffi::av_buffersink_get_frame_flags(self.buffersink, output.as_mut_ptr(), 0) {
                ret if ret >= 0 => Ok(output),
                ret if ret == ffi::AVERROR(libc::EAGAIN as u32) => {
                    Err(anyhow!(RsmpegError::BufferSinkDrainError))
                }
                ret if ret == ffi::AVERROR_EOF => Err(anyhow!(RsmpegError::BufferSinkEofError)),
                ret => Err(anyhow!(
                    "Failed to read CUDA-resized frame from filter graph: {}",
                    av_error_to_string(ret)
                )),
            }
        }
    }
}

impl Drop for CudaResizeFilter {
    fn drop(&mut self) {
        unsafe {
            if !self.graph.is_null() {
                ffi::avfilter_graph_free(&mut self.graph);
            }
        }
    }
}

fn cstring(value: impl Into<Vec<u8>>) -> Result<CString> {
    CString::new(value).context("building FFmpeg filter string")
}

unsafe fn filter_by_name(name: &str) -> Result<*const ffi::AVFilter> {
    let name = cstring(name.as_bytes().to_vec())?;
    let filter = unsafe { ffi::avfilter_get_by_name(name.as_ptr()) };
    if filter.is_null() {
        bail!(
            "FFmpeg filter '{}' is not available",
            name.to_string_lossy()
        );
    }
    Ok(filter)
}

fn check_av(ret: i32, action: &str) -> Result<()> {
    if ret < 0 {
        bail!("Failed while {}: {}", action, av_error_to_string(ret));
    }
    Ok(())
}

unsafe fn unref_buffer(buffer: *mut ffi::AVBufferRef) {
    let mut buffer = buffer;
    unsafe { ffi::av_buffer_unref(&mut buffer) };
}
