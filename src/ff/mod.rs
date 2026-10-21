//! Thin ownership layer over [`ffmpeg_next`].
//!
//! ffmpeg-next supplies the raw bindings (`ffmpeg_next::sys`, re-exported here
//! as `ffi`) and owns the long-lived objects whose teardown matters: format
//! contexts close their I/O, codec contexts free private data, subtitles free
//! their rects. The types in this module keep a small, stable surface for the
//! rest of the crate: each dereferences to the underlying C struct so the
//! transcoder can read fields directly, and each carries the handful of
//! methods the pipeline calls. Everything else goes through `ffi`.
//!
//! Errors use `FfmpegError`; `AVERROR(EAGAIN)` and `AVERROR_EOF` on the
//! decode/encode calls map to the drain and flushed variants so callers can
//! match on pipeline state instead of raw codes.

// Setters and accessors here mirror the C field they touch one to one; the
// struct documentation in FFmpeg's headers is the reference for each.
#![allow(missing_docs, dead_code)]

pub use ffmpeg_next::sys as ffi;

use ffmpeg_next::codec::subtitle::Subtitle as OwnedSubtitle;
use ffmpeg_next::{codec, format};
use std::ffi::{CStr, CString};
use std::fmt;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::os::raw::{c_int, c_void};
use std::path::Path;
use std::ptr::{self, NonNull};

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

pub type Result<T> = std::result::Result<T, FfmpegError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FfmpegError {
    AVError(c_int),
    OpenInputError(c_int),
    FindStreamInfoError(c_int),
    SendPacketError(c_int),
    DecoderFullError,
    ReceiveFrameError(c_int),
    DecoderDrainError,
    DecoderFlushedError,
    SendFrameError(c_int),
    SendFrameAgainError,
    ReceivePacketError(c_int),
    EncoderDrainError,
    EncoderFlushedError,
    BufferSinkGetFrameError(c_int),
    BufferSinkDrainError,
    BufferSinkEofError,
    AVFrameDoubleAllocatingError,
    AVFrameInvalidAllocatingError(c_int),
    TryFromIntError(std::num::TryFromIntError),
    Unknown,
}

impl FfmpegError {
    /// The raw FFmpeg error code carried by this error, when there is one.
    pub fn raw_error(&self) -> Option<c_int> {
        use FfmpegError::*;
        match self {
            AVError(c)
            | OpenInputError(c)
            | FindStreamInfoError(c)
            | SendPacketError(c)
            | ReceiveFrameError(c)
            | SendFrameError(c)
            | ReceivePacketError(c)
            | BufferSinkGetFrameError(c)
            | AVFrameInvalidAllocatingError(c) => Some(*c),
            DecoderFullError | SendFrameAgainError | DecoderDrainError | EncoderDrainError
            | BufferSinkDrainError => Some(averror_eagain()),
            DecoderFlushedError | EncoderFlushedError | BufferSinkEofError => {
                Some(ffi::AVERROR_EOF)
            }
            AVFrameDoubleAllocatingError | TryFromIntError(_) | Unknown => None,
        }
    }
}

impl fmt::Display for FfmpegError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use FfmpegError::*;
        match self {
            AVError(c) => write!(f, "AVERROR({c}): {}", strerror(*c)),
            OpenInputError(c) => write!(f, "Cannot open input ({c}): {}", strerror(*c)),
            FindStreamInfoError(c) => write!(f, "Cannot find stream info ({c}): {}", strerror(*c)),
            SendPacketError(c) => write!(f, "Send packet failed ({c}): {}", strerror(*c)),
            DecoderFullError => write!(f, "Decoder is full, receive frames first"),
            ReceiveFrameError(c) => write!(f, "Receive frame failed ({c}): {}", strerror(*c)),
            DecoderDrainError => write!(f, "Decoder drained, send more packets"),
            DecoderFlushedError => write!(f, "Decoder flushed"),
            SendFrameError(c) => write!(f, "Send frame failed ({c}): {}", strerror(*c)),
            SendFrameAgainError => write!(f, "Encoder is full, receive packets first"),
            ReceivePacketError(c) => write!(f, "Receive packet failed ({c}): {}", strerror(*c)),
            EncoderDrainError => write!(f, "Encoder drained, send more frames"),
            EncoderFlushedError => write!(f, "Encoder flushed"),
            BufferSinkGetFrameError(c) => {
                write!(f, "Buffer sink get frame failed ({c}): {}", strerror(*c))
            }
            BufferSinkDrainError => write!(f, "Buffer sink drained"),
            BufferSinkEofError => write!(f, "Buffer sink reached EOF"),
            AVFrameDoubleAllocatingError => write!(f, "Frame buffer already allocated"),
            AVFrameInvalidAllocatingError(c) => {
                write!(f, "Frame buffer allocation failed ({c}): {}", strerror(*c))
            }
            TryFromIntError(e) => write!(f, "{e}"),
            Unknown => write!(f, "Unknown FFmpeg error"),
        }
    }
}

impl std::error::Error for FfmpegError {}

impl From<ffmpeg_next::Error> for FfmpegError {
    fn from(error: ffmpeg_next::Error) -> Self {
        FfmpegError::AVError(c_int::from(error))
    }
}

impl From<std::num::TryFromIntError> for FfmpegError {
    fn from(error: std::num::TryFromIntError) -> Self {
        FfmpegError::TryFromIntError(error)
    }
}

/// Human-readable text for an FFmpeg error code.
pub fn strerror(code: c_int) -> String {
    ffmpeg_next::Error::from(code).to_string()
}

#[inline]
pub fn averror_eagain() -> c_int {
    ffi::AVERROR(ffi::EAGAIN)
}

/// Map a negative FFmpeg return code to [`FfmpegError::AVError`].
#[inline]
pub fn check(ret: c_int) -> Result<c_int> {
    if ret < 0 {
        Err(FfmpegError::AVError(ret))
    } else {
        Ok(ret)
    }
}

/// `AV_BUFFERSRC_FLAG_KEEP_REF` from libavfilter/buffersrc.h. bindgen puts the
/// anonymous enum it lives in under a `_bindgen_ty_N` name that changes with
/// the header set, so the value is spelled out here.
pub const AV_BUFFERSRC_FLAG_KEEP_REF: c_int = 8;

/// Build an [`ffi::AVRational`].
#[inline]
pub const fn ra(num: c_int, den: c_int) -> ffi::AVRational {
    ffi::AVRational { num, den }
}

/// Reinterpret an `AVFrame::format` value as a pixel format.
#[inline]
pub fn pix_fmt_from_i32(value: c_int) -> ffi::AVPixelFormat {
    // SAFETY: AVPixelFormat is a C enum with int representation; FFmpeg stores
    // the same values in AVFrame::format.
    unsafe { std::mem::transmute::<c_int, ffi::AVPixelFormat>(value) }
}

/// Reinterpret an `AVFrame::format` value as a sample format.
#[inline]
pub fn sample_fmt_from_i32(value: c_int) -> ffi::AVSampleFormat {
    // SAFETY: as above, for AVSampleFormat.
    unsafe { std::mem::transmute::<c_int, ffi::AVSampleFormat>(value) }
}

fn cstr_path(path: &CStr) -> &Path {
    Path::new(std::str::from_utf8(path.to_bytes()).unwrap_or(""))
}

/// Generates `set_<field>` setters that write straight into the C struct.
macro_rules! setters {
    ($ty:ty => $($field:ident : $kind:ty),* $(,)?) => {
        impl $ty {
            $(
                paste_setter!($field, $kind);
            )*
        }
    };
}

macro_rules! paste_setter {
    ($field:ident, $kind:ty) => {
        ::paste::paste! {
            #[inline]
            pub fn [<set_ $field>](&mut self, value: $kind) {
                // SAFETY: the pointer is owned by this wrapper and valid for its lifetime.
                unsafe { (*self.as_mut_ptr()).$field = value; }
            }
        }
    };
}

// ---------------------------------------------------------------------------
// Codecs
// ---------------------------------------------------------------------------

/// Borrowed codec descriptor (`const AVCodec*`); FFmpeg owns the table.
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct AVCodecRef<'a> {
    ptr: NonNull<ffi::AVCodec>,
    _marker: PhantomData<&'a ffi::AVCodec>,
}

impl<'a> AVCodecRef<'a> {
    /// # Safety
    /// `ptr` must point at a codec descriptor that outlives `'a`.
    pub unsafe fn from_raw(ptr: NonNull<ffi::AVCodec>) -> Self {
        Self {
            ptr,
            _marker: PhantomData,
        }
    }

    pub fn as_ptr(&self) -> *const ffi::AVCodec {
        self.ptr.as_ptr()
    }

    pub fn name(&self) -> &CStr {
        unsafe { CStr::from_ptr(self.name) }
    }

    pub fn long_name(&self) -> &CStr {
        unsafe { CStr::from_ptr(self.long_name) }
    }

    pub fn hw_config(&self, index: usize) -> Option<ffi::AVCodecHWConfig> {
        let ptr = unsafe { ffi::avcodec_get_hw_config(self.as_ptr(), index as c_int) };
        if ptr.is_null() {
            None
        } else {
            Some(unsafe { *ptr })
        }
    }

    /// Pixel formats this encoder accepts, from `avcodec_get_supported_config`.
    pub fn pix_fmts(&self) -> Option<&'a [ffi::AVPixelFormat]> {
        unsafe {
            self.supported_config::<ffi::AVPixelFormat>(
                ffi::AVCodecConfig::AV_CODEC_CONFIG_PIX_FORMAT,
                |v| *v == ffi::AVPixelFormat::AV_PIX_FMT_NONE,
            )
        }
    }

    /// Sample formats this encoder accepts, from `avcodec_get_supported_config`.
    pub fn sample_fmts(&self) -> Option<&'a [ffi::AVSampleFormat]> {
        unsafe {
            self.supported_config::<ffi::AVSampleFormat>(
                ffi::AVCodecConfig::AV_CODEC_CONFIG_SAMPLE_FORMAT,
                |v| *v == ffi::AVSampleFormat::AV_SAMPLE_FMT_NONE,
            )
        }
    }

    /// Sample rates this encoder accepts, from `avcodec_get_supported_config`.
    pub fn supported_samplerates(&self) -> Option<&'a [c_int]> {
        unsafe {
            self.supported_config::<c_int>(ffi::AVCodecConfig::AV_CODEC_CONFIG_SAMPLE_RATE, |v| {
                *v == 0
            })
        }
    }

    unsafe fn supported_config<T>(
        &self,
        config: ffi::AVCodecConfig,
        is_terminator: impl Fn(&T) -> bool,
    ) -> Option<&'a [T]> {
        let mut data: *const c_void = ptr::null();
        let mut count: c_int = 0;
        let ret = unsafe {
            ffi::avcodec_get_supported_config(
                ptr::null(),
                self.as_ptr(),
                config,
                0,
                &mut data,
                &mut count,
            )
        };
        if ret < 0 || data.is_null() {
            return None;
        }
        let data = data as *const T;
        let mut len = 0usize;
        if count > 0 {
            len = count as usize;
        } else {
            while !is_terminator(unsafe { &*data.add(len) }) {
                len += 1;
            }
        }
        Some(unsafe { std::slice::from_raw_parts(data, len) })
    }
}

impl Deref for AVCodecRef<'_> {
    type Target = ffi::AVCodec;
    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.as_ref() }
    }
}

impl fmt::Debug for AVCodecRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AVCodecRef")
            .field("name", &self.name())
            .finish()
    }
}

/// Codec lookup entry points.
pub struct AVCodec;

impl AVCodec {
    pub fn find_decoder(id: ffi::AVCodecID) -> Option<AVCodecRef<'static>> {
        NonNull::new(unsafe { ffi::avcodec_find_decoder(id) } as *mut ffi::AVCodec)
            .map(|p| unsafe { AVCodecRef::from_raw(p) })
    }

    pub fn find_encoder(id: ffi::AVCodecID) -> Option<AVCodecRef<'static>> {
        NonNull::new(unsafe { ffi::avcodec_find_encoder(id) } as *mut ffi::AVCodec)
            .map(|p| unsafe { AVCodecRef::from_raw(p) })
    }

    pub fn find_decoder_by_name(name: &CStr) -> Option<AVCodecRef<'static>> {
        NonNull::new(unsafe { ffi::avcodec_find_decoder_by_name(name.as_ptr()) } as *mut _)
            .map(|p| unsafe { AVCodecRef::from_raw(p) })
    }

    pub fn find_encoder_by_name(name: &CStr) -> Option<AVCodecRef<'static>> {
        NonNull::new(unsafe { ffi::avcodec_find_encoder_by_name(name.as_ptr()) } as *mut _)
            .map(|p| unsafe { AVCodecRef::from_raw(p) })
    }
}

// ---------------------------------------------------------------------------
// Codec parameters
// ---------------------------------------------------------------------------

/// Owned `AVCodecParameters`.
#[repr(transparent)]
pub struct AVCodecParameters {
    ptr: NonNull<ffi::AVCodecParameters>,
}

impl AVCodecParameters {
    pub fn new() -> Self {
        let ptr = NonNull::new(unsafe { ffi::avcodec_parameters_alloc() })
            .expect("avcodec_parameters_alloc");
        Self { ptr }
    }

    /// # Safety
    /// `ptr` must be an allocated, unowned `AVCodecParameters`.
    pub unsafe fn from_raw(ptr: NonNull<ffi::AVCodecParameters>) -> Self {
        Self { ptr }
    }

    pub fn as_ptr(&self) -> *const ffi::AVCodecParameters {
        self.ptr.as_ptr()
    }

    pub fn as_mut_ptr(&mut self) -> *mut ffi::AVCodecParameters {
        self.ptr.as_ptr()
    }

    pub fn into_raw(self) -> NonNull<ffi::AVCodecParameters> {
        let ptr = self.ptr;
        std::mem::forget(self);
        ptr
    }

    pub fn copy_from_context(&mut self, context: &AVCodecContext) {
        unsafe { ffi::avcodec_parameters_from_context(self.as_mut_ptr(), context.as_ptr()) };
    }

    pub fn copy(&mut self, from: &AVCodecParameters) {
        unsafe { ffi::avcodec_parameters_copy(self.as_mut_ptr(), from.as_ptr()) };
    }

    pub fn codec_type(&self) -> ffi::AVMediaType {
        self.codec_type
    }

    pub fn ch_layout(&self) -> AVChannelLayoutRef<'_> {
        AVChannelLayoutRef(&self.deref().ch_layout)
    }
}

impl Default for AVCodecParameters {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for AVCodecParameters {
    fn clone(&self) -> Self {
        let mut new = Self::new();
        new.copy(self);
        new
    }
}

impl Drop for AVCodecParameters {
    fn drop(&mut self) {
        let mut ptr = self.ptr.as_ptr();
        unsafe { ffi::avcodec_parameters_free(&mut ptr) };
    }
}

impl Deref for AVCodecParameters {
    type Target = ffi::AVCodecParameters;
    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.as_ref() }
    }
}

impl DerefMut for AVCodecParameters {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.ptr.as_mut() }
    }
}

/// Borrowed `AVCodecParameters` owned by a stream.
#[repr(transparent)]
pub struct AVCodecParametersRef<'a> {
    inner: std::mem::ManuallyDrop<AVCodecParameters>,
    _marker: PhantomData<&'a ffi::AVCodecParameters>,
}

impl<'a> AVCodecParametersRef<'a> {
    /// # Safety
    /// `ptr` must stay valid for `'a`.
    pub unsafe fn from_raw(ptr: NonNull<ffi::AVCodecParameters>) -> Self {
        Self {
            inner: std::mem::ManuallyDrop::new(unsafe { AVCodecParameters::from_raw(ptr) }),
            _marker: PhantomData,
        }
    }
}

impl Deref for AVCodecParametersRef<'_> {
    type Target = AVCodecParameters;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// Mutably borrowed `AVCodecParameters` owned by a stream.
#[repr(transparent)]
pub struct AVCodecParametersMut<'a> {
    inner: std::mem::ManuallyDrop<AVCodecParameters>,
    _marker: PhantomData<&'a mut ffi::AVCodecParameters>,
}

impl<'a> AVCodecParametersMut<'a> {
    /// # Safety
    /// `ptr` must stay valid and unaliased for `'a`.
    pub unsafe fn from_raw(ptr: NonNull<ffi::AVCodecParameters>) -> Self {
        Self {
            inner: std::mem::ManuallyDrop::new(unsafe { AVCodecParameters::from_raw(ptr) }),
            _marker: PhantomData,
        }
    }
}

impl Deref for AVCodecParametersMut<'_> {
    type Target = AVCodecParameters;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for AVCodecParametersMut<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

// ---------------------------------------------------------------------------
// Channel layouts
// ---------------------------------------------------------------------------

/// Owned `AVChannelLayout`, uninitialised on drop.
pub struct AVChannelLayout(ffi::AVChannelLayout);

impl AVChannelLayout {
    /// # Safety
    /// `layout` must be initialised and not owned elsewhere.
    pub unsafe fn new(layout: ffi::AVChannelLayout) -> Self {
        Self(layout)
    }

    pub fn from_nb_channels(nb_channels: c_int) -> Self {
        let mut layout: ffi::AVChannelLayout = unsafe { std::mem::zeroed() };
        unsafe { ffi::av_channel_layout_default(&mut layout, nb_channels) };
        Self(layout)
    }

    pub fn from_mask(mask: u64) -> Option<Self> {
        let mut layout: ffi::AVChannelLayout = unsafe { std::mem::zeroed() };
        (unsafe { ffi::av_channel_layout_from_mask(&mut layout, mask) } >= 0)
            .then_some(Self(layout))
    }

    /// Hand the layout to FFmpeg; the caller becomes responsible for it.
    pub fn into_inner(self) -> ffi::AVChannelLayout {
        let layout = self.0;
        std::mem::forget(self);
        layout
    }

    pub fn copy(&mut self, src: &ffi::AVChannelLayout) {
        unsafe { ffi::av_channel_layout_copy(&mut self.0, src) };
    }

    pub fn describe(&self) -> Result<CString> {
        describe_layout(&self.0)
    }
}

impl Clone for AVChannelLayout {
    fn clone(&self) -> Self {
        let mut layout: ffi::AVChannelLayout = unsafe { std::mem::zeroed() };
        unsafe { ffi::av_channel_layout_copy(&mut layout, &self.0) };
        Self(layout)
    }
}

impl Drop for AVChannelLayout {
    fn drop(&mut self) {
        unsafe { ffi::av_channel_layout_uninit(&mut self.0) };
    }
}

impl Deref for AVChannelLayout {
    type Target = ffi::AVChannelLayout;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Borrowed `AVChannelLayout`.
pub struct AVChannelLayoutRef<'a>(pub &'a ffi::AVChannelLayout);

impl AVChannelLayoutRef<'_> {
    pub fn describe(&self) -> Result<CString> {
        describe_layout(self.0)
    }

    /// Deep-copy into an owned layout.
    pub fn to_owned(&self) -> AVChannelLayout {
        let mut layout: ffi::AVChannelLayout = unsafe { std::mem::zeroed() };
        unsafe { ffi::av_channel_layout_copy(&mut layout, self.0) };
        AVChannelLayout(layout)
    }
}

impl Deref for AVChannelLayoutRef<'_> {
    type Target = ffi::AVChannelLayout;
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

fn describe_layout(layout: &ffi::AVChannelLayout) -> Result<CString> {
    let mut buf = vec![0u8; 128];
    let n = check(unsafe {
        ffi::av_channel_layout_describe(layout, buf.as_mut_ptr() as *mut _, buf.len())
    })?;
    buf.truncate(n as usize);
    Ok(CString::new(buf).unwrap_or_default())
}

// ---------------------------------------------------------------------------
// Dictionaries
// ---------------------------------------------------------------------------

/// Owned `AVDictionary`.
#[repr(transparent)]
pub struct AVDictionary {
    ptr: NonNull<ffi::AVDictionary>,
}

impl AVDictionary {
    pub fn new(key: &CStr, value: &CStr, flags: u32) -> Self {
        let mut dict: *mut ffi::AVDictionary = ptr::null_mut();
        unsafe { ffi::av_dict_set(&mut dict, key.as_ptr(), value.as_ptr(), flags as c_int) };
        Self {
            ptr: NonNull::new(dict).expect("av_dict_set"),
        }
    }

    pub fn new_int(key: &CStr, value: i64, flags: u32) -> Self {
        let mut dict: *mut ffi::AVDictionary = ptr::null_mut();
        unsafe { ffi::av_dict_set_int(&mut dict, key.as_ptr(), value, flags as c_int) };
        Self {
            ptr: NonNull::new(dict).expect("av_dict_set_int"),
        }
    }

    /// # Safety
    /// `ptr` must be an unowned, non-empty dictionary.
    pub unsafe fn from_raw(ptr: NonNull<ffi::AVDictionary>) -> Self {
        Self { ptr }
    }

    pub fn into_raw(self) -> NonNull<ffi::AVDictionary> {
        let ptr = self.ptr;
        std::mem::forget(self);
        ptr
    }

    pub fn as_ptr(&self) -> *const ffi::AVDictionary {
        self.ptr.as_ptr()
    }

    pub fn as_mut_ptr(&mut self) -> *mut ffi::AVDictionary {
        self.ptr.as_ptr()
    }

    pub fn set(mut self, key: &CStr, value: &CStr, flags: u32) -> Self {
        let mut dict = self.ptr.as_ptr();
        unsafe { ffi::av_dict_set(&mut dict, key.as_ptr(), value.as_ptr(), flags as c_int) };
        self.ptr = NonNull::new(dict).expect("av_dict_set");
        self
    }

    pub fn set_int(mut self, key: &CStr, value: i64, flags: u32) -> Self {
        let mut dict = self.ptr.as_ptr();
        unsafe { ffi::av_dict_set_int(&mut dict, key.as_ptr(), value, flags as c_int) };
        self.ptr = NonNull::new(dict).expect("av_dict_set_int");
        self
    }

    pub fn copy(mut self, another: &AVDictionary, flags: u32) -> Self {
        let mut dict = self.ptr.as_ptr();
        unsafe { ffi::av_dict_copy(&mut dict, another.as_ptr(), flags as c_int) };
        self.ptr = NonNull::new(dict).expect("av_dict_copy");
        self
    }

    pub fn get(
        &self,
        key: &CStr,
        prev: Option<AVDictionaryEntryRef<'_>>,
        flags: u32,
    ) -> Option<AVDictionaryEntryRef<'_>> {
        let prev_ptr = prev.map_or(ptr::null(), |e| e.as_ptr());
        let entry =
            unsafe { ffi::av_dict_get(self.as_ptr(), key.as_ptr(), prev_ptr, flags as c_int) };
        NonNull::new(entry).map(|p| unsafe { AVDictionaryEntryRef::from_raw(p) })
    }

    pub fn iter(&self) -> AVDictionaryIter<'_> {
        AVDictionaryIter {
            dict: self,
            ptr: ptr::null(),
        }
    }

    pub fn count(&self) -> usize {
        unsafe { ffi::av_dict_count(self.as_ptr()) as usize }
    }
}

impl Clone for AVDictionary {
    fn clone(&self) -> Self {
        let mut dict: *mut ffi::AVDictionary = ptr::null_mut();
        unsafe { ffi::av_dict_copy(&mut dict, self.as_ptr(), 0) };
        Self {
            ptr: NonNull::new(dict).expect("av_dict_copy of a non-empty dictionary"),
        }
    }
}

impl Drop for AVDictionary {
    fn drop(&mut self) {
        let mut ptr = self.ptr.as_ptr();
        unsafe { ffi::av_dict_free(&mut ptr) };
    }
}

impl fmt::Debug for AVDictionary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(
                self.iter()
                    .map(|e| (e.key().to_owned(), e.value().to_owned())),
            )
            .finish()
    }
}

/// Borrowed `AVDictionary` owned by a stream or context.
#[repr(transparent)]
pub struct AVDictionaryRef<'a> {
    inner: std::mem::ManuallyDrop<AVDictionary>,
    _marker: PhantomData<&'a ffi::AVDictionary>,
}

impl<'a> AVDictionaryRef<'a> {
    /// # Safety
    /// `ptr` must stay valid for `'a`.
    pub unsafe fn from_raw(ptr: NonNull<ffi::AVDictionary>) -> Self {
        Self {
            inner: std::mem::ManuallyDrop::new(unsafe { AVDictionary::from_raw(ptr) }),
            _marker: PhantomData,
        }
    }
}

impl Deref for AVDictionaryRef<'_> {
    type Target = AVDictionary;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

pub struct AVDictionaryIter<'a> {
    dict: &'a AVDictionary,
    ptr: *const ffi::AVDictionaryEntry,
}

impl<'a> Iterator for AVDictionaryIter<'a> {
    type Item = AVDictionaryEntryRef<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        self.ptr = unsafe { ffi::av_dict_iterate(self.dict.as_ptr(), self.ptr) };
        NonNull::new(self.ptr as *mut _).map(|p| unsafe { AVDictionaryEntryRef::from_raw(p) })
    }
}

#[derive(Clone, Copy)]
pub struct AVDictionaryEntryRef<'a> {
    ptr: NonNull<ffi::AVDictionaryEntry>,
    _marker: PhantomData<&'a ffi::AVDictionaryEntry>,
}

impl<'a> AVDictionaryEntryRef<'a> {
    /// # Safety
    /// `ptr` must stay valid for `'a`.
    pub unsafe fn from_raw(ptr: NonNull<ffi::AVDictionaryEntry>) -> Self {
        Self {
            ptr,
            _marker: PhantomData,
        }
    }

    pub fn as_ptr(&self) -> *const ffi::AVDictionaryEntry {
        self.ptr.as_ptr()
    }

    pub fn key(&self) -> &'a CStr {
        unsafe { CStr::from_ptr((*self.ptr.as_ptr()).key) }
    }

    pub fn value(&self) -> &'a CStr {
        unsafe { CStr::from_ptr((*self.ptr.as_ptr()).value) }
    }
}

// ---------------------------------------------------------------------------
// Frames
// ---------------------------------------------------------------------------

/// Owned `AVFrame`.
#[repr(transparent)]
pub struct AVFrame {
    ptr: NonNull<ffi::AVFrame>,
}

impl AVFrame {
    pub fn new() -> Self {
        Self {
            ptr: NonNull::new(unsafe { ffi::av_frame_alloc() }).expect("av_frame_alloc"),
        }
    }

    /// # Safety
    /// `ptr` must be an allocated, unowned frame.
    pub unsafe fn from_raw(ptr: NonNull<ffi::AVFrame>) -> Self {
        Self { ptr }
    }

    pub fn as_ptr(&self) -> *const ffi::AVFrame {
        self.ptr.as_ptr()
    }

    pub fn as_mut_ptr(&mut self) -> *mut ffi::AVFrame {
        self.ptr.as_ptr()
    }

    pub fn into_raw(self) -> NonNull<ffi::AVFrame> {
        let ptr = self.ptr;
        std::mem::forget(self);
        ptr
    }

    pub fn is_allocated(&self) -> bool {
        !(self.data[0].is_null() && self.buf[0].is_null())
    }

    /// Allocate buffers for the configured format and size; refuses to leak an
    /// already-allocated frame.
    pub fn alloc_buffer(&mut self) -> Result<()> {
        if self.is_allocated() {
            return Err(FfmpegError::AVFrameDoubleAllocatingError);
        }
        let ret = unsafe { ffi::av_frame_get_buffer(self.as_mut_ptr(), 0) };
        if ret < 0 {
            return Err(FfmpegError::AVFrameInvalidAllocatingError(ret));
        }
        Ok(())
    }

    pub fn get_buffer(&mut self, align: c_int) -> Result<()> {
        check(unsafe { ffi::av_frame_get_buffer(self.as_mut_ptr(), align) })?;
        Ok(())
    }

    pub fn data_mut(&mut self) -> &mut [*mut u8; 8] {
        unsafe { &mut (*self.as_mut_ptr()).data }
    }

    pub fn linesize_mut(&mut self) -> &mut [c_int; 8] {
        unsafe { &mut (*self.as_mut_ptr()).linesize }
    }

    pub fn ch_layout(&self) -> AVChannelLayoutRef<'_> {
        AVChannelLayoutRef(&self.deref().ch_layout)
    }

    pub fn make_writable(&mut self) -> Result<()> {
        check(unsafe { ffi::av_frame_make_writable(self.as_mut_ptr()) })?;
        Ok(())
    }

    pub fn is_writable(&self) -> bool {
        unsafe { ffi::av_frame_is_writable(self.as_ptr() as *mut _) > 0 }
    }

    pub fn hwframe_transfer_data(&mut self, src: &AVFrame) -> Result<()> {
        check(unsafe { ffi::av_hwframe_transfer_data(self.as_mut_ptr(), src.as_ptr(), 0) })?;
        Ok(())
    }

    pub fn unref(&mut self) {
        unsafe { ffi::av_frame_unref(self.as_mut_ptr()) };
    }
}

impl Default for AVFrame {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for AVFrame {
    fn clone(&self) -> Self {
        Self {
            ptr: NonNull::new(unsafe { ffi::av_frame_clone(self.as_ptr()) })
                .expect("av_frame_clone"),
        }
    }
}

impl Drop for AVFrame {
    fn drop(&mut self) {
        let mut ptr = self.ptr.as_ptr();
        unsafe { ffi::av_frame_free(&mut ptr) };
    }
}

impl Deref for AVFrame {
    type Target = ffi::AVFrame;
    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.as_ref() }
    }
}

impl DerefMut for AVFrame {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.ptr.as_mut() }
    }
}

impl fmt::Debug for AVFrame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AVFrame")
            .field("width", &self.width)
            .field("height", &self.height)
            .field("pts", &self.pts)
            .field("pict_type", &self.pict_type)
            .field("nb_samples", &self.nb_samples)
            .field("format", &self.format)
            .field("sample_rate", &self.sample_rate)
            .finish()
    }
}

setters!(AVFrame =>
    width: c_int,
    height: c_int,
    pts: i64,
    pkt_dts: i64,
    duration: i64,
    time_base: ffi::AVRational,
    pict_type: ffi::AVPictureType,
    nb_samples: c_int,
    format: c_int,
    ch_layout: ffi::AVChannelLayout,
    sample_rate: c_int,
    sample_aspect_ratio: ffi::AVRational,
    flags: c_int,
);

// ---------------------------------------------------------------------------
// Packets
// ---------------------------------------------------------------------------

/// Owned `AVPacket`.
#[repr(transparent)]
pub struct AVPacket {
    ptr: NonNull<ffi::AVPacket>,
}

impl AVPacket {
    pub fn new() -> Self {
        Self {
            ptr: NonNull::new(unsafe { ffi::av_packet_alloc() }).expect("av_packet_alloc"),
        }
    }

    /// # Safety
    /// `ptr` must be an allocated, unowned packet.
    pub unsafe fn from_raw(ptr: NonNull<ffi::AVPacket>) -> Self {
        Self { ptr }
    }

    pub fn as_ptr(&self) -> *const ffi::AVPacket {
        self.ptr.as_ptr()
    }

    pub fn as_mut_ptr(&mut self) -> *mut ffi::AVPacket {
        self.ptr.as_ptr()
    }

    pub fn into_raw(self) -> NonNull<ffi::AVPacket> {
        let ptr = self.ptr;
        std::mem::forget(self);
        ptr
    }

    pub fn rescale_ts(&mut self, from: ffi::AVRational, to: ffi::AVRational) {
        unsafe { ffi::av_packet_rescale_ts(self.as_mut_ptr(), from, to) };
    }

    pub fn unref(&mut self) {
        unsafe { ffi::av_packet_unref(self.as_mut_ptr()) };
    }
}

impl Default for AVPacket {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for AVPacket {
    fn clone(&self) -> Self {
        Self {
            ptr: NonNull::new(unsafe { ffi::av_packet_clone(self.as_ptr()) })
                .expect("av_packet_clone"),
        }
    }
}

impl Drop for AVPacket {
    fn drop(&mut self) {
        let mut ptr = self.ptr.as_ptr();
        unsafe { ffi::av_packet_free(&mut ptr) };
    }
}

impl Deref for AVPacket {
    type Target = ffi::AVPacket;
    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.as_ref() }
    }
}

impl DerefMut for AVPacket {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.ptr.as_mut() }
    }
}

impl fmt::Debug for AVPacket {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AVPacket")
            .field("stream_index", &self.stream_index)
            .field("pts", &self.pts)
            .field("dts", &self.dts)
            .field("duration", &self.duration)
            .field("size", &self.size)
            .field("flags", &self.flags)
            .finish()
    }
}

setters!(AVPacket =>
    pts: i64,
    dts: i64,
    stream_index: c_int,
    flags: c_int,
    duration: i64,
    pos: i64,
    time_base: ffi::AVRational,
);

// ---------------------------------------------------------------------------
// Subtitles
// ---------------------------------------------------------------------------

/// Owned `AVSubtitle`; rects are freed on drop.
pub struct AVSubtitle {
    inner: OwnedSubtitle,
}

impl AVSubtitle {
    pub fn new() -> Self {
        Self {
            inner: OwnedSubtitle::new(),
        }
    }

    pub fn as_ptr(&self) -> *const ffi::AVSubtitle {
        unsafe { self.inner.as_ptr() }
    }

    pub fn as_mut_ptr(&mut self) -> *mut ffi::AVSubtitle {
        unsafe { self.inner.as_mut_ptr() }
    }
}

impl Default for AVSubtitle {
    fn default() -> Self {
        Self::new()
    }
}

impl Deref for AVSubtitle {
    type Target = ffi::AVSubtitle;
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.as_ptr() }
    }
}

impl DerefMut for AVSubtitle {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.as_mut_ptr() }
    }
}

// ---------------------------------------------------------------------------
// Codec contexts
// ---------------------------------------------------------------------------

/// Owned `AVCodecContext`, backed by [`ffmpeg_next::codec::Context`].
pub struct AVCodecContext {
    inner: codec::Context,
}

impl AVCodecContext {
    pub fn new(codec: &AVCodecRef<'_>) -> Self {
        let codec = unsafe { ffmpeg_next::Codec::wrap(codec.as_ptr()) };
        Self {
            inner: codec::Context::new_with_codec(codec),
        }
    }

    pub fn as_ptr(&self) -> *const ffi::AVCodecContext {
        unsafe { self.inner.as_ptr() }
    }

    pub fn as_mut_ptr(&mut self) -> *mut ffi::AVCodecContext {
        unsafe { self.inner.as_mut_ptr() }
    }

    pub fn codec(&self) -> Option<AVCodecRef<'_>> {
        NonNull::new(self.deref().codec as *mut ffi::AVCodec)
            .map(|p| unsafe { AVCodecRef::from_raw(p) })
    }

    /// Open the codec. Unconsumed options come back in the result.
    pub fn open(&mut self, dict: Option<AVDictionary>) -> Result<Option<AVDictionary>> {
        let mut dict_ptr = dict.map_or(ptr::null_mut(), |d| d.into_raw().as_ptr());
        let ret = unsafe { ffi::avcodec_open2(self.as_mut_ptr(), ptr::null(), &mut dict_ptr) };
        let leftover = NonNull::new(dict_ptr).map(|p| unsafe { AVDictionary::from_raw(p) });
        check(ret)?;
        Ok(leftover)
    }

    pub fn send_packet(&mut self, packet: Option<&AVPacket>) -> Result<()> {
        let ptr = packet.map_or(ptr::null(), |p| p.as_ptr());
        let ret = unsafe { ffi::avcodec_send_packet(self.as_mut_ptr(), ptr) };
        match ret {
            r if r >= 0 => Ok(()),
            r if r == averror_eagain() => Err(FfmpegError::DecoderFullError),
            ffi::AVERROR_EOF => Err(FfmpegError::DecoderFlushedError),
            r => Err(FfmpegError::SendPacketError(r)),
        }
    }

    pub fn receive_frame(&mut self) -> Result<AVFrame> {
        let mut frame = AVFrame::new();
        let ret = unsafe { ffi::avcodec_receive_frame(self.as_mut_ptr(), frame.as_mut_ptr()) };
        match ret {
            r if r >= 0 => Ok(frame),
            r if r == averror_eagain() => Err(FfmpegError::DecoderDrainError),
            ffi::AVERROR_EOF => Err(FfmpegError::DecoderFlushedError),
            r => Err(FfmpegError::ReceiveFrameError(r)),
        }
    }

    pub fn send_frame(&mut self, frame: Option<&AVFrame>) -> Result<()> {
        let ptr = frame.map_or(ptr::null(), |f| f.as_ptr());
        let ret = unsafe { ffi::avcodec_send_frame(self.as_mut_ptr(), ptr) };
        match ret {
            r if r >= 0 => Ok(()),
            r if r == averror_eagain() => Err(FfmpegError::SendFrameAgainError),
            ffi::AVERROR_EOF => Err(FfmpegError::EncoderFlushedError),
            r => Err(FfmpegError::SendFrameError(r)),
        }
    }

    pub fn receive_packet(&mut self) -> Result<AVPacket> {
        let mut packet = AVPacket::new();
        let ret = unsafe { ffi::avcodec_receive_packet(self.as_mut_ptr(), packet.as_mut_ptr()) };
        match ret {
            r if r >= 0 => Ok(packet),
            r if r == averror_eagain() => Err(FfmpegError::EncoderDrainError),
            ffi::AVERROR_EOF => Err(FfmpegError::EncoderFlushedError),
            r => Err(FfmpegError::ReceivePacketError(r)),
        }
    }

    /// Decode one subtitle packet; `None` flushes. Returns `Ok(None)` when the
    /// packet produced no subtitle.
    pub fn decode_subtitle(&mut self, packet: Option<&mut AVPacket>) -> Result<Option<AVSubtitle>> {
        let mut subtitle = AVSubtitle::new();
        let mut got: c_int = 0;
        let empty;
        let pkt_ptr: *const ffi::AVPacket = match packet {
            Some(p) => p.as_ptr(),
            None => {
                empty = AVPacket::new();
                empty.as_ptr()
            }
        };
        check(unsafe {
            ffi::avcodec_decode_subtitle2(
                self.as_mut_ptr(),
                subtitle.as_mut_ptr(),
                &mut got,
                pkt_ptr,
            )
        })?;
        Ok((got != 0).then_some(subtitle))
    }

    pub fn encode_subtitle(&mut self, subtitle: &AVSubtitle, buf: &mut [u8]) -> Result<()> {
        check(unsafe {
            ffi::avcodec_encode_subtitle(
                self.as_mut_ptr(),
                buf.as_mut_ptr(),
                buf.len() as c_int,
                subtitle.as_ptr(),
            )
        })?;
        Ok(())
    }

    pub fn apply_codecpar(&mut self, codecpar: &AVCodecParameters) -> Result<()> {
        check(unsafe { ffi::avcodec_parameters_to_context(self.as_mut_ptr(), codecpar.as_ptr()) })?;
        Ok(())
    }

    pub fn extract_codecpar(&self) -> AVCodecParameters {
        let mut params = AVCodecParameters::new();
        params.copy_from_context(self);
        params
    }

    pub fn ch_layout(&self) -> AVChannelLayoutRef<'_> {
        AVChannelLayoutRef(&self.deref().ch_layout)
    }

    pub fn flush_buffers(&mut self) {
        unsafe { ffi::avcodec_flush_buffers(self.as_mut_ptr()) };
    }

    pub fn is_hwaccel(&self) -> bool {
        !self.hw_device_ctx.is_null() || !self.hw_frames_ctx.is_null()
    }
}

impl Deref for AVCodecContext {
    type Target = ffi::AVCodecContext;
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.as_ptr() }
    }
}

impl DerefMut for AVCodecContext {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.as_mut_ptr() }
    }
}

impl fmt::Debug for AVCodecContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AVCodecContext")
            .field("codec", &self.codec().map(|c| c.name().to_owned()))
            .field("width", &self.width)
            .field("height", &self.height)
            .field("pix_fmt", &self.pix_fmt)
            .field("sample_rate", &self.sample_rate)
            .field("sample_fmt", &self.sample_fmt)
            .field("time_base", &(self.time_base.num, self.time_base.den))
            .finish()
    }
}

setters!(AVCodecContext =>
    framerate: ffi::AVRational,
    ch_layout: ffi::AVChannelLayout,
    height: c_int,
    width: c_int,
    sample_aspect_ratio: ffi::AVRational,
    pix_fmt: ffi::AVPixelFormat,
    time_base: ffi::AVRational,
    pkt_timebase: ffi::AVRational,
    sample_rate: c_int,
    sample_fmt: ffi::AVSampleFormat,
    flags: c_int,
    flags2: c_int,
    bit_rate: i64,
    rc_max_rate: i64,
    rc_buffer_size: c_int,
    strict_std_compliance: c_int,
    gop_size: c_int,
    max_b_frames: c_int,
    profile: c_int,
    level: c_int,
    thread_count: c_int,
    thread_type: c_int,
    qmin: c_int,
    qmax: c_int,
    global_quality: c_int,
    compression_level: c_int,
    colorspace: ffi::AVColorSpace,
    color_range: ffi::AVColorRange,
    color_primaries: ffi::AVColorPrimaries,
    color_trc: ffi::AVColorTransferCharacteristic,
    chroma_sample_location: ffi::AVChromaLocation,
    codec_tag: u32,
    get_format: Option<unsafe extern "C" fn(*mut ffi::AVCodecContext, *const ffi::AVPixelFormat) -> ffi::AVPixelFormat>,
);

// ---------------------------------------------------------------------------
// Streams
// ---------------------------------------------------------------------------

/// Borrowed `AVStream` owned by a format context.
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct AVStreamRef<'a> {
    ptr: NonNull<ffi::AVStream>,
    _marker: PhantomData<&'a ffi::AVStream>,
}

impl<'a> AVStreamRef<'a> {
    /// # Safety
    /// `ptr` must stay valid for `'a`.
    pub unsafe fn from_raw(ptr: NonNull<ffi::AVStream>) -> Self {
        Self {
            ptr,
            _marker: PhantomData,
        }
    }

    pub fn as_ptr(&self) -> *const ffi::AVStream {
        self.ptr.as_ptr()
    }

    pub fn codecpar(&self) -> AVCodecParametersRef<'a> {
        unsafe { AVCodecParametersRef::from_raw(NonNull::new(self.codecpar).expect("codecpar")) }
    }

    pub fn metadata(&self) -> Option<AVDictionaryRef<'a>> {
        NonNull::new(self.metadata).map(|p| unsafe { AVDictionaryRef::from_raw(p) })
    }

    /// Guess the frame rate from container and codec information.
    pub fn guess_framerate(&self) -> Option<ffi::AVRational> {
        Some(unsafe {
            ffi::av_guess_frame_rate(ptr::null_mut(), self.as_ptr() as *mut _, ptr::null_mut())
        })
    }
}

impl Deref for AVStreamRef<'_> {
    type Target = ffi::AVStream;
    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.as_ref() }
    }
}

/// Mutably borrowed `AVStream` owned by a format context.
#[repr(transparent)]
pub struct AVStreamMut<'a> {
    ptr: NonNull<ffi::AVStream>,
    _marker: PhantomData<&'a mut ffi::AVStream>,
}

impl<'a> AVStreamMut<'a> {
    /// # Safety
    /// `ptr` must stay valid and unaliased for `'a`.
    pub unsafe fn from_raw(ptr: NonNull<ffi::AVStream>) -> Self {
        Self {
            ptr,
            _marker: PhantomData,
        }
    }

    pub fn as_ptr(&self) -> *const ffi::AVStream {
        self.ptr.as_ptr()
    }

    pub fn as_mut_ptr(&mut self) -> *mut ffi::AVStream {
        self.ptr.as_ptr()
    }

    pub fn codecpar(&self) -> AVCodecParametersRef<'_> {
        unsafe { AVCodecParametersRef::from_raw(NonNull::new(self.codecpar).expect("codecpar")) }
    }

    pub fn codecpar_mut(&mut self) -> AVCodecParametersMut<'_> {
        unsafe { AVCodecParametersMut::from_raw(NonNull::new(self.codecpar).expect("codecpar")) }
    }

    pub fn set_codecpar(&mut self, parameters: AVCodecParameters) {
        unsafe { ffi::avcodec_parameters_copy(self.codecpar, parameters.as_ptr()) };
    }

    pub fn metadata(&self) -> Option<AVDictionaryRef<'_>> {
        NonNull::new(self.metadata).map(|p| unsafe { AVDictionaryRef::from_raw(p) })
    }

    /// Replace the stream metadata; the stream owns the dictionary afterwards.
    pub fn set_metadata(&mut self, dict: Option<AVDictionary>) {
        unsafe {
            let mut old = (*self.as_mut_ptr()).metadata;
            ffi::av_dict_free(&mut old);
            (*self.as_mut_ptr()).metadata = dict.map_or(ptr::null_mut(), |d| d.into_raw().as_ptr());
        }
    }

    pub fn guess_framerate(&self) -> Option<ffi::AVRational> {
        Some(unsafe {
            ffi::av_guess_frame_rate(ptr::null_mut(), self.as_ptr() as *mut _, ptr::null_mut())
        })
    }
}

impl Deref for AVStreamMut<'_> {
    type Target = ffi::AVStream;
    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.as_ref() }
    }
}

impl DerefMut for AVStreamMut<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.ptr.as_mut() }
    }
}

setters!(AVStreamMut<'_> =>
    avg_frame_rate: ffi::AVRational,
    discard: ffi::AVDiscard,
    disposition: c_int,
    duration: i64,
    sample_aspect_ratio: ffi::AVRational,
    time_base: ffi::AVRational,
    id: c_int,
);

fn stream_slice<'a>(ctx: *const ffi::AVFormatContext) -> &'a [AVStreamRef<'a>] {
    // SAFETY: AVStreamRef is repr(transparent) over NonNull<AVStream>, and FFmpeg
    // never stores null entries in `streams`.
    unsafe {
        let ctx = &*ctx;
        std::slice::from_raw_parts(
            ctx.streams as *const AVStreamRef<'a>,
            ctx.nb_streams as usize,
        )
    }
}

fn stream_slice_mut<'a>(ctx: *mut ffi::AVFormatContext) -> &'a mut [AVStreamMut<'a>] {
    unsafe {
        let ctx = &mut *ctx;
        std::slice::from_raw_parts_mut(ctx.streams as *mut AVStreamMut<'a>, ctx.nb_streams as usize)
    }
}

// ---------------------------------------------------------------------------
// Format contexts
// ---------------------------------------------------------------------------

/// Borrowed demuxer descriptor.
#[derive(Clone, Copy)]
pub struct AVInputFormatRef<'a>(&'a ffi::AVInputFormat);

impl AVInputFormatRef<'_> {
    pub fn name(&self) -> &CStr {
        unsafe { CStr::from_ptr(self.0.name) }
    }

    pub fn long_name(&self) -> &CStr {
        unsafe { CStr::from_ptr(self.0.long_name) }
    }
}

impl Deref for AVInputFormatRef<'_> {
    type Target = ffi::AVInputFormat;
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

/// Borrowed muxer descriptor.
#[derive(Clone, Copy)]
pub struct AVOutputFormatRef<'a>(&'a ffi::AVOutputFormat);

impl AVOutputFormatRef<'_> {
    pub fn name(&self) -> &CStr {
        unsafe { CStr::from_ptr(self.0.name) }
    }

    pub fn long_name(&self) -> &CStr {
        unsafe { CStr::from_ptr(self.0.long_name) }
    }
}

impl Deref for AVOutputFormatRef<'_> {
    type Target = ffi::AVOutputFormat;
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

/// Opened demuxer, backed by [`ffmpeg_next::format::context::Input`].
pub struct AVFormatContextInput {
    inner: format::context::Input,
}

impl AVFormatContextInput {
    /// Open `url` and read stream info.
    pub fn open(url: &CStr) -> Result<Self> {
        let inner = format::input(cstr_path(url))
            .map_err(|e| FfmpegError::OpenInputError(c_int::from(e)))?;
        Ok(Self { inner })
    }

    pub fn as_ptr(&self) -> *const ffi::AVFormatContext {
        unsafe { self.inner.as_ptr() }
    }

    pub fn as_mut_ptr(&mut self) -> *mut ffi::AVFormatContext {
        unsafe { self.inner.as_mut_ptr() }
    }

    /// Next packet, or `None` at end of stream.
    pub fn read_packet(&mut self) -> Result<Option<AVPacket>> {
        let mut packet = AVPacket::new();
        match unsafe { ffi::av_read_frame(self.as_mut_ptr(), packet.as_mut_ptr()) } {
            r if r >= 0 => Ok(Some(packet)),
            ffi::AVERROR_EOF => Ok(None),
            r => Err(FfmpegError::AVError(r)),
        }
    }

    pub fn seek(&mut self, stream_index: c_int, timestamp: i64, flags: c_int) -> Result<()> {
        check(unsafe { ffi::av_seek_frame(self.as_mut_ptr(), stream_index, timestamp, flags) })?;
        Ok(())
    }

    pub fn streams(&self) -> &[AVStreamRef<'_>] {
        stream_slice(self.as_ptr())
    }

    pub fn streams_mut(&mut self) -> &mut [AVStreamMut<'_>] {
        stream_slice_mut(self.as_mut_ptr())
    }

    pub fn iformat(&self) -> AVInputFormatRef<'_> {
        AVInputFormatRef(unsafe { &*self.deref().iformat })
    }

    pub fn metadata(&self) -> Option<AVDictionaryRef<'_>> {
        NonNull::new(self.deref().metadata).map(|p| unsafe { AVDictionaryRef::from_raw(p) })
    }

    pub fn dump(&mut self, index: usize, filename: &CStr) {
        unsafe { ffi::av_dump_format(self.as_mut_ptr(), index as c_int, filename.as_ptr(), 0) };
    }
}

impl Deref for AVFormatContextInput {
    type Target = ffi::AVFormatContext;
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.as_ptr() }
    }
}

impl DerefMut for AVFormatContextInput {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.as_mut_ptr() }
    }
}

/// Muxer with its output opened, backed by [`ffmpeg_next::format::context::Output`].
pub struct AVFormatContextOutput {
    inner: format::context::Output,
}

impl AVFormatContextOutput {
    /// Create the muxer for `filename`, guessing the container from its name,
    /// and open the file.
    pub fn create(filename: &CStr) -> Result<Self> {
        let inner = format::output(cstr_path(filename))?;
        Ok(Self { inner })
    }

    pub fn as_ptr(&self) -> *const ffi::AVFormatContext {
        unsafe { self.inner.as_ptr() }
    }

    pub fn as_mut_ptr(&mut self) -> *mut ffi::AVFormatContext {
        unsafe { self.inner.as_mut_ptr() }
    }

    pub fn new_stream(&mut self) -> AVStreamMut<'_> {
        let stream = unsafe { ffi::avformat_new_stream(self.as_mut_ptr(), ptr::null()) };
        unsafe { AVStreamMut::from_raw(NonNull::new(stream).expect("avformat_new_stream")) }
    }

    /// Write the header. Options are consumed; anything FFmpeg did not use is
    /// handed back in `dict`, including on failure.
    pub fn write_header(&mut self, dict: &mut Option<AVDictionary>) -> Result<()> {
        let mut dict_ptr = dict
            .take()
            .map_or(ptr::null_mut(), |d| d.into_raw().as_ptr());
        let ret = unsafe { ffi::avformat_write_header(self.as_mut_ptr(), &mut dict_ptr) };
        *dict = NonNull::new(dict_ptr).map(|p| unsafe { AVDictionary::from_raw(p) });
        check(ret)?;
        Ok(())
    }

    pub fn write_trailer(&mut self) -> Result<()> {
        check(unsafe { ffi::av_write_trailer(self.as_mut_ptr()) })?;
        Ok(())
    }

    pub fn write_frame(&mut self, packet: &mut AVPacket) -> Result<()> {
        check(unsafe { ffi::av_write_frame(self.as_mut_ptr(), packet.as_mut_ptr()) })?;
        Ok(())
    }

    pub fn interleaved_write_frame(&mut self, packet: &mut AVPacket) -> Result<()> {
        check(unsafe { ffi::av_interleaved_write_frame(self.as_mut_ptr(), packet.as_mut_ptr()) })?;
        Ok(())
    }

    pub fn streams(&self) -> &[AVStreamRef<'_>] {
        stream_slice(self.as_ptr())
    }

    pub fn streams_mut(&mut self) -> &mut [AVStreamMut<'_>] {
        stream_slice_mut(self.as_mut_ptr())
    }

    pub fn oformat(&self) -> AVOutputFormatRef<'_> {
        AVOutputFormatRef(unsafe { &*self.deref().oformat })
    }

    pub fn dump(&mut self, index: c_int, filename: &CStr) {
        unsafe { ffi::av_dump_format(self.as_mut_ptr(), index, filename.as_ptr(), 1) };
    }
}

impl Deref for AVFormatContextOutput {
    type Target = ffi::AVFormatContext;
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.as_ptr() }
    }
}

impl DerefMut for AVFormatContextOutput {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.as_mut_ptr() }
    }
}

// ---------------------------------------------------------------------------
// Audio helpers
// ---------------------------------------------------------------------------

/// Owned `AVAudioFifo`.
pub struct AVAudioFifo {
    ptr: NonNull<ffi::AVAudioFifo>,
}

impl AVAudioFifo {
    pub fn new(sample_fmt: ffi::AVSampleFormat, channels: c_int, nb_samples: c_int) -> Self {
        Self {
            ptr: NonNull::new(unsafe {
                ffi::av_audio_fifo_alloc(sample_fmt, channels, nb_samples)
            })
            .expect("av_audio_fifo_alloc"),
        }
    }

    pub fn as_mut_ptr(&mut self) -> *mut ffi::AVAudioFifo {
        self.ptr.as_ptr()
    }

    pub fn realloc(&mut self, nb_samples: c_int) {
        unsafe { ffi::av_audio_fifo_realloc(self.as_mut_ptr(), nb_samples) };
    }

    /// # Safety
    /// `data` must point at `nb_samples` valid samples per plane.
    pub unsafe fn write(&mut self, data: *const *mut u8, nb_samples: c_int) -> Result<()> {
        check(unsafe {
            ffi::av_audio_fifo_write(self.as_mut_ptr(), data as *const *mut c_void, nb_samples)
        })?;
        Ok(())
    }

    /// # Safety
    /// `data` must have room for `nb_samples` samples per plane.
    pub unsafe fn peek(&mut self, data: *const *mut u8, nb_samples: c_int) -> Result<c_int> {
        check(unsafe {
            ffi::av_audio_fifo_peek(self.as_mut_ptr(), data as *const *mut c_void, nb_samples)
        })
    }

    /// # Safety
    /// `data` must have room for `nb_samples` samples per plane.
    pub unsafe fn read(&mut self, data: *const *mut u8, nb_samples: c_int) -> Result<c_int> {
        check(unsafe {
            ffi::av_audio_fifo_read(self.as_mut_ptr(), data as *const *mut c_void, nb_samples)
        })
    }

    pub fn drain(&mut self, nb_samples: c_int) {
        unsafe { ffi::av_audio_fifo_drain(self.as_mut_ptr(), nb_samples) };
    }

    pub fn reset(&mut self) {
        unsafe { ffi::av_audio_fifo_reset(self.as_mut_ptr()) };
    }

    pub fn size(&self) -> c_int {
        unsafe { ffi::av_audio_fifo_size(self.ptr.as_ptr()) }
    }

    pub fn space(&self) -> c_int {
        unsafe { ffi::av_audio_fifo_space(self.ptr.as_ptr()) }
    }
}

impl Drop for AVAudioFifo {
    fn drop(&mut self) {
        unsafe { ffi::av_audio_fifo_free(self.as_mut_ptr()) };
    }
}

/// A sample buffer with per-plane pointers, as `av_samples_alloc` lays it out.
/// `nb_samples` is the capacity.
pub struct AVSamples {
    _buffer: Box<[u8]>,
    pub audio_data: Box<[*mut u8]>,
    pub linesize: c_int,
    pub nb_channels: c_int,
    pub nb_samples: c_int,
    pub sample_fmt: ffi::AVSampleFormat,
    pub align: c_int,
}

impl AVSamples {
    /// Required `(linesize, buffer_size)` for the parameters, or `None` when invalid.
    pub fn get_buffer_size(
        nb_channels: c_int,
        nb_samples: c_int,
        sample_fmt: ffi::AVSampleFormat,
        align: c_int,
    ) -> Option<(c_int, c_int)> {
        let mut linesize = 0;
        let size = unsafe {
            ffi::av_samples_get_buffer_size(
                &mut linesize,
                nb_channels,
                nb_samples,
                sample_fmt,
                align,
            )
        };
        (size >= 0).then_some((linesize, size))
    }

    /// Allocate a zeroed buffer and plane pointers; `None` on invalid parameters.
    pub fn new(
        nb_channels: c_int,
        nb_samples: c_int,
        sample_fmt: ffi::AVSampleFormat,
        align: c_int,
    ) -> Option<Self> {
        let (_, buffer_size) = Self::get_buffer_size(nb_channels, nb_samples, sample_fmt, align)?;
        let buffer = vec![0u8; buffer_size as usize].into_boxed_slice();
        let planar = unsafe { ffi::av_sample_fmt_is_planar(sample_fmt) } != 0;
        let nb_planes = if planar { nb_channels } else { 1 };
        let mut audio_data = vec![ptr::null_mut::<u8>(); nb_planes as usize].into_boxed_slice();
        let mut linesize = 0;
        let ret = unsafe {
            ffi::av_samples_fill_arrays(
                audio_data.as_mut_ptr(),
                &mut linesize,
                buffer.as_ptr(),
                nb_channels,
                nb_samples,
                sample_fmt,
                align,
            )
        };
        if ret < 0 {
            return None;
        }
        Some(Self {
            _buffer: buffer,
            audio_data,
            linesize,
            nb_channels,
            nb_samples,
            sample_fmt,
            align,
        })
    }
}

/// Owned `SwrContext`.
pub struct SwrContext {
    ptr: NonNull<ffi::SwrContext>,
}

impl SwrContext {
    pub fn new(
        out_ch_layout: &ffi::AVChannelLayout,
        out_sample_fmt: ffi::AVSampleFormat,
        out_sample_rate: c_int,
        in_ch_layout: &ffi::AVChannelLayout,
        in_sample_fmt: ffi::AVSampleFormat,
        in_sample_rate: c_int,
    ) -> Result<Self> {
        let mut context: *mut ffi::SwrContext = ptr::null_mut();
        check(unsafe {
            ffi::swr_alloc_set_opts2(
                &mut context,
                out_ch_layout,
                out_sample_fmt,
                out_sample_rate,
                in_ch_layout,
                in_sample_fmt,
                in_sample_rate,
                0,
                ptr::null_mut(),
            )
        })?;
        Ok(Self {
            ptr: NonNull::new(context).ok_or(FfmpegError::Unknown)?,
        })
    }

    pub fn as_mut_ptr(&mut self) -> *mut ffi::SwrContext {
        self.ptr.as_ptr()
    }

    pub fn is_initialized(&self) -> bool {
        unsafe { ffi::swr_is_initialized(self.ptr.as_ptr()) > 0 }
    }

    pub fn init(&mut self) -> Result<()> {
        check(unsafe { ffi::swr_init(self.as_mut_ptr()) })?;
        Ok(())
    }

    pub fn get_out_samples(&self, in_samples: c_int) -> c_int {
        unsafe { ffi::swr_get_out_samples(self.ptr.as_ptr(), in_samples) }
    }

    pub fn get_delay(&self, base: usize) -> usize {
        unsafe { ffi::swr_get_delay(self.ptr.as_ptr(), base as i64) as usize }
    }

    /// Convert samples; returns samples produced per channel.
    ///
    /// # Safety
    /// Both buffers must match the configured layouts and counts.
    pub unsafe fn convert(
        &mut self,
        out_buffer: *mut *mut u8,
        out_count: c_int,
        in_buffer: *const *const u8,
        in_count: c_int,
    ) -> Result<c_int> {
        check(unsafe {
            ffi::swr_convert(
                self.as_mut_ptr(),
                out_buffer as *const *mut u8,
                out_count,
                in_buffer,
                in_count,
            )
        })
    }

    pub fn convert_frame(&mut self, input: Option<&AVFrame>, output: &mut AVFrame) -> Result<()> {
        let input = input.map_or(ptr::null(), |f| f.as_ptr());
        check(unsafe { ffi::swr_convert_frame(self.as_mut_ptr(), output.as_mut_ptr(), input) })?;
        Ok(())
    }
}

impl Drop for SwrContext {
    fn drop(&mut self) {
        let mut ptr = self.ptr.as_ptr();
        unsafe { ffi::swr_free(&mut ptr) };
    }
}

/// Owned `SwsContext`.
pub struct SwsContext {
    ptr: NonNull<ffi::SwsContext>,
}

impl SwsContext {
    #[allow(clippy::too_many_arguments)]
    pub fn get_context(
        src_w: c_int,
        src_h: c_int,
        src_format: ffi::AVPixelFormat,
        dst_w: c_int,
        dst_h: c_int,
        dst_format: ffi::AVPixelFormat,
        flags: c_int,
        src_filter: Option<&ffi::SwsFilter>,
        dst_filter: Option<&ffi::SwsFilter>,
        param: Option<&[f64; 2]>,
    ) -> Option<Self> {
        let context = unsafe {
            ffi::sws_getContext(
                src_w,
                src_h,
                src_format,
                dst_w,
                dst_h,
                dst_format,
                flags,
                src_filter.map_or(ptr::null_mut(), |f| f as *const _ as *mut _),
                dst_filter.map_or(ptr::null_mut(), |f| f as *const _ as *mut _),
                param.map_or(ptr::null(), |p| p.as_ptr()),
            )
        };
        NonNull::new(context).map(|ptr| Self { ptr })
    }

    pub fn as_mut_ptr(&mut self) -> *mut ffi::SwsContext {
        self.ptr.as_ptr()
    }

    /// # Safety
    /// Plane pointers and strides must describe valid images.
    #[allow(clippy::too_many_arguments)]
    pub unsafe fn scale(
        &mut self,
        src_slice: *const *const u8,
        src_stride: *const c_int,
        src_slice_y: c_int,
        src_slice_h: c_int,
        dst: *const *mut u8,
        dst_stride: *const c_int,
    ) -> Result<()> {
        check(unsafe {
            ffi::sws_scale(
                self.as_mut_ptr(),
                src_slice,
                src_stride,
                src_slice_y,
                src_slice_h,
                dst,
                dst_stride,
            )
        })?;
        Ok(())
    }

    pub fn scale_frame(
        &mut self,
        src_frame: &AVFrame,
        src_slice_y: c_int,
        src_slice_h: c_int,
        dst_frame: &mut AVFrame,
    ) -> Result<()> {
        unsafe {
            self.scale(
                src_frame.data.as_ptr() as *const *const u8,
                src_frame.linesize.as_ptr(),
                src_slice_y,
                src_slice_h,
                dst_frame.data.as_ptr(),
                dst_frame.linesize.as_ptr(),
            )
        }
    }
}

impl Drop for SwsContext {
    fn drop(&mut self) {
        unsafe { ffi::sws_freeContext(self.as_mut_ptr()) };
    }
}
