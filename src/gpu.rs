use clap::ValueEnum;
use log::warn;
use rsmpeg::avcodec::{AVCodec, AVCodecRef};
use rsmpeg::ffi::{self};
use serde::Serialize;
use std::collections::HashMap;
use std::ffi::{c_void, CStr, CString};

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum)]
pub enum HwAccel {
    Auto,
    None,
    Nvenc,
    Vaapi,
    Qsv,
    Videotoolbox,
    Amf,
}

pub fn try_create_hw_device(device_name: &str) -> Option<*mut ffi::AVBufferRef> {
    unsafe {
        let c_name = CString::new(device_name).ok()?;
        let dev_type = ffi::av_hwdevice_find_type_by_name(c_name.as_ptr());
        if dev_type == ffi::AV_HWDEVICE_TYPE_NONE {
            return None;
        }
        let mut buf: *mut ffi::AVBufferRef = std::ptr::null_mut();
        let ret = ffi::av_hwdevice_ctx_create(
            &mut buf,
            dev_type,
            std::ptr::null(),
            std::ptr::null_mut(),
            0,
        );
        if ret >= 0 && !buf.is_null() {
            Some(buf)
        } else {
            None
        }
    }
}

pub fn find_hw_encoder(
    codec_id: ffi::AVCodecID,
    pref: HwAccel,
) -> (Option<AVCodecRef<'static>>, Option<*mut ffi::AVBufferRef>) {
    let encoders = encoder_candidates(codec_id);

    let filter = |name: &str| match pref {
        HwAccel::Auto => true,
        HwAccel::None => false,
        HwAccel::Nvenc => name.contains("nvenc"),
        HwAccel::Vaapi => name.contains("vaapi"),
        HwAccel::Qsv => name.contains("qsv"),
        HwAccel::Videotoolbox => name.contains("videotoolbox"),
        HwAccel::Amf => name.contains("amf"),
    };

    for (encoder_name, dev_types) in encoders.iter().copied() {
        if !filter(encoder_name) {
            continue;
        }
        if pref == HwAccel::Auto && encoder_name.contains("_amf") {
            warn!(
                "Skipping hardware encoder {} because AMD AMF currently fails to honor bitrate targets; falling back",
                encoder_name
            );
            continue;
        }
        let cname = CString::new(encoder_name).unwrap();
        if let Some(encoder) = AVCodec::find_encoder_by_name(cname.as_c_str()) {
            let mut device_ref: Option<*mut ffi::AVBufferRef> = None;
            let mut success = dev_types.is_empty();
            for dev in dev_types {
                if let Some(buf) = try_create_hw_device(dev) {
                    device_ref = Some(buf);
                    success = true;
                    break;
                }
            }
            if !success {
                continue;
            }
            return (Some(encoder), device_ref);
        }
    }
    (None, None)
}

pub fn encoder_candidates(
    codec_id: ffi::AVCodecID,
) -> &'static [(&'static str, &'static [&'static str])] {
    match codec_id {
        ffi::AV_CODEC_ID_H264 => h264_candidates(),
        ffi::AV_CODEC_ID_HEVC => hevc_candidates(),
        _ => &[],
    }
}

#[cfg(target_os = "macos")]
const fn h264_candidates() -> &'static [(&'static str, &'static [&'static str])] {
    &[
        ("h264_videotoolbox", &["videotoolbox"]),
        ("h264_nvenc", &["cuda"]),
        ("h264_qsv", &["qsv"]),
        ("h264_vaapi", &["vaapi"]),
        ("h264_amf", &["d3d11va", "dxva2"]),
    ]
}
#[cfg(target_os = "macos")]
const fn hevc_candidates() -> &'static [(&'static str, &'static [&'static str])] {
    &[
        ("hevc_videotoolbox", &["videotoolbox"]),
        ("hevc_nvenc", &["cuda"]),
        ("hevc_qsv", &["qsv"]),
        ("hevc_vaapi", &["vaapi"]),
        ("hevc_amf", &["d3d11va", "dxva2"]),
    ]
}

#[cfg(all(not(target_os = "macos"), target_os = "windows"))]
const fn h264_candidates() -> &'static [(&'static str, &'static [&'static str])] {
    &[
        ("h264_amf", &["d3d11va", "dxva2"]),
        ("h264_nvenc", &["cuda"]),
        ("h264_qsv", &["qsv"]),
    ]
}
#[cfg(all(not(target_os = "macos"), target_os = "windows"))]
const fn hevc_candidates() -> &'static [(&'static str, &'static [&'static str])] {
    &[
        ("hevc_amf", &["d3d11va", "dxva2"]),
        ("hevc_nvenc", &["cuda"]),
        ("hevc_qsv", &["qsv"]),
    ]
}

#[cfg(all(not(target_os = "macos"), not(target_os = "windows")))]
const fn h264_candidates() -> &'static [(&'static str, &'static [&'static str])] {
    &[
        ("h264_nvenc", &["cuda"]),
        ("h264_vaapi", &["vaapi"]),
        ("h264_qsv", &["qsv"]),
    ]
}
#[cfg(all(not(target_os = "macos"), not(target_os = "windows")))]
const fn hevc_candidates() -> &'static [(&'static str, &'static [&'static str])] {
    &[
        ("hevc_nvenc", &["cuda"]),
        ("hevc_vaapi", &["vaapi"]),
        ("hevc_qsv", &["qsv"]),
    ]
}

pub fn probe_hw_devices() -> HashMap<&'static str, bool> {
    let types: &[&str] = &[
        "cuda",
        "vaapi",
        "qsv",
        "videotoolbox",
        "d3d11va",
        "dxva2",
        "opencl",
        "vulkan",
        "drm",
        "mediacodec",
    ];
    let mut map = HashMap::new();
    for &t in types {
        if let Some(mut buf) = try_create_hw_device(t) {
            map.insert(t, true);
            unsafe { ffi::av_buffer_unref(&mut buf) };
        } else {
            map.insert(t, false);
        }
    }
    map
}

#[derive(Serialize)]
pub struct HwDeviceEntry {
    pub name: String,
    pub available: bool,
}

#[derive(Serialize)]
pub struct HwEncoderEntry {
    pub name: String,
    pub present: bool,
    pub required_devices: Vec<String>,
    pub available: bool,
}

pub fn probe_hw_encoders(device_ok: &HashMap<&'static str, bool>) -> Vec<HwEncoderEntry> {
    let encs: &[(&str, &[&str])] = &[
        ("h264_nvenc", &["cuda"]),
        ("hevc_nvenc", &["cuda"]),
        ("h264_qsv", &["qsv"]),
        ("hevc_qsv", &["qsv"]),
        ("h264_vaapi", &["vaapi"]),
        ("hevc_vaapi", &["vaapi"]),
        ("h264_videotoolbox", &["videotoolbox"]),
        ("hevc_videotoolbox", &["videotoolbox"]),
        ("h264_amf", &["d3d11va", "dxva2"]),
        ("hevc_amf", &["d3d11va", "dxva2"]),
    ];
    let mut out = Vec::new();
    for (name, devs) in encs {
        let cname = CString::new(*name).unwrap();
        let present = AVCodec::find_encoder_by_name(cname.as_c_str()).is_some();
        let usable = present
            && devs
                .iter()
                .any(|d| device_ok.get(d).copied().unwrap_or(false));
        out.push(HwEncoderEntry {
            name: name.to_string(),
            present,
            required_devices: devs.iter().map(|s| s.to_string()).collect(),
            available: usable,
        });
    }
    out
}

pub fn print_probe() {
    println!("Hardware Probe:\n");
    let devices = probe_hw_devices();
    println!("Devices (av_hwdevice_ctx_create):");
    for (name, ok) in devices.iter() {
        println!(
            "  - {:<13} {}",
            name,
            if *ok { "available" } else { "unavailable" }
        );
    }
    println!();
    let encs = probe_hw_encoders(&devices);
    println!("Encoders (present + device available):");
    for e in encs {
        let uses = e.required_devices.join(",");
        println!(
            "  - {:<20} {:<11} (devices: {})",
            e.name,
            if e.available {
                "available"
            } else {
                "unavailable"
            },
            uses
        );
    }
}

fn media_type_to_str(t: ffi::AVMediaType) -> &'static str {
    match t {
        ffi::AVMEDIA_TYPE_VIDEO => "video",
        ffi::AVMEDIA_TYPE_AUDIO => "audio",
        ffi::AVMEDIA_TYPE_SUBTITLE => "subtitle",
        ffi::AVMEDIA_TYPE_DATA => "data",
        _ => "other",
    }
}

pub fn print_probe_codecs(only_video: bool, only_hw: bool) {
    unsafe {
        let ver_codec = ffi::avcodec_version();
        let ver_fmt = ffi::avformat_version();
        let ver_util = ffi::avutil_version();
        let conf = CStr::from_ptr(ffi::avcodec_configuration());
        println!("FFmpeg Versions:");
        println!("  avcodec: {}", ver_codec);
        println!("  avformat: {}", ver_fmt);
        println!("  avutil: {}", ver_util);
        println!("Configuration:\n  {}\n", conf.to_string_lossy());

        let mut opaque: *mut c_void = std::ptr::null_mut();
        let mut enc_count = 0usize;
        let mut dec_count = 0usize;
        println!("Encoders:");
        loop {
            let codec = ffi::av_codec_iterate(&mut opaque as *mut _);
            if codec.is_null() {
                break;
            }
            if ffi::av_codec_is_encoder(codec) != 0 {
                enc_count += 1;
                let name = if !(*codec).name.is_null() {
                    CStr::from_ptr((*codec).name).to_string_lossy().into_owned()
                } else {
                    String::from("<unknown>")
                };
                let long_name = if !(*codec).long_name.is_null() {
                    CStr::from_ptr((*codec).long_name)
                        .to_string_lossy()
                        .into_owned()
                } else {
                    String::new()
                };
                let mt = media_type_to_str((*codec).type_);
                let is_video = mt == "video";
                let is_hw_encoder = name.contains("nvenc")
                    || name.contains("vaapi")
                    || name.contains("qsv")
                    || name.contains("videotoolbox")
                    || name.contains("amf");
                if (only_video && !is_video) || (only_hw && !is_hw_encoder) {
                    continue;
                }
                println!(
                    "  - {:<22} {:<40} [{}{}]",
                    name,
                    long_name,
                    mt,
                    if is_hw_encoder { ", hw" } else { "" }
                );
            }
        }

        opaque = std::ptr::null_mut();
        println!("\nDecoders (with HW configs):");
        let devices_map = probe_hw_devices();
        loop {
            let codec = ffi::av_codec_iterate(&mut opaque as *mut _);
            if codec.is_null() {
                break;
            }
            if ffi::av_codec_is_decoder(codec) != 0 {
                dec_count += 1;
                let name = if !(*codec).name.is_null() {
                    CStr::from_ptr((*codec).name).to_string_lossy().into_owned()
                } else {
                    String::from("<unknown>")
                };
                let long_name = if !(*codec).long_name.is_null() {
                    CStr::from_ptr((*codec).long_name)
                        .to_string_lossy()
                        .into_owned()
                } else {
                    String::new()
                };
                let mt = media_type_to_str((*codec).type_);
                let is_video = mt == "video";
                if !(only_video && !is_video) {
                    print!("  - {:<22} {:<40} [{}]", name, long_name, mt);
                }
                let mut i = 0;
                let mut hw_entries: Vec<String> = Vec::new();
                loop {
                    let cfg = ffi::avcodec_get_hw_config(codec, i);
                    if cfg.is_null() {
                        break;
                    }
                    let dtype = (*cfg).device_type;
                    let dname_ptr = ffi::av_hwdevice_get_type_name(dtype);
                    let dname = if !dname_ptr.is_null() {
                        CStr::from_ptr(dname_ptr).to_string_lossy().into_owned()
                    } else {
                        format!("type={}", dtype)
                    };
                    let available = devices_map.get::<str>(&dname).copied().unwrap_or(false);
                    hw_entries.push(format!("{}{}", dname, if available { "(ok)" } else { "" }));
                    i += 1;
                }
                let has_hw = !hw_entries.is_empty();
                if only_hw && !has_hw {
                    continue;
                }
                if only_video && !is_video {
                    continue;
                }
                if has_hw {
                    println!("  [hw: {}]", hw_entries.join(", "));
                } else {
                    println!("");
                }
            }
        }
        println!("\nSummary: encoders={}, decoders={}", enc_count, dec_count);
    }
}

#[derive(Serialize)]
pub struct CodecEntry {
    pub name: String,
    pub long_name: String,
    pub kind: String,
    pub media_type: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hw_devices: Option<Vec<String>>,
}

#[derive(Serialize)]
pub struct CodecProbeSummary {
    pub ffmpeg: serde_json::Value,
    pub devices: Vec<HwDeviceEntry>,
    pub hw_encoders: Vec<HwEncoderEntry>,
    pub encoders: Vec<CodecEntry>,
    pub decoders: Vec<CodecEntry>,
}

pub fn gather_probe_json(
    only_video: bool,
    only_hw: bool,
    include_hw: bool,
    include_codecs: bool,
) -> CodecProbeSummary {
    let devices_map = probe_hw_devices();
    let device_entries: Vec<HwDeviceEntry> = devices_map
        .iter()
        .map(|(k, v)| HwDeviceEntry {
            name: k.to_string(),
            available: *v,
        })
        .collect();
    let hw_encs = if include_hw {
        probe_hw_encoders(&devices_map)
    } else {
        Vec::new()
    };
    unsafe {
        let ver = serde_json::json!({
            "avcodec": ffi::avcodec_version(),
            "avformat": ffi::avformat_version(),
            "avutil": ffi::avutil_version(),
            "configuration": CStr::from_ptr(ffi::avcodec_configuration()).to_string_lossy(),
        });
        let mut encoders: Vec<CodecEntry> = Vec::new();
        let mut decoders: Vec<CodecEntry> = Vec::new();
        if include_codecs {
            let mut opaque: *mut c_void = std::ptr::null_mut();
            loop {
                let codec = ffi::av_codec_iterate(&mut opaque as *mut _);
                if codec.is_null() {
                    break;
                }
                if ffi::av_codec_is_encoder(codec) == 0 {
                    continue;
                }
                let name = if !(*codec).name.is_null() {
                    CStr::from_ptr((*codec).name).to_string_lossy().into_owned()
                } else {
                    String::from("<unknown>")
                };
                let long_name = if !(*codec).long_name.is_null() {
                    CStr::from_ptr((*codec).long_name)
                        .to_string_lossy()
                        .into_owned()
                } else {
                    String::new()
                };
                let mt = media_type_to_str((*codec).type_).to_string();
                let is_video = mt == "video";
                let is_hw = name.contains("nvenc")
                    || name.contains("vaapi")
                    || name.contains("qsv")
                    || name.contains("videotoolbox")
                    || name.contains("amf");
                if (only_video && !is_video) || (only_hw && !is_hw) {
                    continue;
                }
                encoders.push(CodecEntry {
                    name,
                    long_name,
                    kind: "encoder".into(),
                    media_type: mt,
                    hw_devices: None,
                });
            }
            opaque = std::ptr::null_mut();
            loop {
                let codec = ffi::av_codec_iterate(&mut opaque as *mut _);
                if codec.is_null() {
                    break;
                }
                if ffi::av_codec_is_decoder(codec) == 0 {
                    continue;
                }
                let name = if !(*codec).name.is_null() {
                    CStr::from_ptr((*codec).name).to_string_lossy().into_owned()
                } else {
                    String::from("<unknown>")
                };
                let long_name = if !(*codec).long_name.is_null() {
                    CStr::from_ptr((*codec).long_name)
                        .to_string_lossy()
                        .into_owned()
                } else {
                    String::new()
                };
                let mt = media_type_to_str((*codec).type_).to_string();
                let is_video = mt == "video";
                let mut i = 0;
                let mut hw_list: Vec<String> = Vec::new();
                loop {
                    let cfg = ffi::avcodec_get_hw_config(codec, i);
                    if cfg.is_null() {
                        break;
                    }
                    let dtype = (*cfg).device_type;
                    let dname_ptr = ffi::av_hwdevice_get_type_name(dtype);
                    let dname = if !dname_ptr.is_null() {
                        CStr::from_ptr(dname_ptr).to_string_lossy().into_owned()
                    } else {
                        format!("type={}", dtype)
                    };
                    let available = devices_map.get::<str>(&dname).copied().unwrap_or(false);
                    hw_list.push(if available {
                        format!("{}(ok)", dname)
                    } else {
                        dname
                    });
                    i += 1;
                }
                let has_hw = !hw_list.is_empty();
                if (only_video && !is_video) || (only_hw && !has_hw) {
                    continue;
                }
                decoders.push(CodecEntry {
                    name,
                    long_name,
                    kind: "decoder".into(),
                    media_type: mt,
                    hw_devices: if has_hw { Some(hw_list) } else { None },
                });
            }
        }
        CodecProbeSummary {
            ffmpeg: ver,
            devices: device_entries,
            hw_encoders: hw_encs,
            encoders,
            decoders,
        }
    }
}
