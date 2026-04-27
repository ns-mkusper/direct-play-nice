//! Probe command implementations for inspecting media metadata, codec availability, and hardware capabilities.

use anyhow::Result;
use rsmpeg::avformat::AVFormatContextInput;
use rsmpeg::ffi;
use serde::Serialize;
use std::ffi::CStr;

use crate::types::StreamsFilter;

pub(super) fn print_streams_info(input_file: &CStr, filter: StreamsFilter) -> Result<()> {
    let ictx = AVFormatContextInput::open(input_file)?;
    println!("Input: {}", input_file.to_string_lossy());
    let duration_us = ictx.duration;
    if duration_us > 0 {
        println!("Duration: {} ms", duration_us / 1000);
    }
    for st in ictx.streams() {
        let idx = st.index;
        let tb = st.time_base;
        let cp = st.codecpar();
        let ctype = cp.codec_type;
        let kind_matches = match filter {
            StreamsFilter::All => true,
            StreamsFilter::Video => ctype == ffi::AVMEDIA_TYPE_VIDEO,
            StreamsFilter::Audio => ctype == ffi::AVMEDIA_TYPE_AUDIO,
            StreamsFilter::Subtitle => ctype == ffi::AVMEDIA_TYPE_SUBTITLE,
        };
        if !kind_matches {
            continue;
        }
        let cname = unsafe { std::ffi::CStr::from_ptr(ffi::avcodec_get_name(cp.codec_id)) };
        let (stream_id, disp_default, disp_forced, disp_hi, disp_vi) = unsafe {
            let s_ptr = st.as_ptr();
            let id = (*s_ptr).id;
            let d = (*s_ptr).disposition;
            (
                id,
                (d & ffi::AV_DISPOSITION_DEFAULT as i32) != 0,
                (d & ffi::AV_DISPOSITION_FORCED as i32) != 0,
                (d & ffi::AV_DISPOSITION_HEARING_IMPAIRED as i32) != 0,
                (d & ffi::AV_DISPOSITION_VISUAL_IMPAIRED as i32) != 0,
            )
        };
        print!(
            "[stream {} id={}] type={:?} codec={} ",
            idx,
            stream_id,
            ctype,
            cname.to_string_lossy()
        );
        match ctype {
            ffi::AVMEDIA_TYPE_VIDEO => {
                let (w, h) = (cp.width, cp.height);
                let fps = st
                    .guess_framerate()
                    .map(|r| format!("{}/{}", r.num, r.den))
                    .unwrap_or_else(|| "?".into());
                println!(
                    "res={}x{} fps={} bitrate={} time_base={}/{}",
                    w, h, fps, cp.bit_rate, tb.num, tb.den
                );
            }
            ffi::AVMEDIA_TYPE_AUDIO => {
                let ch = cp.ch_layout.nb_channels;
                println!(
                    "channels={} sample_rate={} bitrate={} time_base={}/{}",
                    ch, cp.sample_rate, cp.bit_rate, tb.num, tb.den
                );
            }
            ffi::AVMEDIA_TYPE_SUBTITLE => {
                println!("subtitle time_base={}/{}", tb.num, tb.den);
            }
            _ => {
                println!("time_base={}/{}", tb.num, tb.den);
            }
        }
        println!(
            "  disposition: default={} forced={} hearing_impaired={} visual_impaired={}",
            disp_default, disp_forced, disp_hi, disp_vi
        );
    }
    Ok(())
}

#[derive(Serialize)]
struct JsonStreamInfo {
    index: i32,
    stream_id: i32,
    kind: String,
    codec: String,
    time_base: (i32, i32),
    #[serde(skip_serializing_if = "Option::is_none")]
    width: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    height: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    fps: Option<(i32, i32)>,
    #[serde(skip_serializing_if = "Option::is_none")]
    bitrate: Option<i64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    channels: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    sample_rate: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    language: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    title: Option<String>,
    disposition: JsonDisposition,
}

#[derive(Serialize)]
pub(super) struct JsonProbe {
    input: String,
    duration_ms: Option<i64>,
    streams: Vec<JsonStreamInfo>,
}

#[derive(Serialize)]
struct JsonDisposition {
    default: bool,
    forced: bool,
    hearing_impaired: bool,
    visual_impaired: bool,
}

pub(super) fn gather_streams_info_json(
    input_file: &CStr,
    filter: StreamsFilter,
) -> Result<JsonProbe> {
    let ictx = AVFormatContextInput::open(input_file)?;
    let duration_us = ictx.duration;
    let mut out: Vec<JsonStreamInfo> = Vec::new();
    for st in ictx.streams() {
        let idx = st.index;
        let tb = st.time_base;
        let cp = st.codecpar();
        let ctype = cp.codec_type;
        let kind = match ctype {
            ffi::AVMEDIA_TYPE_VIDEO => "video",
            ffi::AVMEDIA_TYPE_AUDIO => "audio",
            ffi::AVMEDIA_TYPE_SUBTITLE => "subtitle",
            ffi::AVMEDIA_TYPE_ATTACHMENT => "attachment",
            ffi::AVMEDIA_TYPE_DATA => "data",
            _ => "other",
        }
        .to_string();
        let kind_matches = match filter {
            StreamsFilter::All => true,
            StreamsFilter::Video => ctype == ffi::AVMEDIA_TYPE_VIDEO,
            StreamsFilter::Audio => ctype == ffi::AVMEDIA_TYPE_AUDIO,
            StreamsFilter::Subtitle => ctype == ffi::AVMEDIA_TYPE_SUBTITLE,
        };
        if !kind_matches {
            continue;
        }
        let cname = unsafe { std::ffi::CStr::from_ptr(ffi::avcodec_get_name(cp.codec_id)) };
        let (stream_id, disp_default, disp_forced, disp_hi, disp_vi) = unsafe {
            let s_ptr = st.as_ptr();
            let id = (*s_ptr).id;
            let d = (*s_ptr).disposition;
            (
                id,
                (d & ffi::AV_DISPOSITION_DEFAULT as i32) != 0,
                (d & ffi::AV_DISPOSITION_FORCED as i32) != 0,
                (d & ffi::AV_DISPOSITION_HEARING_IMPAIRED as i32) != 0,
                (d & ffi::AV_DISPOSITION_VISUAL_IMPAIRED as i32) != 0,
            )
        };
        let (mut width, mut height, mut fps, mut channels, mut sample_rate) =
            (None, None, None, None, None);
        match ctype {
            ffi::AVMEDIA_TYPE_VIDEO => {
                width = Some(cp.width);
                height = Some(cp.height);
                fps = st.guess_framerate().map(|r| (r.num, r.den));
            }
            ffi::AVMEDIA_TYPE_AUDIO => {
                channels = Some(cp.ch_layout.nb_channels);
                sample_rate = Some(cp.sample_rate);
            }
            _ => {}
        }
        let (language, title): (Option<String>, Option<String>) = (None, None);
        out.push(JsonStreamInfo {
            index: idx,
            stream_id,
            kind,
            codec: cname.to_string_lossy().into_owned(),
            time_base: (tb.num, tb.den),
            width,
            height,
            fps,
            bitrate: if cp.bit_rate > 0 {
                Some(cp.bit_rate)
            } else {
                None
            },
            channels,
            sample_rate,
            language,
            title,
            disposition: JsonDisposition {
                default: disp_default,
                forced: disp_forced,
                hearing_impaired: disp_hi,
                visual_impaired: disp_vi,
            },
        });
    }
    Ok(JsonProbe {
        input: input_file.to_string_lossy().into_owned(),
        duration_ms: if duration_us > 0 {
            Some(duration_us / 1000)
        } else {
            None
        },
        streams: out,
    })
}
