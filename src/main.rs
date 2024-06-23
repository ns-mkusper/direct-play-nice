use anyhow::{anyhow, Context};
use rsmpeg::avcodec::{AVCodec, AVCodecContext};
use rsmpeg::avformat::{AVFormatContextInput, AVFormatContextOutput, AVStreamRef};
use std::any::Any;
use std::collections::HashMap;
use std::error::Error;
use std::ffi::{CStr, CString};
use std::fs;
use std::os::raw::c_int;
use std::path::Path;

// TODO: Register all codecs and formats

fn convert_video_file(input_file: &CStr, output_file: &CStr) -> Result<(), Box<dyn Error>> {
    // INPUT
    let mut input_format_context = AVFormatContextInput::open(input_file, None, &mut None)?;

    // OUTPUT
    let mut output_format_context = AVFormatContextOutput::create(&output_file, None)?;

    let mut stream_index_map = HashMap::new(); // input stream: output stream

    for av_stream_ref in input_format_context.streams() {
        let stream_codecpar = av_stream_ref.codecpar();
        let codec_id = stream_codecpar.codec_id;
        let decoder = AVCodec::find_decoder(codec_id)
            .with_context(|| anyhow!("video decoder ({}) not found.", codec_id))?;
        let mut decode_context = AVCodecContext::new(&decoder);

        decode_context.apply_codecpar(&stream_codecpar)?;
        decode_context.set_time_base(av_stream_ref.time_base);
        if let Some(framerate) = av_stream_ref.guess_framerate() {
            decode_context.set_framerate(framerate);
        }
        // TODO: Set the output video codec parameters
        // TODO: Create the output audio stream
        // TODO: Set the output audio codec parameters
        // TODO: Create the output subtitle stream
        // TODO: Set the output subtitle codec parameters
        let mut out_stream = output_format_context.new_stream();
        out_stream.set_codecpar(decode_context.extract_codecpar());
        out_stream.set_time_base(decode_context.time_base);
        stream_index_map.insert(av_stream_ref.index as i32, out_stream.index as i32);
    }

    let mut dict = None;
    output_format_context.write_header(&mut dict)?;

    // Write output frames
    loop {
        let mut packet = match input_format_context.read_packet()? {
            Some(x) => x,
            None => break,
        };
        packet.set_stream_index(
            stream_index_map
                .get(&(packet.stream_index as i32))
                .unwrap()
                .clone() as c_int,
        );
        // TODO: Re-encode audio, video and subtitles
        output_format_context
            .interleaved_write_frame(&mut packet)
            .unwrap();
    }

    output_format_context.write_trailer()?;

    Ok(())
}

fn main() {
    let args_c: Result<Vec<CString>, _> = std::env::args().map(CString::new).collect();
    let args = args_c.unwrap();

    // TODO: validate cli args

    let input_file = &args[1];
    let output_file = &args[2];

    convert_video_file(input_file, output_file);

    // dump_av_info(&CString::new("./test.jpg").unwrap()).unwrap();
}
