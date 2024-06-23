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

// Register all codecs and formats
// Open the input file
// Find the input video and audio streams
// Create the output format context
// Create the output video stream
// Set the output video codec parameters
// Create the output audio stream
// Set the output audio codec parameters
// Create the output subtitle stream
// Set the output subtitle codec parameters
// Open the output file
// Write the header to the output file
// Read frames from the input file and write them to the output file
// Reencode the video frame
// Reencode the audio frame
// Reencode the subtitle frame
// Write the trailer to the output file
// Close the input and output files
// Free the output context

// fn dump_av_info(path: &CStr) -> Result<(), Box<dyn Error>> {
//     let mut input_format_context = AVFormatContextInput::open(path, None, &mut None)?;
//     input_format_context.dump(0, path)?;
//     Ok(())
// }

fn convert_video_file(input_file: &CStr, output_file: &CStr) -> Result<(), Box<dyn Error>> {
    // INPUT
    let mut input_format_context = AVFormatContextInput::open(input_file, None, &mut None)?;

    // let video_streams: Vec<&AVStreamRef> = streams
    //     .into_iter()
    //     .filter(|s| s.codecpar().codec_type().is_video())
    //     .collect();

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
        let mut out_stream = output_format_context.new_stream();
        out_stream.set_codecpar(decode_context.extract_codecpar());
        out_stream.set_time_base(decode_context.time_base);
        stream_index_map.insert(av_stream_ref.index as i32, out_stream.index as i32);
    }

    let mut dict = None;
    output_format_context.write_header(&mut dict)?;
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
        output_format_context
            .interleaved_write_frame(&mut packet)
            .unwrap();
    }

    output_format_context.write_trailer()?;

    // out_stream.set_codecpar();
    // out_stream.set_time_base(time_base);

    /*
    AVMediaType::is_video()
    Iterate over the video streams
     */

    match video_index {
        Some(n) => println!("Video stream: {}", n),
        None => println!("No video streams detected!"),
    }

    Ok(())
}

fn main() {
    let args_c: Result<Vec<CString>, _> = std::env::args().map(CString::new).collect();
    let args = args_c.unwrap();

    let input_file = &args[1];

    let output_file = &args[2];

    // if !fs::metadata(output_file).is_ok() {
    //     println!(
    //         "Error: output file {} already exist! Not overwriting.",
    //         output_file
    //     );
    //     return;
    // }

    convert_video_file(input_file, output_file);

    // dump_av_info(&CString::new("./test.jpg").unwrap()).unwrap();
}

// extern crate rsmpeg;

// use rsmpeg::{EncodeCfg, File, Format, Packet, Stream, Timeline, VideoQuality};
// use std::env;

//   // Register all codecs and formats
//   // Open the input file
//   // Find the input video and audio streams
//   // Create the output format context
//   // Create the output video stream
//   // Set the output video codec parameters
//   // Create the output audio stream
//   // Set the output audio codec parameters
//   // Create the output subtitle stream
//   // Set the output subtitle codec parameters
//   // Open the output file
//   // Write the header to the output file
//   // Read frames from the input file and write them to the output file
//     // Reencode the video frame
//     // Reencode the audio frame
//     // Reencode the subtitle frame
//   // Write the trailer to the output file
//   // Close the input and output files
//   // Free the output context

// fn main() {
//     let args: Vec<String> = env::args().collect();
//     let input_path = &args[1];
//     let output_path = &args[2];
//     let vid_q = VideoQuality::High;
//     let cfg = EncodeCfg {
//         format: Format::Mp4,
//         video: Some((
//             "h264_nvenc".to_owned(),
//             rsmpeg::VideoEncCfg {
//                 quality: vid_q,
//                 bitrate: 4096_000,
//                 profile: "high".to_owned(),
//                 hlevel: 4.1,
//                 framerate: rsmpeg::Framerate { num: 60, den: 1 },
//             },
//         )),
//         audio: Some(("aac".to_owned(), rsmpeg::AudioEncCfg { bitrate: 160_000 })),
//         subtitles: Some(("mov_text".to_owned(), rsmpeg::SubtitleEncCfg {})),
//         dct: None,
//         threads: 12,
//     };

//     let infile = File::open(input_path).unwrap();
//     let mut outfile = File::create(output_path).unwrap();

//     let mut streams = Timeline::from_file(infile).unwrap();

//     streams
//         .video_streams_mut()
//         .filter_map(|s| s.extract_mut().filter(|_| true).map(|(p, i, _)| (i, p)))
//         .for_each(|(i, s)| {
//             s.set_codec_cfg(
//                 "h264_nvenc".to_string(),
//                 rsmpeg::VideoEncCfg {
//                     bitrate: 0,
//                     ..cfg.video.as_ref().unwrap().1
//                 },
//             )
//             .unwrap();
//             // NB: Adjust start time of video stream to avoid potential audio/video
//             // desync. The start_time of the input stream is slightly too late; if we
//             // leave it unchanged, the nvenc video encoder will start encoding very
//             // slightly behind the audio, presumably because glitches in the sequence
//             // of reference frame times.
//             streams.set_start(
//                 i,
//                 streams.start(i) - (rsmpeg::Duration::from_secs_f64(0.0)).into(),
//             );
//         });

//     streams
//         .audio_streams_mut()
//         .filter_map(|s| s.extract_mut().filter(|_| true).map(|(p, i, _)| (i, p)))
//         .for_each(|(i, s)| {
//             s.set_codec_cfg("aac".to_string(), cfg.audio.as_ref().unwrap().1)
//                 .unwrap();
//         });

//     streams
//         .subtitle_streams_mut()
//         .filter_map(|s| s.extract_mut().filter(|_| true).map(|(p, i, _)| (i, p)))
//         .for_each(|(i, s)| {
//             match s.codec() {
//                 Stream::Codec::Bitmap(_) => {
//                     s.set_codec_cfg("dvd_subtitle".to_string(), rsmpeg::CodecCfg::default())
//                         .unwrap();
//                 }
//                 _ => s
//                     .set_codec_cfg("mov_text".to_string(), rsmpeg::CodecCfg::default())
//                     .unwrap(),
//             };
//         });

//     let stream = rsmpeg::Stream {
//         stream_id: 0,
//         pts: 0,
//         dts: 0,
//         duration: vid_q.fixed_duration(),
//         data: Vec::new(),
//         special: rsmpeg::SpecialType::Empty,
//     };

//     streams.ext_streams_mut().push(stream);

//     rsmpeg::encode(&streams, cfg, &mut outfile).unwrap();
// }
