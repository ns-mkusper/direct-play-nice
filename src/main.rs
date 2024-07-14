use anyhow::{anyhow, bail, Context, Result};
use log::{debug, error, info, warn};
use rsmpeg::avcodec::{AVCodec, AVCodecContext, AVPacket, AVSubtitle};
use rsmpeg::avformat::{AVFormatContextInput, AVFormatContextOutput, AVStreamMut};
use rsmpeg::avutil::{ra, AVAudioFifo, AVChannelLayout, AVFrame, AVSamples};
use rsmpeg::error::RsmpegError;
use rsmpeg::ffi::{self, AVSubtitleRect};
use rsmpeg::swresample::SwrContext;
use rsmpeg::swscale::SwsContext;
use std::backtrace::Backtrace;
use std::error::Error;
use std::mem;
use std::ptr;

use std::{
    ffi::{CStr, CString},
    sync::atomic::{AtomicI64, Ordering},
};

/// The output bit rate in bit/s
const OUTPUT_BIT_RATE: i64 = 96000;
/// The number of output channels
const OUTPUT_CHANNELS: i32 = 2;

// TODO: Register all codecs and formats
// TODO: ensure audio streams have the same metadata
// inspired by https://github.com/larksuite/rsmpeg/blob/master/tests/ffmpeg_examples/transcode_aac.rs
pub enum StreamExtras {
    Some((SwrContext, AVAudioFifo)),
    None,
}

pub struct StreamProcessingContext {
    decode_context: AVCodecContext,
    encode_context: AVCodecContext,
    stream_index: i32,
    media_type: ffi::AVMediaType,
    frame_buffer: Option<AVAudioFifo>, // TODO: Support video stream buffers too?
    resample_context: Option<SwrContext>,
    is_processed: bool,
}

impl std::fmt::Debug for StreamProcessingContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StreamProcessingContext")
        // .field("decode_context", &self.decode_context)
        // .field("encode_context", &self.encode_context)
            .field("stream_index", &self.stream_index)
            .field("media_type", &self.media_type)
        // .field("frame_buffer", &self.frame_buffer)
        // .field("resample_context", &self.resample_context)
            .field("is_processed", &self.is_processed)
            .finish()
    }
}

fn init_audio_resampler(
    decode_context: &mut AVCodecContext,
    encode_context: &mut AVCodecContext,
) -> Result<SwrContext> {
    let mut resample_context = SwrContext::new(
        &encode_context.ch_layout,
        encode_context.sample_fmt,
        encode_context.sample_rate,
        &decode_context.ch_layout,
        decode_context.sample_fmt,
        decode_context.sample_rate,
    )
        .context("Could not allocate resample context")?;
    resample_context
        .init()
        .context("Could not open resample context")?;
    Ok(resample_context)
}

fn add_samples_to_fifo(
    fifo: &mut AVAudioFifo,
    samples_buffer: &AVSamples,
    frame_size: i32,
) -> Result<()> {
    fifo.realloc(fifo.size() + frame_size);
    unsafe { fifo.write(samples_buffer.audio_data.as_ptr(), frame_size) }
    .context("Could not write data to FIFO")?;
    Ok(())
}

fn init_output_audio_frame(
    nb_samples: i32,
    ch_layout: ffi::AVChannelLayout,
    sample_fmt: i32,
    sample_rate: i32,
) -> Result<AVFrame> {
    let mut frame = AVFrame::new();
    frame.set_nb_samples(nb_samples);
    frame.set_ch_layout(ch_layout);
    frame.set_format(sample_fmt);
    frame.set_sample_rate(sample_rate);

    frame
        .get_buffer(0)
        .context("Could not allocate output frame samples")?;

    Ok(frame)
}

/// Return boolean: if data is written.
fn encode_audio_frame(
    mut frame: Option<AVFrame>,
    output_format_context: &mut AVFormatContextOutput,
    encode_context: &mut AVCodecContext,
    stream_index: i32,
) -> Result<()> {
    static PTS: AtomicI64 = AtomicI64::new(0);
    static ENCODED_FRAMES: AtomicI64 = AtomicI64::new(0);

    ENCODED_FRAMES.fetch_add(1 as i64, Ordering::Relaxed);
    info!("PTS: {:?}, encoded_frames: {:?}", PTS, ENCODED_FRAMES);

    if let Some(frame) = frame.as_mut() {
        info!("FRAME NB SAMPLES: {}", frame.nb_samples);
        frame.set_pts(PTS.fetch_add(frame.nb_samples as i64, Ordering::Relaxed));
        // frame.set_pts(frame.best_effort_timestamp);
        info!("FRAME PTS: {:?}", frame.pts);
    }

    encode_context.send_frame(frame.as_ref())?;

    loop {
        let mut packet = match encode_context.receive_packet() {
            Ok(packet) => packet,
            Err(RsmpegError::EncoderDrainError) | Err(RsmpegError::EncoderFlushedError) => {
                break;
            }
            Err(e) => Err(e).context("Could not encode frame")?,
        };

        packet.set_stream_index(stream_index);

        packet.rescale_ts(
            encode_context.time_base,
            output_format_context
                .streams()
                .get(stream_index as usize)
                .context("Failed to get stream")?
                .time_base,
        );

        output_format_context
            .write_frame(&mut packet)
            .context("Could not write frame")?;

        info!("Sucessfully wrote audio frame to output file!");
    }
    Ok(())
}

fn load_encode_and_write(
    fifo: &mut AVAudioFifo,
    output_format_context: &mut AVFormatContextOutput,
    encode_context: &mut AVCodecContext,
    stream_index: i32,
) -> Result<()> {
    let frame_size = fifo.size().min(encode_context.frame_size); // TODO: should be encode_context.frame_size but it reads 0

    info!("ENCODE CONTEXT FRAME SIZE: {}", encode_context.frame_size);
    info!("LOAD FRAME SIZE: {}", frame_size);
    let mut frame = init_output_audio_frame(
        frame_size,
        encode_context.ch_layout().clone().into_inner(),
        encode_context.sample_fmt,
        encode_context.sample_rate,
    )
        .context("Failed to initialize audio frame.")?;

    if unsafe {
        let read_frame_size = fifo.read(frame.data_mut().as_mut_ptr(), frame_size)?;
        info!("Read frame size: {}", read_frame_size);
        read_frame_size
    } < frame_size
    {
        bail!("Could not read data from FIFO");
    }

    encode_audio_frame(
        Some(frame),
        output_format_context,
        encode_context,
        stream_index,
    )
        .context("Error encoding audio frame!")?;

    Ok(())
}

fn encode_write_frame(
    frame: Option<&AVFrame>,
    encode_context: &mut AVCodecContext,
    output_format_context: &mut AVFormatContextOutput,
    out_stream_index: usize,
) -> Result<()> {
    if let Some(frame) = frame {
        info!("Frame details:");
        info!("  Number of samples: {}", frame.nb_samples);
        info!("  Channel layout: {:?}", frame.ch_layout);
        info!("  Sample format: {:?}", frame.format);
        info!("  Sample rate: {}", frame.sample_rate);
        info!("  PTS: {:?}", frame.pts);
        info!("  out_stream_index: {}", out_stream_index);
    } else {
        info!("Sending null frame to flush encoder");
    }

    info!("Encoder context details:");
    info!("  Frame size: {}", encode_context.frame_size);
    info!("  Channel layout: {:?}", encode_context.ch_layout);
    info!("  Sample format: {:?}", encode_context.sample_fmt);
    info!("  Sample rate: {}", encode_context.sample_rate);

    match encode_context.send_frame(frame) {
        Ok(()) => {
            // Frame sent successfully
            info!("Frame sent successfully");
        }
        Err(RsmpegError::EncoderFlushedError) => {
            // This error occurs when trying to send a frame to an already-flushed encoder
            info!("Encoder has been flushed");
            // Handle accordingly, maybe break from loop or return
        }
        Err(RsmpegError::AVError(code)) => {
            info!("FFmpeg error occurred: {}", code);
            // Handle specific FFmpeg errors if needed
        }
        Err(e) => {
            // Catch-all for any other errors
            info!("An error occurred while sending frame: {}", e);
            // Handle or propagate the error
            return Err(e.into());
        }
    }

    loop {
        let mut packet = match encode_context.receive_packet() {
            Ok(packet) => packet,
            Err(RsmpegError::EncoderDrainError) | Err(RsmpegError::EncoderFlushedError) => break,
            Err(e) => {
                return Err(e.into());
            }
        };

        packet.set_stream_index(out_stream_index as i32);
        packet.rescale_ts(
            encode_context.time_base,
            output_format_context
                .streams()
                .get(out_stream_index)
                .context("Failed to get stream")?
                .time_base,
        );

        match output_format_context.interleaved_write_frame(&mut packet) {
            Ok(()) => Ok(()),
            Err(RsmpegError::AVError(-22)) => Ok(()),
            Err(e) => Err(e),
        }?;
    }

    Ok(())
}

fn set_video_codec_par(
    decode_context: &mut AVCodecContext,
    encode_context: &mut AVCodecContext,
    output_stream: &mut AVStreamMut,
) {
    // apply video encoder params from input file
    encode_context.set_sample_rate(decode_context.sample_rate);
    encode_context.set_width(decode_context.width);
    encode_context.set_height(decode_context.height);
    encode_context.set_time_base(decode_context.time_base);
    encode_context.set_pix_fmt(ffi::AV_PIX_FMT_YUV420P);
    encode_context.set_max_b_frames(decode_context.max_b_frames);
    encode_context.set_bit_rate(decode_context.bit_rate);
    encode_context.set_gop_size(decode_context.gop_size); // causes [libx264 @ 000001A178C1E740] MB rate (146880000) > level limit (245760)
    // TODO: find a safe way to do this
    unsafe {
        (*encode_context.as_mut_ptr()).profile = ffi::FF_PROFILE_H264_HIGH as i32;
        (*encode_context.as_mut_ptr()).level = 41;
        // opt_set(encode_context.priv_data, c"level", c"4.1", 0);
    }
    output_stream.set_codecpar(encode_context.extract_codecpar());
}

fn set_audio_codec_par(
    decode_context: &mut AVCodecContext,
    encode_context: &mut AVCodecContext,
    output_stream: &mut AVStreamMut,
) {
    // TODO: Read input to determine output audio codec params
    let encoder = AVCodec::find_encoder(ffi::AV_CODEC_ID_AAC).expect("Could not find AAC encoder");
    encode_context.set_ch_layout(AVChannelLayout::from_nb_channels(OUTPUT_CHANNELS).into_inner());
    // The input file's sample rate is used to avoid a sample rate conversion.
    encode_context.set_sample_rate(decode_context.sample_rate);
    encode_context.set_sample_fmt(encoder.sample_fmts().unwrap()[0]);
    encode_context.set_bit_rate(OUTPUT_BIT_RATE);

    output_stream.set_codecpar(encode_context.extract_codecpar());
    output_stream.set_time_base(ra(1, decode_context.sample_rate)); // use high-precision time base
}

fn set_subtitle_codec_par(decode_context: &mut AVCodecContext, encode_context: &mut AVCodecContext, output_stream: &mut AVStreamMut) {
    // Set subtitle encoder parameters based on the input subtitle stream
    encode_context.set_time_base(decode_context.time_base);

    if decode_context.subtitle_header_size > 0 {
        let mut new_subtitle_header = vec![0u8; decode_context.subtitle_header_size as usize];
        new_subtitle_header.copy_from_slice(unsafe {
            std::slice::from_raw_parts(
                decode_context.subtitle_header,
                decode_context.subtitle_header_size as usize,
            )
        });

        unsafe {
            (*encode_context.as_mut_ptr()).subtitle_header = ffi::av_mallocz(new_subtitle_header.len()) as *mut _;
            (*encode_context.as_mut_ptr()).subtitle_header_size = new_subtitle_header.len() as i32;
            std::ptr::copy_nonoverlapping(
                new_subtitle_header.as_ptr(),
                (*encode_context.as_mut_ptr()).subtitle_header,
                new_subtitle_header.len(),
            );
        }
    }

    // subtitle header needs to be set to call open (check docstring)
    // TODO: handle cases where decoder subtitle header is invalid/empty/corrupt?
    // unsafe {
    //     (*encode_context.as_mut_ptr()).subtitle_header = decode_context.subtitle_header;
    //     (*encode_context.as_mut_ptr()).subtitle_header_size = decode_context.subtitle_header_size;
    // }
    // println!("Decoder subtitle header size: {}", decode_context.subtitle_header_size);
    // println!("Encoder subtitle header size: {}", encode_context.subtitle_header_size);

    // unsafe { ptr::copy_nonoverlapping(decode_context.subtitle_header,
    //                                   encode_context.subtitle_header, decode_context.subtitle_header_size as usize) }; // throws status access violation error


    output_stream.set_codecpar(encode_context.extract_codecpar());
}

fn convert_video_file(input_file: &CStr, output_file: &CStr) -> Result<(), anyhow::Error> {
    let mut input_format_context = AVFormatContextInput::open(input_file, None, &mut None)?;
    input_format_context.dump(0, input_file)?;

    let mut output_format_context = AVFormatContextOutput::create(&output_file, None)?;

    let mut stream_contexts: Vec<StreamProcessingContext> = Vec::new();
    let mut subtitle_buffer: Vec<u8> = vec![0u8; 1024 * 1024];
    let subtitle_buffer: &mut [u8] = &mut subtitle_buffer;

    for stream in input_format_context.streams() {
        let input_stream_codecpar = stream.codecpar();
        let input_codec_id = input_stream_codecpar.codec_id;
        let decoder = AVCodec::find_decoder(input_codec_id)
            .with_context(|| anyhow!("decoder ({}) not found.", input_codec_id))?;
        let mut decode_context = AVCodecContext::new(&decoder);
        decode_context.apply_codecpar(&input_stream_codecpar)?;
        decode_context.set_time_base(stream.time_base); // TODO: needed?
        if let Some(framerate) = stream.guess_framerate() {
            decode_context.set_framerate(framerate);
        }
        decode_context.open(None)?;

        let mut output_stream = output_format_context.new_stream();
        let mut encode_context: AVCodecContext;
        let media_type: ffi::AVMediaType;
        let mut frame_buffer: Option<AVAudioFifo> = None;
        let mut resample_context: Option<SwrContext> = None;

        match decode_context.codec_type {
            ffi::AVMEDIA_TYPE_VIDEO => {
                let encoder = AVCodec::find_encoder(ffi::AV_CODEC_ID_H264)
                    .expect("Could not find H264 encoder");
                encode_context = AVCodecContext::new(&encoder);
                media_type = ffi::AVMEDIA_TYPE_VIDEO;

                set_video_codec_par(&mut decode_context, &mut encode_context, &mut output_stream);
            }
            ffi::AVMEDIA_TYPE_AUDIO => {
                let encoder = AVCodec::find_encoder(ffi::AV_CODEC_ID_AAC)
                    .expect("Could not find AAC encoder");

                encode_context = AVCodecContext::new(&encoder);
                media_type = ffi::AVMEDIA_TYPE_AUDIO;

                set_audio_codec_par(&mut decode_context, &mut encode_context, &mut output_stream);
                // Initialize the resampler to be able to convert audio sample formats.
                resample_context =
                    init_audio_resampler(&mut decode_context, &mut encode_context).ok();

                // Initialize the FIFO buffer to store audio samples to be encoded.
                frame_buffer = Some(AVAudioFifo::new(
                    encode_context.sample_fmt,
                    encode_context.ch_layout.nb_channels,
                    1,
                ));
            }
            ffi::AVMEDIA_TYPE_SUBTITLE => {
                // TODO: handle cases with no metadata?
                // TODO: always copy metadata or only in some cases?
                output_stream.set_metadata(stream.metadata().as_deref().cloned());

                let encoder = AVCodec::find_encoder(ffi::AV_CODEC_ID_MOV_TEXT)
                    .expect("Could not find MOV_TEXT encoder");
                encode_context = AVCodecContext::new(&encoder);
                media_type = ffi::AVMEDIA_TYPE_SUBTITLE;
                set_subtitle_codec_par(&mut decode_context, &mut encode_context, &mut output_stream);
            }
            // TODO: Handle metadata streams
            n => {
                info!("Found unknown stream type ({})", n);
                continue;
            }
        }

        let stream_process_context = StreamProcessingContext {
            decode_context,
            encode_context,
            stream_index: output_stream.index,

            media_type,
            frame_buffer,
            resample_context,
            is_processed: false,
        };

        stream_contexts.push(stream_process_context);
    }

    // Write the header of the output file container.
    output_format_context
        .write_header(&mut None)
        .context("Error writing output file header")?;

    let (
        mut video_packet_count,
        mut audio_packet_count,
        mut subtitle_packet_count,
        mut misc_packet_count,
    ): (i32, i32, i32, i32) = (0, 0, 0, 0);
    // Write output frames

    loop {
        let mut packet = match input_format_context.read_packet()? {
            Some(x) => x,
            None => break,
        };

        let Some(mut sp_context) = stream_contexts
            .iter_mut()
            .find(|context| context.stream_index == packet.stream_index)
        else {
            break;
        }; // TODO: handle missing matching streams

        let input_stream = &input_format_context.streams()[packet.stream_index as usize];
        match sp_context.media_type {
            ffi::AVMEDIA_TYPE_VIDEO => {
                video_packet_count += 1;
                continue;
                packet.rescale_ts(input_stream.time_base, sp_context.decode_context.time_base);

                sp_context.encode_context.open(None).context("Error opening video encoding context")?;

                match sp_context.decode_context.send_packet(Some(&packet)) {
                    Ok(_) | Err(RsmpegError::DecoderFlushedError) => {}
                    Err(e) => {
                        info!("{}", e); // Erroring here
                        return Err(e.into()); // TODO: reasonable to end here on error?
                    }
                }

                loop {
                    let frame = match sp_context.decode_context.receive_frame() {
                        Ok(frame) => {
                            info!("Successfully processed frame!");
                            frame
                        }
                        Err(RsmpegError::DecoderDrainError)
                            | Err(RsmpegError::DecoderFlushedError) => {
                                break;
                            }
                        Err(e) => {
                            info!("Decoder receive frame error: {}", e);
                            break;
                        }
                    };

                    let mut new_frame = AVFrame::new();
                    new_frame.set_width(sp_context.decode_context.width);
                    new_frame.set_height(sp_context.decode_context.height);
                    new_frame.set_format(ffi::AV_PIX_FMT_YUV420P);
                    new_frame.alloc_buffer().context("Error allocating ")?;

                    let mut sws_context = SwsContext::get_context(
                        sp_context.decode_context.width,
                        sp_context.decode_context.height,
                        sp_context.decode_context.pix_fmt,
                        sp_context.encode_context.width,
                        sp_context.encode_context.height,
                        sp_context.encode_context.pix_fmt,
                        ffi::SWS_FAST_BILINEAR | ffi::SWS_ACCURATE_RND,
                        None,
                        None,
                        None,
                    )
                        .context("Failed to create a swscale context.")?;

                    new_frame.set_pts(frame.best_effort_timestamp);

                    sws_context
                        .scale_frame(&frame, 0, sp_context.decode_context.height, &mut new_frame)
                        .context("Failed to scale frame.")?;

                    encode_write_frame(
                        Some(&new_frame),
                        &mut sp_context.encode_context,
                        &mut output_format_context,
                        packet.stream_index as usize,
                    )
                        .context("Failed to write video frame")?;
                }
            }
            // TODO: process medstream into AAC
            // see: https://github.com/larksuite/rsmpeg/blob/master/tests/ffmpeg_examples/transcode_aac.rs
            ffi::AVMEDIA_TYPE_AUDIO => {
                audio_packet_count += 1;

                packet.rescale_ts(input_stream.time_base, sp_context.decode_context.time_base);

                sp_context.encode_context.open(None).context("Error opening audio encoding context")?;

                let Some(mut fifo) = sp_context.frame_buffer.as_mut() else {
                    info!("Failed to get Audio FIFO buffer!");
                    break; // TODO: add handling of bad buffer
                };

                match sp_context.decode_context.send_packet(Some(&packet)) {
                    Ok(_) | Err(RsmpegError::DecoderFlushedError) => {}
                    Err(e) => {
                        info!("{}", e); // Erroring here
                        return Err(e.into()); // TODO: reasonable to end here on error?
                    }
                }

                loop {
                    let frame = match sp_context.decode_context.receive_frame() {
                        Ok(frame) => {
                            info!("Successfully processed frame!");
                            frame
                        }
                        Err(RsmpegError::DecoderDrainError)
                            | Err(RsmpegError::DecoderFlushedError) => {
                                break;
                            }
                        Err(e) => {
                            info!("Decoder receive frame error: {}", e);
                            break;
                        }
                    };

                    let output_samples = AVSamples::new(
                        sp_context.encode_context.ch_layout.nb_channels,
                        frame.nb_samples,
                        sp_context.encode_context.sample_fmt,
                        0,
                    )
                        .context("Create samples buffer failed.")?;
                    let output_frame_size = sp_context.encode_context.frame_size; // TODO: sp_context.encode_context.frame_size but it's 0 for some reason
                    info!("OUTPUT FRAME SIZE: {}", output_frame_size);

                    let mut output_samples = AVSamples::new(
                        sp_context.encode_context.ch_layout.nb_channels,
                        frame.nb_samples,
                        sp_context.encode_context.sample_fmt,
                        0,
                    )
                        .context("Create samples buffer failed.")?;

                    match &mut sp_context.resample_context {
                        Some(resampler) => unsafe {
                            resampler
                                .convert(
                                    output_samples.audio_data.as_mut_ptr(),
                                    output_samples.nb_samples,
                                    frame.extended_data as *const _,
                                    frame.nb_samples,
                                )
                                .context("Could not convert input samples")?;
                        },
                        None => {}
                    }

                    add_samples_to_fifo(&mut fifo, &output_samples, frame.nb_samples)?;

                    info!("FIFO SIZE: {}", fifo.size());
                    info!("AUDIO STREAM INDEX: {}", sp_context.stream_index);
                    while fifo.size() >= output_frame_size {
                        load_encode_and_write(&mut fifo, &mut output_format_context, &mut sp_context.encode_context, sp_context.stream_index as i32)?;
                    }
                }
            }
            ffi::AVMEDIA_TYPE_SUBTITLE => {
                subtitle_packet_count += 1;

                packet.rescale_ts(input_stream.time_base, sp_context.decode_context.time_base);

                sp_context
                    .encode_context
                    .open(None)
                    .context("Could not open subitle encoder context")?;

                // TODO: should I decode the subs this way?
                match sp_context.decode_context.decode_subtitle(Some(&mut packet)) {
                    Ok(sub) => {

                        match sub {
                            Some(s) =>                         {
                                // TODO: write the subtitle buffers to the output file at some point
                                sp_context.encode_context.encode_subtitle(&s, subtitle_buffer)?;

                                // println!("SUBTITLE BUFFER CONTENT: {}", String::from_utf8_lossy(subtitle_buffer));
                            }
                            None => {}
                        }

                        // process_subtitle(&sub);
                    }
                    Err(rsmpeg::error::RsmpegError::DecoderDrainError) => {
                        // The decoder has been fully drained, no more subtitles to decode
                        // You can choose to break out of the loop or continue processing
                        break;
                    }
                    Err(rsmpeg::error::RsmpegError::DecoderFlushedError) => {
                        // The decoder has been flushed, no more subtitles to decode
                        // You can choose to break out of the loop or continue processing
                        break;

                    }
                    Err(e) => {
                        // Handle any other error cases
                        eprintln!("Error decoding subtitle: {}", e);
                        // You can choose to break out of the loop or continue processing
                    }
                }

                // WORK HERE
                // TODO: FAILING (cause there's no frame), probably can't do it this way
                // sp_context.decode_context.send_packet(Some(&packet))
                // .context("Could not send subtitle packet for decoding")?;

                // loop {
                //     let mut decoded_packet = match sp_context.decode_context.receive_packet() {
                //         Ok(decoded_packet) => {
                //             // println!("Successfully processed decoded_packet!");
                //             decoded_packet
                //         }
                //         Err(RsmpegError::DecoderDrainError)
                //             | Err(RsmpegError::DecoderFlushedError) => {
                //                 println!("Decoder drained or flushed");
                //             break;
                //         }
                //         Err(e) => {
                //             println!("Decoder receive frame error: {}", e);
                //             break;
                //         }
                //     };

                //     packet.rescale_ts(sp_context.decode_context.time_base, sp_context.encode_context.time_base);
                // output_format_context
                //     .interleaved_write_frame(&mut decoded_packet)
                //     .context("Could not write subtitle packet")?;


                // }
            }

            _ => {
                misc_packet_count += 1;
            }
        }
    }

    println!("finishing");

    // After processing all packets, flush each encoder
    for mut context in stream_contexts {
        info!("StreamProcessingContext: {:?}", context);
        match context.media_type {
            ffi::AVMEDIA_TYPE_VIDEO => {
                // flush video encoder
                println!("FLUSHING VIDEO ENCODE CONTEXT");
                let _ = encode_write_frame(
                    None,
                    &mut context.encode_context,
                    &mut output_format_context,
                    context.stream_index as usize,
                );
                println!("FLUSHED VIDEO ENCODE CONTEXT");

            }
            ffi::AVMEDIA_TYPE_AUDIO => {
                // Write all audio to file
                // Anything left over?
                let Some(mut fifo) = context.frame_buffer.as_mut() else {
                    break; // TODO: add handling of bad buffer
                };

                if fifo.size() > 0 {
                    // Write last frame
                    load_encode_and_write(
                        &mut fifo,
                        &mut output_format_context,
                        &mut context.encode_context,
                        context.stream_index as i32,
                    )
                        .context("Failed to load and encode audio frame.")?;
                }

                // Flush encode context
                encode_audio_frame(
                    None,
                    &mut output_format_context,
                    &mut context.encode_context,
                    context.stream_index as i32,
                )
                    .context("Failed to flush audio encoder.")?;
            }
            ffi::AVMEDIA_TYPE_SUBTITLE => {}
            _ => {}
        }


    }

    info!(
        "Video Packet Count: {}",
        format!("{:>10}", video_packet_count)
    );
    info!(
        "Audio Packet Count: {}",
        format!("{:>10}", audio_packet_count)
    );
    info!(
        "Subtitle Packet Count: {}",
        format!("{:>10}", subtitle_packet_count)
    );
    info!(
        "Misc Packet Count: {}",
        format!("{:>10}", misc_packet_count)
    );
    output_format_context.write_trailer()?;

    Ok(())
}

fn main() -> Result<()> {
    unsafe {
        ffi::av_log_set_level(ffi::AV_LOG_TRACE as i32); // Set the log level to TRACE (most verbose)
    }
    let args_c: Result<Vec<CString>, _> = std::env::args().map(CString::new).collect();
    let args = args_c.unwrap();

    // TODO: validate cli args

    let input_file = &args[1];
    let output_file = &args[2];

    convert_video_file(input_file, output_file)?;
    Ok(())
}
