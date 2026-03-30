use thiserror::Error;

#[derive(Error, Debug)]
pub enum OcrError {
    #[error("Ort error: {0}")]
    Ort(#[from] ort::Error),
    #[error("Io error")]
    Io(#[from] std::io::Error),
    #[error("Model init failed at {stage}: {source}")]
    ModelInit {
        stage: &'static str,
        #[source]
        source: Box<OcrError>,
    },
    #[error("Session not initialized")]
    ImageError(#[from] image::ImageError),
    #[error("Image error")]
    SessionNotInitialized,
}
