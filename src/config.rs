//! Utility config
use crate::streaming_devices::streaming_device::StreamingDevice;
use serde::Deserialize;
use std::fs;
use std::path::Path;
use toml::from_str;

#[derive(Deserialize, Debug)]
pub struct Config {
    streaming_devices: Vec<String>,
}

pub fn parse_config_from_toml<P: AsRef<Path>>(path: P) -> Result<Config, toml::de::Error> {
    let toml_content = fs::read_to_string(path).unwrap();
    let config: Config = from_str(&toml_content)?;
    Ok(config)
}
