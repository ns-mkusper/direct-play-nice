//! Utility config
use crate::streaming_devices::streaming_device::StreamingDevice;
use anyhow::Error;
use serde::Deserialize;
use std::fs;
use std::path::Path;
use toml::from_str;

#[derive(Deserialize, Debug)]
pub struct Config {
    // TODO: implement complete config file
    streaming_devices: Vec<StreamingDevice>,
}

pub fn parse_config_from_toml<P: AsRef<Path>>(path: P) -> Result<Config, Error> {
    let toml_content = fs::read_to_string(path).unwrap();
    let config: Config = from_str(&toml_content)?;
    Ok(config)
}
