//! Compatibility shim that preserves legacy module layout while exposing the current app entry modules.

#[path = "app_convert.rs"]
pub mod app_convert;
#[path = "app_entry.rs"]
pub mod app_entry;
