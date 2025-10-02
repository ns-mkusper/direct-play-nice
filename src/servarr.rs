use anyhow::{anyhow, bail, Context, Result};
use log::{info, warn};
use std::env;
use std::ffi::CString;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntegrationKind {
    Sonarr,
    Radarr,
}

impl IntegrationKind {
    fn label(self) -> &'static str {
        match self {
            IntegrationKind::Sonarr => "Sonarr",
            IntegrationKind::Radarr => "Radarr",
        }
    }

    fn episode_path_var(self) -> &'static str {
        match self {
            IntegrationKind::Sonarr => "sonarr_episodefile_path",
            IntegrationKind::Radarr => "radarr_moviefile_path",
        }
    }

    fn title_var(self) -> &'static str {
        match self {
            IntegrationKind::Sonarr => "sonarr_series_title",
            IntegrationKind::Radarr => "radarr_movie_title",
        }
    }

    fn is_upgrade_var(self) -> &'static str {
        match self {
            IntegrationKind::Sonarr => "sonarr_isupgrade",
            IntegrationKind::Radarr => "radarr_isupgrade",
        }
    }
}

#[derive(Debug, Clone)]
pub struct ReplacePlan {
    pub kind: IntegrationKind,
    pub event_type: String,
    pub display_name: Option<String>,
    pub is_upgrade: Option<bool>,
    pub input_path: PathBuf,
    pub final_output_path: PathBuf,
    pub temp_output_path: PathBuf,
    pub backup_path: PathBuf,
    pub input_cstring: CString,
    pub temp_output_cstring: CString,
}

impl ReplacePlan {
    pub fn assign_to_args(
        &self,
        input_slot: &mut Option<CString>,
        output_slot: &mut Option<CString>,
    ) {
        if input_slot.is_none() {
            *input_slot = Some(self.input_cstring.clone());
        }
        if output_slot.is_none() {
            *output_slot = Some(self.temp_output_cstring.clone());
        }
    }

    pub fn log_summary(&self) {
        let title = self.display_name.as_deref().unwrap_or_else(|| {
            self.input_path
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("<unknown>")
        });
        let final_display = self
            .final_output_path
            .file_name()
            .and_then(|n| n.to_str())
            .map(|s| s.to_owned())
            .unwrap_or_else(|| self.final_output_path.to_string_lossy().into_owned());

        if self.final_output_path == self.input_path {
            info!(
                "{} {} event: converting '{}' in place",
                self.kind.label(),
                self.event_type,
                title
            );
        } else {
            info!(
                "{} {} event: converting '{}' -> '{}'",
                self.kind.label(),
                self.event_type,
                title,
                final_display
            );
        }
        if let Some(upgrade) = self.is_upgrade {
            info!("{} reported upgrade flag: {}", self.kind.label(), upgrade);
        }
    }

    pub fn finalize_success(self) -> Result<()> {
        use std::fs;

        // Move the original file aside
        if self.input_path.exists() {
            fs::rename(&self.input_path, &self.backup_path).with_context(|| {
                format!(
                    "{} integration failed to move original file '{}' to backup '{}'",
                    self.kind.label(),
                    self.input_path.display(),
                    self.backup_path.display()
                )
            })?;
        }

        // Promote the converted temp file into place
        fs::rename(&self.temp_output_path, &self.final_output_path).with_context(|| {
            format!(
                "{} integration could not promote '{}' to '{}'",
                self.kind.label(),
                self.temp_output_path.display(),
                self.final_output_path.display()
            )
        })?;

        // Clean up the backup copy (best effort)
        match fs::remove_file(&self.backup_path) {
            Ok(_) => {}
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => {}
            Err(err) => {
                warn!(
                    "{} integration left a backup file at '{}': {}",
                    self.kind.label(),
                    self.backup_path.display(),
                    err
                );
            }
        }

        Ok(())
    }

    pub fn abort_on_failure(&self) -> Result<()> {
        use std::fs;

        if self.temp_output_path.exists() {
            fs::remove_file(&self.temp_output_path).with_context(|| {
                format!(
                    "{} integration failed to remove temporary file '{}'",
                    self.kind.label(),
                    self.temp_output_path.display()
                )
            })?;
        }

        if self.backup_path.exists() && !self.input_path.exists() {
            fs::rename(&self.backup_path, &self.input_path).with_context(|| {
                format!(
                    "{} integration could not restore backup '{}' to '{}'",
                    self.kind.label(),
                    self.backup_path.display(),
                    self.input_path.display()
                )
            })?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ArgsView<'a> {
    pub has_input: bool,
    pub has_output: bool,
    pub desired_extension: &'a str,
}

#[derive(Debug, Clone)]
pub enum IntegrationPreparation {
    None,
    Skip { reason: String },
    Replace(ReplacePlan),
}

pub fn prepare_from_env(view: ArgsView<'_>) -> Result<IntegrationPreparation> {
    if let Some(event) = env::var("sonarr_eventtype").ok().filter(|v| !v.is_empty()) {
        return handle_event(IntegrationKind::Sonarr, event, view);
    }

    if let Some(event) = env::var("radarr_eventtype").ok().filter(|v| !v.is_empty()) {
        return handle_event(IntegrationKind::Radarr, event, view);
    }

    Ok(IntegrationPreparation::None)
}

fn handle_event(
    kind: IntegrationKind,
    event_type: String,
    view: ArgsView<'_>,
) -> Result<IntegrationPreparation> {
    match event_type.as_str() {
        "Download" => prepare_download(kind, event_type, view),
        "Test" => Ok(IntegrationPreparation::Skip {
            reason: format!(
                "{} test event detected; exiting without conversion.",
                kind.label()
            ),
        }),
        other => Ok(IntegrationPreparation::Skip {
            reason: format!(
                "{} event '{}' does not trigger conversion; exiting cleanly.",
                kind.label(),
                other
            ),
        }),
    }
}

fn prepare_download(
    kind: IntegrationKind,
    event_type: String,
    view: ArgsView<'_>,
) -> Result<IntegrationPreparation> {
    if view.has_input && view.has_output {
        // User supplied explicit paths; allow normal CLI behaviour.
        info!(
            "{} Download event detected but CLI input/output were provided; using CLI paths.",
            kind.label()
        );
        return Ok(IntegrationPreparation::None);
    }

    if view.has_input ^ view.has_output {
        bail!(
            "Detected {} Download event but only one of <INPUT_FILE>/<OUTPUT_FILE> was provided. Provide both or rely on integration defaults.",
            kind.label()
        );
    }

    if view.has_input {
        return Ok(IntegrationPreparation::None);
    }

    let source_path = env::var(kind.episode_path_var()).with_context(|| {
        format!(
            "{} integration requires ${} to be set for Download events.",
            kind.label(),
            kind.episode_path_var()
        )
    })?;
    let input_path = PathBuf::from(source_path);

    if !input_path.exists() {
        bail!(
            "{} integration could not find media file at '{}'.",
            kind.label(),
            input_path.display()
        );
    }

    let final_output_path = resolve_output_path(&input_path, view.desired_extension)?;
    let temp_output_path = append_suffix(&final_output_path, ".direct-play-nice.tmp");
    let backup_path = append_suffix(&input_path, ".direct-play-nice.bak");

    let input_cstring = path_to_cstring(&input_path)?;
    let temp_output_cstring = path_to_cstring(&temp_output_path)?;

    let display_name = env::var(kind.title_var()).ok();
    let is_upgrade = env::var(kind.is_upgrade_var()).ok().and_then(parse_boolish);

    let plan = ReplacePlan {
        kind,
        event_type,
        display_name,
        is_upgrade,
        input_path,
        final_output_path,
        temp_output_path,
        backup_path,
        input_cstring,
        temp_output_cstring,
    };
    plan.log_summary();

    Ok(IntegrationPreparation::Replace(plan))
}

fn resolve_output_path(input_path: &Path, desired: &str) -> Result<PathBuf> {
    if desired.eq_ignore_ascii_case("match-input") || desired.eq_ignore_ascii_case("same") {
        return Ok(input_path.to_path_buf());
    }

    let trimmed = desired.trim().trim_start_matches('.');
    if trimmed.is_empty() {
        bail!(
            "Invalid --servarr-output-extension '{}': must not be empty.",
            desired
        );
    }

    Ok(input_path.with_extension(trimmed))
}

fn append_suffix(path: &Path, suffix: &str) -> PathBuf {
    let parent = path.parent();
    let filename = path
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_else(|| String::from("file"));

    let new_name = match filename.rfind('.') {
        Some(idx) => {
            let (stem, ext) = filename.split_at(idx);
            format!("{}{}{}", stem, suffix, ext)
        }
        None => format!("{}{}", filename, suffix),
    };

    match parent {
        Some(dir) => dir.join(new_name),
        None => PathBuf::from(new_name),
    }
}

fn path_to_cstring(path: &Path) -> Result<CString> {
    let path_str = path
        .to_str()
        .ok_or_else(|| anyhow!("Path contains invalid UTF-8: {}", path.display()))?;
    CString::new(path_str.as_bytes()).context("Failed to convert path to CString")
}

fn parse_boolish(value: String) -> Option<bool> {
    match value.to_ascii_lowercase().as_str() {
        "true" | "1" | "yes" | "y" => Some(true),
        "false" | "0" | "no" | "n" => Some(false),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn append_suffix_preserves_extension() {
        let base = PathBuf::from("Episode.mkv");
        let suffixed = append_suffix(&base, ".tmp");
        assert_eq!(suffixed, PathBuf::from("Episode.tmp.mkv"));
    }

    #[test]
    fn resolve_output_path_match_input() {
        let base = PathBuf::from("Movie.mkv");
        let resolved = resolve_output_path(&base, "match-input").unwrap();
        assert_eq!(resolved, base);
    }

    #[test]
    fn resolve_output_path_custom_extension() {
        let base = PathBuf::from("Movie.mkv");
        let resolved = resolve_output_path(&base, "mp4").unwrap();
        assert_eq!(resolved, PathBuf::from("Movie.mp4"));
    }
}
