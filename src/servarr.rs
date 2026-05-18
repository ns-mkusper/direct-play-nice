//! Sonarr/Radarr integration workflow for event handling, path planning, and atomic media replacement.

use anyhow::{bail, Context, Result};
use log::{info, warn};
use std::env;
use std::ffi::CString;
use std::path::{Path, PathBuf};

mod api;
mod env_helpers;
mod language;
mod media_paths;
mod path_policy;

pub use api::ApiSettings;
pub use language::{parse_language_list, LanguageRequirements};

#[cfg(test)]
mod servarr_tests;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Supported Servarr integration sources.
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
/// Fully-resolved atomic replacement plan for one media file.
///
/// Replacement is always staged through `temp_output_path` and `backup_path` so
/// failures can roll back to the original source file.
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

    pub fn finalize_success(self) -> Result<PathBuf> {
        use std::fs;

        let had_original = self.input_path.exists();

        // Move the original aside first, so final promotion can be an atomic rename.
        if had_original {
            fs::rename(&self.input_path, &self.backup_path).with_context(|| {
                format!(
                    "{} integration failed to move original file '{}' to backup '{}'",
                    self.kind.label(),
                    self.input_path.display(),
                    self.backup_path.display()
                )
            })?;
        }

        // Promote the converted temp file into place; restore backup on failure.
        if let Err(promote_err) = fs::rename(&self.temp_output_path, &self.final_output_path) {
            if had_original && self.backup_path.exists() && !self.input_path.exists() {
                if let Err(restore_err) = fs::rename(&self.backup_path, &self.input_path) {
                    return Err(promote_err).with_context(|| {
                        format!(
                            "{} integration could not promote '{}' to '{}' and failed to restore backup '{}': {}",
                            self.kind.label(),
                            self.temp_output_path.display(),
                            self.final_output_path.display(),
                            self.input_path.display(),
                            restore_err
                        )
                    });
                }
            }
            return Err(promote_err).with_context(|| {
                format!(
                    "{} integration could not promote '{}' to '{}'",
                    self.kind.label(),
                    self.temp_output_path.display(),
                    self.final_output_path.display()
                )
            });
        }

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

        Ok(self.final_output_path)
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

#[derive(Debug, Clone)]
/// Borrowed view of CLI path-related arguments used by Servarr planning.
pub struct ArgsView<'a> {
    pub has_input: bool,
    pub has_output: bool,
    pub desired_extension: &'a str,
    pub desired_suffix: &'a str,
    pub language_requirements: LanguageRequirements,
    pub api_settings: ApiSettings,
}

#[derive(Debug, Clone)]
/// Preparation result for integration-triggered runs.
pub enum IntegrationPreparation {
    None,
    Skip { reason: String },
    Replace(ReplacePlan),
    Batch(Vec<ReplacePlan>),
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
    crate::logging::log_relevant_env(kind);

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

    let input_paths = resolve_media_paths(kind).with_context(|| {
        format!(
            "{} integration requires ${} to be set for Download events.",
            kind.label(),
            kind.episode_path_var()
        )
    })?;

    if input_paths.is_empty() {
        bail!(
            "{} integration did not receive any media file paths.",
            kind.label()
        );
    }

    let effective_suffix = if view.desired_suffix.trim().is_empty() {
        match kind {
            IntegrationKind::Sonarr => ".fixed",
            IntegrationKind::Radarr => "",
        }
    } else {
        view.desired_suffix
    };

    let display_name = get_env_ignore_case(kind.title_var());
    let is_upgrade = get_env_ignore_case(kind.is_upgrade_var()).and_then(parse_boolish);

    if view.language_requirements.enabled
        && view.language_requirements.audio.is_empty()
        && view.language_requirements.subtitles.is_empty()
    {
        warn!(
            "{} language check is enabled but no required audio/subtitle languages were configured; continuing conversion.",
            kind.label()
        );
    }

    let mut language_mismatches = Vec::new();
    if view.language_requirements.is_effective() {
        for input_path in &input_paths {
            let report = language::check_file(input_path, &view.language_requirements)
                .with_context(|| format!("checking languages for '{}'", input_path.display()))?;
            if !report.satisfied() {
                language_mismatches.push((input_path.clone(), report));
            }
        }
    }

    if !language_mismatches.is_empty() {
        for (path, report) in &language_mismatches {
            warn!(
                "{} language requirements not met for '{}': missing {} (present audio [{}], subtitles [{}]).",
                kind.label(),
                path.display(),
                report.describe_missing(),
                report.present_audio.join(", "),
                report.present_subtitles.join(", ")
            );
        }
        match api::trigger_verified_redownload(
            kind,
            &view.api_settings,
            &view.language_requirements,
        )? {
            api::RedownloadOutcome::Grabbed { title } => {
                return Ok(IntegrationPreparation::Skip {
                    reason: format!(
                        "{} language requirements were not met; blacklisted the current download and grabbed verified replacement '{}'.",
                        kind.label(), title
                    ),
                });
            }
            api::RedownloadOutcome::NoVerifiedRelease { reason } => {
                return Ok(IntegrationPreparation::Skip {
                    reason: format!(
                        "{} language requirements were not met, but no replacement was grabbed: {}. Leaving current file untouched.",
                        kind.label(), reason
                    ),
                });
            }
        }
    }

    let mut plans = Vec::new();

    for input_path in input_paths {
        if !input_path.exists() {
            bail!(
                "{} integration could not find media file at '{}'.",
                kind.label(),
                input_path.display()
            );
        }

        let final_output_path =
            resolve_output_path(&input_path, view.desired_extension, effective_suffix)?;
        let temp_output_path = append_suffix(&final_output_path, ".direct-play-nice.tmp");
        let backup_path = append_suffix(&input_path, ".direct-play-nice.bak");

        let input_cstring = path_to_cstring(&input_path)?;
        let temp_output_cstring = path_to_cstring(&temp_output_path)?;

        let plan = ReplacePlan {
            kind,
            event_type: event_type.clone(),
            display_name: display_name.clone(),
            is_upgrade,
            input_path,
            final_output_path,
            temp_output_path,
            backup_path,
            input_cstring,
            temp_output_cstring,
        };
        plan.log_summary();
        plans.push(plan);
    }

    if plans.len() == 1 {
        Ok(IntegrationPreparation::Replace(plans.remove(0)))
    } else {
        Ok(IntegrationPreparation::Batch(plans))
    }
}

fn resolve_output_path(
    input_path: &Path,
    desired_ext: &str,
    desired_suffix: &str,
) -> Result<PathBuf> {
    path_policy::resolve_output_path(input_path, desired_ext, desired_suffix)
}

fn append_suffix(path: &Path, suffix: &str) -> PathBuf {
    path_policy::append_suffix(path, suffix)
}

fn resolve_media_paths(kind: IntegrationKind) -> Result<Vec<PathBuf>> {
    media_paths::resolve_media_paths(kind)
}

fn path_to_cstring(path: &Path) -> Result<CString> {
    path_policy::path_to_cstring(path)
}

fn parse_boolish(value: String) -> Option<bool> {
    env_helpers::parse_boolish(value)
}

fn get_env_ignore_case(key: &str) -> Option<String> {
    env_helpers::get_env_ignore_case(key)
}
