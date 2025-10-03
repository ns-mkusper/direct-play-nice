use anyhow::{anyhow, bail, Context, Result};
use log::{debug, info, warn};
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
    pub desired_suffix: &'a str,
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

    let input_path = resolve_media_path(kind).with_context(|| {
        format!(
            "{} integration requires ${} to be set for Download events.",
            kind.label(),
            kind.episode_path_var()
        )
    })?;

    if !input_path.exists() {
        bail!(
            "{} integration could not find media file at '{}'.",
            kind.label(),
            input_path.display()
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

    let final_output_path =
        resolve_output_path(&input_path, view.desired_extension, effective_suffix)?;
    let temp_output_path = append_suffix(&final_output_path, ".direct-play-nice.tmp");
    let backup_path = append_suffix(&input_path, ".direct-play-nice.bak");

    let input_cstring = path_to_cstring(&input_path)?;
    let temp_output_cstring = path_to_cstring(&temp_output_path)?;

    let display_name = get_env_ignore_case(kind.title_var());
    let is_upgrade = get_env_ignore_case(kind.is_upgrade_var()).and_then(parse_boolish);

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

fn resolve_output_path(
    input_path: &Path,
    desired_ext: &str,
    desired_suffix: &str,
) -> Result<PathBuf> {
    let parent = input_path.parent();
    let stem = input_path
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or_else(|| {
            anyhow!(
                "Input file name is not valid UTF-8: {}",
                input_path.display()
            )
        })?;

    let suffix = normalize_suffix(desired_suffix);

    let final_extension = if desired_ext.eq_ignore_ascii_case("match-input")
        || desired_ext.eq_ignore_ascii_case("same")
    {
        input_path
            .extension()
            .and_then(|ext| ext.to_str())
            .map(|s| s.to_string())
            .unwrap_or_default()
    } else {
        let trimmed = desired_ext.trim().trim_start_matches('.');
        if trimmed.is_empty() {
            bail!(
                "Invalid --servarr-output-extension '{}': must not be empty.",
                desired_ext
            );
        }
        trimmed.to_string()
    };

    let mut filename = String::from(stem);
    if let Some(sfx) = suffix.as_ref() {
        filename.push_str(sfx);
    }
    if !final_extension.is_empty() {
        filename.push('.');
        filename.push_str(&final_extension);
    }

    let new_path = match parent {
        Some(dir) => dir.join(filename),
        None => PathBuf::from(filename),
    };

    Ok(new_path)
}

fn normalize_suffix(raw: &str) -> Option<String> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return None;
    }
    if trimmed.starts_with('.') {
        Some(trimmed.to_string())
    } else {
        Some(format!(".{}", trimmed))
    }
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

fn resolve_media_path(kind: IntegrationKind) -> Result<PathBuf> {
    let env_snapshot = crate::logging::collect_relevant_env(kind);

    if let Some(path) = match kind {
        IntegrationKind::Sonarr => {
            get_env_path(&["sonarr_episodefile_path", "sonarr_episodefile_paths"])
        }
        IntegrationKind::Radarr => {
            get_env_path(&["radarr_moviefile_path", "radarr_moviefile_paths"])
        }
    } {
        return Ok(PathBuf::from(path));
    }

    match kind {
        IntegrationKind::Sonarr => {
            let series = first_non_empty(&[
                "sonarr_series_path",
                "sonarr_destinationfolder",
                "sonarr_destinationpath",
            ]);
            let relative = first_non_empty(&[
                "sonarr_episodefile_relativepath",
                "sonarr_episodefile_relativepaths",
            ]);
            let source_folder =
                first_non_empty(&["sonarr_episodefile_sourcefolder", "sonarr_sourcefolder"]);
            let source_path = get_env_path(&["sonarr_episodefile_sourcepath", "sonarr_sourcepath"]);
            if let (Some(series), Some(rel)) = (series.as_deref(), relative.as_deref()) {
                if !series.trim().is_empty() && !rel.trim().is_empty() {
                    let joined = Path::new(series).join(rel);
                    debug!(
                        "Sonarr fallback path resolved via series/relative: {} + {}",
                        series, rel
                    );
                    return Ok(joined);
                }
            }

            if let (Some(series), Some(source)) = (series.as_deref(), source_path.as_deref()) {
                if !series.trim().is_empty() && !source.trim().is_empty() {
                    if let Some(file_name) = Path::new(source).file_name() {
                        let joined = Path::new(series).join(file_name);
                        debug!(
                            "Sonarr fallback path resolved via series/source file: {} + {:?}",
                            series, file_name
                        );
                        return Ok(joined);
                    }
                }
            }

            if let (Some(folder), Some(rel)) = (source_folder.as_deref(), relative.as_deref()) {
                if !folder.trim().is_empty() && !rel.trim().is_empty() {
                    let joined = Path::new(folder).join(rel);
                    debug!(
                        "Sonarr fallback path resolved via source folder/relative: {} + {}",
                        folder, rel
                    );
                    return Ok(joined);
                }
            }

            if let (Some(folder), Some(source)) = (source_folder.as_deref(), source_path.as_deref())
            {
                if !folder.trim().is_empty() && !source.trim().is_empty() {
                    if let Some(file_name) = Path::new(source).file_name() {
                        let joined = Path::new(folder).join(file_name);
                        debug!(
                            "Sonarr fallback path resolved via source folder/file: {} + {:?}",
                            folder, file_name
                        );
                        return Ok(joined);
                    }
                }
            }

            if let Some(source) = source_path {
                if !source.trim().is_empty() {
                    debug!("Sonarr fallback path resolved via source path: {}", source);
                    return Ok(PathBuf::from(source));
                }
            }

            Err(anyhow!(
                "sonarr_episodefile_path and fallback variables are unavailable. Observed env: {}",
                format_env_snapshot(&env_snapshot)
            ))
        }
        IntegrationKind::Radarr => {
            let movie = first_non_empty(&[
                "radarr_movie_path",
                "radarr_destinationfolder",
                "radarr_destinationpath",
            ]);
            let relative = first_non_empty(&[
                "radarr_moviefile_relativepath",
                "radarr_moviefile_relativepaths",
            ]);
            let source_folder =
                first_non_empty(&["radarr_moviefile_sourcefolder", "radarr_sourcefolder"]);
            let source_path = get_env_path(&["radarr_moviefile_sourcepath", "radarr_sourcepath"]);
            if let (Some(movie), Some(rel)) = (movie.as_deref(), relative.as_deref()) {
                if !movie.trim().is_empty() && !rel.trim().is_empty() {
                    let joined = Path::new(movie).join(rel);
                    debug!(
                        "Radarr fallback path resolved via movie/relative: {} + {}",
                        movie, rel
                    );
                    return Ok(joined);
                }
            }

            if let (Some(movie), Some(source)) = (movie.as_deref(), source_path.as_deref()) {
                if !movie.trim().is_empty() && !source.trim().is_empty() {
                    if let Some(file_name) = Path::new(source).file_name() {
                        let joined = Path::new(movie).join(file_name);
                        debug!(
                            "Radarr fallback path resolved via movie/source file: {} + {:?}",
                            movie, file_name
                        );
                        return Ok(joined);
                    }
                }
            }

            if let (Some(folder), Some(rel)) = (source_folder.as_deref(), relative.as_deref()) {
                if !folder.trim().is_empty() && !rel.trim().is_empty() {
                    let joined = Path::new(folder).join(rel);
                    debug!(
                        "Radarr fallback path resolved via source folder/relative: {} + {}",
                        folder, rel
                    );
                    return Ok(joined);
                }
            }

            if let (Some(folder), Some(source)) = (source_folder.as_deref(), source_path.as_deref())
            {
                if !folder.trim().is_empty() && !source.trim().is_empty() {
                    if let Some(file_name) = Path::new(source).file_name() {
                        let joined = Path::new(folder).join(file_name);
                        debug!(
                            "Radarr fallback path resolved via source folder/file: {} + {:?}",
                            folder, file_name
                        );
                        return Ok(joined);
                    }
                }
            }

            if let Some(source) = source_path {
                if !source.trim().is_empty() {
                    debug!("Radarr fallback path resolved via source path: {}", source);
                    return Ok(PathBuf::from(source));
                }
            }

            Err(anyhow!(
                "radarr_moviefile_path and fallback variables are unavailable. Observed env: {}",
                format_env_snapshot(&env_snapshot)
            ))
        }
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

fn get_env_ignore_case(key: &str) -> Option<String> {
    if let Ok(val) = env::var(key) {
        return Some(val);
    }

    let target = key.to_ascii_lowercase();
    for (k, v) in env::vars() {
        if k.to_ascii_lowercase() == target {
            return Some(v);
        }
    }
    None
}

fn format_env_snapshot(entries: &[(String, String)]) -> String {
    if entries.is_empty() {
        return "<none>".to_string();
    }

    entries
        .iter()
        .map(|(k, v)| format!("{}={}", k, v))
        .collect::<Vec<_>>()
        .join(", ")
}

fn first_non_empty(keys: &[&str]) -> Option<String> {
    for key in keys {
        if let Some(val) = get_env_ignore_case(key) {
            let trimmed = val.trim();
            if !trimmed.is_empty() {
                return Some(trimmed.to_string());
            }
        }
    }
    None
}

fn get_env_path(keys: &[&str]) -> Option<String> {
    for key in keys {
        if let Some(val) = get_env_ignore_case(key) {
            let trimmed = val.trim();
            if trimmed.is_empty() {
                continue;
            }
            let first = trimmed.split('|').next().unwrap().trim();
            if !first.is_empty() {
                return Some(first.to_string());
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;
    use tempfile::tempdir;

    static ENV_MUTEX: Mutex<()> = Mutex::new(());

    #[test]
    fn append_suffix_preserves_extension() {
        let base = PathBuf::from("Episode.mkv");
        let suffixed = append_suffix(&base, ".tmp");
        assert_eq!(suffixed, PathBuf::from("Episode.tmp.mkv"));
    }

    #[test]
    fn resolve_output_path_match_input() {
        let base = PathBuf::from("Movie.mkv");
        let resolved = resolve_output_path(&base, "match-input", "").unwrap();
        assert_eq!(resolved, base);
    }

    #[test]
    fn resolve_output_path_custom_extension() {
        let base = PathBuf::from("Movie.mkv");
        let resolved = resolve_output_path(&base, "mp4", "").unwrap();
        assert_eq!(resolved, PathBuf::from("Movie.mp4"));
    }

    #[test]
    fn resolve_output_path_with_suffix_and_extension() {
        let base = PathBuf::from("Movie.mkv");
        let resolved = resolve_output_path(&base, "mp4", ".fixed").unwrap();
        assert_eq!(resolved, PathBuf::from("Movie.fixed.mp4"));
    }

    #[test]
    fn resolve_output_path_with_suffix_and_match_input() {
        let base = PathBuf::from("Episode.mkv");
        let resolved = resolve_output_path(&base, "match-input", "fixed").unwrap();
        assert_eq!(resolved, PathBuf::from("Episode.fixed.mkv"));
    }

    #[test]
    fn resolve_media_path_uses_sonarr_fallback() {
        let _guard = ENV_MUTEX.lock().unwrap();
        env::remove_var("sonarr_episodefile_path");
        env::remove_var("sonarr_episodefile_paths");
        env::set_var("sonarr_series_path", "/tmp/show");
        env::set_var("sonarr_episodefile_relativepath", "Season 1/episode.mkv");

        let resolved = resolve_media_path(IntegrationKind::Sonarr).unwrap();
        assert_eq!(
            resolved,
            Path::new("/tmp/show").join("Season 1/episode.mkv")
        );

        env::remove_var("sonarr_episodefile_relativepath");
        env::remove_var("sonarr_series_path");
    }

    #[test]
    fn resolve_media_path_uses_sonarr_source_only() {
        let _guard = ENV_MUTEX.lock().unwrap();
        env::remove_var("sonarr_episodefile_path");
        env::remove_var("sonarr_episodefile_relativepath");
        env::remove_var("sonarr_series_path");
        env::set_var("sonarr_episodefile_sourcepath", "/tmp/source/episode.mkv");

        let resolved = resolve_media_path(IntegrationKind::Sonarr).unwrap();
        assert_eq!(resolved, PathBuf::from("/tmp/source/episode.mkv"));

        env::remove_var("sonarr_episodefile_sourcepath");
    }

    #[test]
    fn resolve_media_path_uses_sonarr_episodefile_paths() {
        let _guard = ENV_MUTEX.lock().unwrap();
        env::remove_var("sonarr_episodefile_path");
        env::set_var("sonarr_episodefile_paths", "/tmp/show/Season 1/episode.mkv");

        let resolved = resolve_media_path(IntegrationKind::Sonarr).unwrap();
        assert_eq!(resolved, PathBuf::from("/tmp/show/Season 1/episode.mkv"));

        env::remove_var("sonarr_episodefile_paths");
    }

    #[test]
    fn resolve_media_path_uses_radarr_fallback() {
        let _guard = ENV_MUTEX.lock().unwrap();
        env::remove_var("radarr_moviefile_path");
        env::remove_var("radarr_moviefile_paths");
        env::set_var("radarr_movie_path", "/tmp/movie");
        env::set_var("radarr_moviefile_relativepath", "movie.mkv");

        let resolved = resolve_media_path(IntegrationKind::Radarr).unwrap();
        assert_eq!(resolved, Path::new("/tmp/movie").join("movie.mkv"));

        env::remove_var("radarr_moviefile_relativepath");
        env::remove_var("radarr_movie_path");
    }

    #[test]
    fn resolve_media_path_uses_radarr_source_only() {
        let _guard = ENV_MUTEX.lock().unwrap();
        env::remove_var("radarr_moviefile_path");
        env::remove_var("radarr_moviefile_relativepath");
        env::remove_var("radarr_movie_path");
        env::set_var("radarr_moviefile_sourcepath", "/tmp/source/movie.mkv");

        let resolved = resolve_media_path(IntegrationKind::Radarr).unwrap();
        assert_eq!(resolved, PathBuf::from("/tmp/source/movie.mkv"));

        env::remove_var("radarr_moviefile_sourcepath");
    }

    #[test]
    fn resolve_media_path_uses_radarr_moviefile_paths() {
        let _guard = ENV_MUTEX.lock().unwrap();
        env::remove_var("radarr_moviefile_path");
        env::set_var("radarr_moviefile_paths", "/tmp/movie/movie.mkv");

        let resolved = resolve_media_path(IntegrationKind::Radarr).unwrap();
        assert_eq!(resolved, PathBuf::from("/tmp/movie/movie.mkv"));

        env::remove_var("radarr_moviefile_paths");
    }

    #[test]
    fn sonarr_default_suffix_is_fixed() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let tmp = tempdir().unwrap();
        let input = tmp.path().join("Episode.mkv");
        std::fs::write(&input, b"dummy").unwrap();

        env::set_var("sonarr_eventtype", "Download");
        env::set_var("sonarr_episodefile_path", &input);

        let view = ArgsView {
            has_input: false,
            has_output: false,
            desired_extension: "mp4",
            desired_suffix: "",
        };

        let prep = prepare_from_env(view).unwrap();
        let IntegrationPreparation::Replace(plan) = prep else {
            panic!("expected replace plan");
        };
        let file_name = plan
            .final_output_path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap();
        assert_eq!(file_name, "Episode.fixed.mp4");

        env::remove_var("sonarr_eventtype");
        env::remove_var("sonarr_episodefile_path");
    }

    #[test]
    fn radarr_default_suffix_is_empty() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let tmp = tempdir().unwrap();
        let input = tmp.path().join("Movie.mkv");
        std::fs::write(&input, b"dummy").unwrap();

        env::set_var("radarr_eventtype", "Download");
        env::set_var("radarr_moviefile_path", &input);

        let view = ArgsView {
            has_input: false,
            has_output: false,
            desired_extension: "mp4",
            desired_suffix: "",
        };

        let prep = prepare_from_env(view).unwrap();
        let IntegrationPreparation::Replace(plan) = prep else {
            panic!("expected replace plan");
        };
        let file_name = plan
            .final_output_path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap();
        assert_eq!(file_name, "Movie.mp4");

        env::remove_var("radarr_eventtype");
        env::remove_var("radarr_moviefile_path");
    }
}
