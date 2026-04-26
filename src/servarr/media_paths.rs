use anyhow::{anyhow, Result};
use log::debug;
use std::path::{Path, PathBuf};

use super::{env_helpers, IntegrationKind};

struct MediaPathKeys {
    direct_keys: &'static [&'static str],
    primary_keys: &'static [&'static str],
    relative_keys: &'static [&'static str],
    source_folder_keys: &'static [&'static str],
    source_path_keys: &'static [&'static str],
    label: &'static str,
    primary_label: &'static str,
    error_message: &'static str,
}

/// Resolves Sonarr/Radarr input media paths from direct vars and fallback env combinations.
pub(super) fn resolve_media_paths(kind: IntegrationKind) -> Result<Vec<PathBuf>> {
    let env_snapshot = crate::logging::collect_relevant_env(kind);
    let keys = media_path_keys(kind);

    if let Some(paths) = env_helpers::get_env_paths(keys.direct_keys) {
        if !paths.is_empty() {
            return Ok(paths.into_iter().map(PathBuf::from).collect());
        }
    }

    let primary = env_helpers::first_non_empty(keys.primary_keys);
    let relative = env_helpers::get_env_paths(keys.relative_keys);
    let source_folder = env_helpers::first_non_empty(keys.source_folder_keys);
    let source_path = env_helpers::get_env_paths(keys.source_path_keys);

    if let Some(paths) = resolve_fallback_paths(
        keys.label,
        keys.primary_label,
        primary.as_deref(),
        relative.as_deref(),
        source_folder.as_deref(),
        source_path.as_deref(),
    ) {
        return Ok(paths);
    }

    Err(anyhow!(
        "{} Observed env: {}",
        keys.error_message,
        env_helpers::format_env_snapshot(&env_snapshot)
    ))
}

fn media_path_keys(kind: IntegrationKind) -> MediaPathKeys {
    match kind {
        IntegrationKind::Sonarr => MediaPathKeys {
            direct_keys: &["sonarr_episodefile_path", "sonarr_episodefile_paths"],
            primary_keys: &[
                "sonarr_series_path",
                "sonarr_destinationfolder",
                "sonarr_destinationpath",
            ],
            relative_keys: &[
                "sonarr_episodefile_relativepath",
                "sonarr_episodefile_relativepaths",
            ],
            source_folder_keys: &["sonarr_episodefile_sourcefolder", "sonarr_sourcefolder"],
            source_path_keys: &["sonarr_episodefile_sourcepath", "sonarr_sourcepath"],
            label: "Sonarr",
            primary_label: "series",
            error_message: "sonarr_episodefile_path and fallback variables are unavailable.",
        },
        IntegrationKind::Radarr => MediaPathKeys {
            direct_keys: &["radarr_moviefile_path", "radarr_moviefile_paths"],
            primary_keys: &[
                "radarr_movie_path",
                "radarr_destinationfolder",
                "radarr_destinationpath",
            ],
            relative_keys: &[
                "radarr_moviefile_relativepath",
                "radarr_moviefile_relativepaths",
            ],
            source_folder_keys: &["radarr_moviefile_sourcefolder", "radarr_sourcefolder"],
            source_path_keys: &["radarr_moviefile_sourcepath", "radarr_sourcepath"],
            label: "Radarr",
            primary_label: "movie",
            error_message: "radarr_moviefile_path and fallback variables are unavailable.",
        },
    }
}

fn resolve_fallback_paths(
    integration_label: &str,
    primary_label: &str,
    primary_root: Option<&str>,
    relative_paths: Option<&[String]>,
    source_folder: Option<&str>,
    source_paths: Option<&[String]>,
) -> Option<Vec<PathBuf>> {
    if let (Some(root), Some(rel_list)) = (primary_root, relative_paths) {
        let joined = join_root_and_relative(integration_label, primary_label, root, rel_list);
        if !joined.is_empty() {
            return Some(joined);
        }
    }

    if let (Some(root), Some(source_list)) = (primary_root, source_paths) {
        let joined = join_root_and_source_file(integration_label, primary_label, root, source_list);
        if !joined.is_empty() {
            return Some(joined);
        }
    }

    if let (Some(folder), Some(rel_list)) = (source_folder, relative_paths) {
        let joined = join_root_and_relative(integration_label, "source folder", folder, rel_list);
        if !joined.is_empty() {
            return Some(joined);
        }
    }

    if let (Some(folder), Some(source_list)) = (source_folder, source_paths) {
        let joined =
            join_root_and_source_file(integration_label, "source folder", folder, source_list);
        if !joined.is_empty() {
            return Some(joined);
        }
    }

    if let Some(source_list) = source_paths {
        let collected = collect_source_paths(integration_label, source_list);
        if !collected.is_empty() {
            return Some(collected);
        }
    }

    None
}

fn join_root_and_relative(
    integration_label: &str,
    base_label: &str,
    base: &str,
    relative_paths: &[String],
) -> Vec<PathBuf> {
    let mut joined = Vec::new();
    for rel in relative_paths {
        if base.trim().is_empty() || rel.trim().is_empty() {
            continue;
        }
        let candidate = Path::new(base).join(rel);
        debug!(
            "{} fallback path resolved via {}/relative: {} + {}",
            integration_label, base_label, base, rel
        );
        joined.push(candidate);
    }
    joined
}

fn join_root_and_source_file(
    integration_label: &str,
    base_label: &str,
    base: &str,
    source_paths: &[String],
) -> Vec<PathBuf> {
    let mut joined = Vec::new();
    for source in source_paths {
        if base.trim().is_empty() || source.trim().is_empty() {
            continue;
        }
        let Some(file_name) = Path::new(source).file_name() else {
            continue;
        };
        let candidate = Path::new(base).join(file_name);
        debug!(
            "{} fallback path resolved via {}/source file: {} + {:?}",
            integration_label, base_label, base, file_name
        );
        joined.push(candidate);
    }
    joined
}

fn collect_source_paths(integration_label: &str, source_paths: &[String]) -> Vec<PathBuf> {
    let mut collected = Vec::new();
    for source in source_paths {
        if source.trim().is_empty() {
            continue;
        }
        debug!(
            "{} fallback path resolved via source path: {}",
            integration_label, source
        );
        collected.push(PathBuf::from(source));
    }
    collected
}
