use anyhow::{anyhow, Result};
use log::debug;
use std::path::{Path, PathBuf};

use super::{env_helpers, IntegrationKind};

pub(super) fn resolve_media_paths(kind: IntegrationKind) -> Result<Vec<PathBuf>> {
    let env_snapshot = crate::logging::collect_relevant_env(kind);

    if let Some(paths) = match kind {
        IntegrationKind::Sonarr => {
            env_helpers::get_env_paths(&["sonarr_episodefile_path", "sonarr_episodefile_paths"])
        }
        IntegrationKind::Radarr => {
            env_helpers::get_env_paths(&["radarr_moviefile_path", "radarr_moviefile_paths"])
        }
    } {
        if !paths.is_empty() {
            return Ok(paths.into_iter().map(PathBuf::from).collect());
        }
    }

    match kind {
        IntegrationKind::Sonarr => {
            let series = env_helpers::first_non_empty(&[
                "sonarr_series_path",
                "sonarr_destinationfolder",
                "sonarr_destinationpath",
            ]);
            let relative = env_helpers::get_env_paths(&[
                "sonarr_episodefile_relativepath",
                "sonarr_episodefile_relativepaths",
            ]);
            let source_folder =
                env_helpers::first_non_empty(&["sonarr_episodefile_sourcefolder", "sonarr_sourcefolder"]);
            let source_path =
                env_helpers::get_env_paths(&["sonarr_episodefile_sourcepath", "sonarr_sourcepath"]);

            if let (Some(series), Some(rel_list)) = (series.as_deref(), relative.clone()) {
                let mut joined = Vec::new();
                for rel in rel_list {
                    if !series.trim().is_empty() && !rel.trim().is_empty() {
                        let candidate = Path::new(series).join(&rel);
                        debug!(
                            "Sonarr fallback path resolved via series/relative: {} + {}",
                            series, rel
                        );
                        joined.push(candidate);
                    }
                }
                if !joined.is_empty() {
                    return Ok(joined);
                }
            }

            if let (Some(series), Some(source_list)) = (series.as_deref(), source_path.clone()) {
                let mut joined = Vec::new();
                for source in source_list {
                    if !series.trim().is_empty() && !source.trim().is_empty() {
                        if let Some(file_name) = Path::new(&source).file_name() {
                            let candidate = Path::new(series).join(file_name);
                            debug!(
                                "Sonarr fallback path resolved via series/source file: {} + {:?}",
                                series, file_name
                            );
                            joined.push(candidate);
                        }
                    }
                }
                if !joined.is_empty() {
                    return Ok(joined);
                }
            }

            if let (Some(folder), Some(rel_list)) = (source_folder.as_deref(), relative.clone()) {
                let mut joined = Vec::new();
                for rel in rel_list {
                    if !folder.trim().is_empty() && !rel.trim().is_empty() {
                        let candidate = Path::new(folder).join(&rel);
                        debug!(
                            "Sonarr fallback path resolved via source folder/relative: {} + {}",
                            folder, rel
                        );
                        joined.push(candidate);
                    }
                }
                if !joined.is_empty() {
                    return Ok(joined);
                }
            }

            if let (Some(folder), Some(source_list)) =
                (source_folder.as_deref(), source_path.clone())
            {
                let mut joined = Vec::new();
                for source in source_list {
                    if !folder.trim().is_empty() && !source.trim().is_empty() {
                        if let Some(file_name) = Path::new(&source).file_name() {
                            let candidate = Path::new(folder).join(file_name);
                            debug!(
                                "Sonarr fallback path resolved via source folder/file: {} + {:?}",
                                folder, file_name
                            );
                            joined.push(candidate);
                        }
                    }
                }
                if !joined.is_empty() {
                    return Ok(joined);
                }
            }

            if let Some(source_list) = source_path {
                let mut collected = Vec::new();
                for source in source_list {
                    if !source.trim().is_empty() {
                        debug!("Sonarr fallback path resolved via source path: {}", source);
                        collected.push(PathBuf::from(source));
                    }
                }
                if !collected.is_empty() {
                    return Ok(collected);
                }
            }

            Err(anyhow!(
                "sonarr_episodefile_path and fallback variables are unavailable. Observed env: {}",
                env_helpers::format_env_snapshot(&env_snapshot)
            ))
        }
        IntegrationKind::Radarr => {
            let movie = env_helpers::first_non_empty(&[
                "radarr_movie_path",
                "radarr_destinationfolder",
                "radarr_destinationpath",
            ]);
            let relative = env_helpers::get_env_paths(&[
                "radarr_moviefile_relativepath",
                "radarr_moviefile_relativepaths",
            ]);
            let source_folder =
                env_helpers::first_non_empty(&["radarr_moviefile_sourcefolder", "radarr_sourcefolder"]);
            let source_path =
                env_helpers::get_env_paths(&["radarr_moviefile_sourcepath", "radarr_sourcepath"]);

            if let (Some(movie), Some(rel_list)) = (movie.as_deref(), relative.clone()) {
                let mut joined = Vec::new();
                for rel in rel_list {
                    if !movie.trim().is_empty() && !rel.trim().is_empty() {
                        let candidate = Path::new(movie).join(&rel);
                        debug!(
                            "Radarr fallback path resolved via movie/relative: {} + {}",
                            movie, rel
                        );
                        joined.push(candidate);
                    }
                }
                if !joined.is_empty() {
                    return Ok(joined);
                }
            }

            if let (Some(movie), Some(source_list)) = (movie.as_deref(), source_path.clone()) {
                let mut joined = Vec::new();
                for source in source_list {
                    if !movie.trim().is_empty() && !source.trim().is_empty() {
                        if let Some(file_name) = Path::new(&source).file_name() {
                            let candidate = Path::new(movie).join(file_name);
                            debug!(
                                "Radarr fallback path resolved via movie/source file: {} + {:?}",
                                movie, file_name
                            );
                            joined.push(candidate);
                        }
                    }
                }
                if !joined.is_empty() {
                    return Ok(joined);
                }
            }

            if let (Some(folder), Some(rel_list)) = (source_folder.as_deref(), relative.clone()) {
                let mut joined = Vec::new();
                for rel in rel_list {
                    if !folder.trim().is_empty() && !rel.trim().is_empty() {
                        let candidate = Path::new(folder).join(&rel);
                        debug!(
                            "Radarr fallback path resolved via source folder/relative: {} + {}",
                            folder, rel
                        );
                        joined.push(candidate);
                    }
                }
                if !joined.is_empty() {
                    return Ok(joined);
                }
            }

            if let (Some(folder), Some(source_list)) =
                (source_folder.as_deref(), source_path.clone())
            {
                let mut joined = Vec::new();
                for source in source_list {
                    if !folder.trim().is_empty() && !source.trim().is_empty() {
                        if let Some(file_name) = Path::new(&source).file_name() {
                            let candidate = Path::new(folder).join(file_name);
                            debug!(
                                "Radarr fallback path resolved via source folder/file: {} + {:?}",
                                folder, file_name
                            );
                            joined.push(candidate);
                        }
                    }
                }
                if !joined.is_empty() {
                    return Ok(joined);
                }
            }

            if let Some(source_list) = source_path {
                let mut collected = Vec::new();
                for source in source_list {
                    if !source.trim().is_empty() {
                        debug!("Radarr fallback path resolved via source path: {}", source);
                        collected.push(PathBuf::from(source));
                    }
                }
                if !collected.is_empty() {
                    return Ok(collected);
                }
            }

            Err(anyhow!(
                "radarr_moviefile_path and fallback variables are unavailable. Observed env: {}",
                env_helpers::format_env_snapshot(&env_snapshot)
            ))
        }
    }
}
