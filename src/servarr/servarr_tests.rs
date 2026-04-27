#[cfg(test)]
mod tests {
    use crate::servarr::*;
    use std::ffi::CString;
    use std::sync::Mutex;
    use tempfile::tempdir;

    static ENV_MUTEX: Mutex<()> = Mutex::new(());

    #[test]
    /// Runs the append suffix preserves extension operation.
    fn append_suffix_preserves_extension() {
        let base = PathBuf::from("Episode.mkv");
        let suffixed = append_suffix(&base, ".tmp");
        assert_eq!(suffixed, PathBuf::from("Episode.tmp.mkv"));
    }

    #[test]
    /// Runs the resolve output path match input operation.
    fn resolve_output_path_match_input() {
        let base = PathBuf::from("Movie.mkv");
        let resolved = resolve_output_path(&base, "match-input", "").unwrap();
        assert_eq!(resolved, base);
    }

    #[test]
    /// Runs the resolve output path custom extension operation.
    fn resolve_output_path_custom_extension() {
        let base = PathBuf::from("Movie.mkv");
        let resolved = resolve_output_path(&base, "mp4", "").unwrap();
        assert_eq!(resolved, PathBuf::from("Movie.mp4"));
    }

    #[test]
    /// Runs the resolve output path with suffix and extension operation.
    fn resolve_output_path_with_suffix_and_extension() {
        let base = PathBuf::from("Movie.mkv");
        let resolved = resolve_output_path(&base, "mp4", ".fixed").unwrap();
        assert_eq!(resolved, PathBuf::from("Movie.fixed.mp4"));
    }

    #[test]
    /// Runs the resolve output path with suffix and match input operation.
    fn resolve_output_path_with_suffix_and_match_input() {
        let base = PathBuf::from("Episode.mkv");
        let resolved = resolve_output_path(&base, "match-input", "fixed").unwrap();
        assert_eq!(resolved, PathBuf::from("Episode.fixed.mkv"));
    }

    #[test]
    /// Runs the resolve media path uses sonarr fallback operation.
    fn resolve_media_path_uses_sonarr_fallback() {
        let _guard = ENV_MUTEX.lock().unwrap();
        env::remove_var("sonarr_episodefile_path");
        env::remove_var("sonarr_episodefile_paths");
        env::set_var("sonarr_series_path", "/tmp/show");
        env::set_var("sonarr_episodefile_relativepath", "Season 1/episode.mkv");

        let resolved = resolve_media_paths(IntegrationKind::Sonarr)
            .unwrap()
            .into_iter()
            .next()
            .unwrap();
        assert_eq!(
            resolved,
            Path::new("/tmp/show").join("Season 1/episode.mkv")
        );

        env::remove_var("sonarr_episodefile_relativepath");
        env::remove_var("sonarr_series_path");
    }

    #[test]
    /// Runs the resolve media path uses sonarr source only operation.
    fn resolve_media_path_uses_sonarr_source_only() {
        let _guard = ENV_MUTEX.lock().unwrap();
        env::remove_var("sonarr_episodefile_path");
        env::remove_var("sonarr_episodefile_relativepath");
        env::remove_var("sonarr_series_path");
        env::set_var("sonarr_episodefile_sourcepath", "/tmp/source/episode.mkv");

        let resolved = resolve_media_paths(IntegrationKind::Sonarr)
            .unwrap()
            .into_iter()
            .next()
            .unwrap();
        assert_eq!(resolved, PathBuf::from("/tmp/source/episode.mkv"));

        env::remove_var("sonarr_episodefile_sourcepath");
    }

    #[test]
    /// Runs the resolve media path uses sonarr episodefile paths operation.
    fn resolve_media_path_uses_sonarr_episodefile_paths() {
        let _guard = ENV_MUTEX.lock().unwrap();
        env::remove_var("sonarr_episodefile_path");
        env::set_var("sonarr_episodefile_paths", "/tmp/show/Season 1/episode.mkv");

        let resolved = resolve_media_paths(IntegrationKind::Sonarr)
            .unwrap()
            .into_iter()
            .next()
            .unwrap();
        assert_eq!(resolved, PathBuf::from("/tmp/show/Season 1/episode.mkv"));

        env::remove_var("sonarr_episodefile_paths");
    }

    #[test]
    /// Runs the resolve media paths handles multiple sonarr entries operation.
    fn resolve_media_paths_handles_multiple_sonarr_entries() {
        let _guard = ENV_MUTEX.lock().unwrap();
        env::remove_var("sonarr_episodefile_path");
        env::set_var(
            "sonarr_episodefile_paths",
            "/tmp/show/Season 1/episode1.mkv|/tmp/show/Season 1/episode2.mkv",
        );

        let resolved = resolve_media_paths(IntegrationKind::Sonarr).unwrap();
        assert_eq!(resolved.len(), 2);
        assert_eq!(
            resolved[0],
            PathBuf::from("/tmp/show/Season 1/episode1.mkv")
        );
        assert_eq!(
            resolved[1],
            PathBuf::from("/tmp/show/Season 1/episode2.mkv")
        );

        env::remove_var("sonarr_episodefile_paths");
    }

    #[test]
    /// Runs the resolve media path uses radarr fallback operation.
    fn resolve_media_path_uses_radarr_fallback() {
        let _guard = ENV_MUTEX.lock().unwrap();
        env::remove_var("radarr_moviefile_path");
        env::remove_var("radarr_moviefile_paths");
        env::set_var("radarr_movie_path", "/tmp/movie");
        env::set_var("radarr_moviefile_relativepath", "movie.mkv");

        let resolved = resolve_media_paths(IntegrationKind::Radarr)
            .unwrap()
            .into_iter()
            .next()
            .unwrap();
        assert_eq!(resolved, Path::new("/tmp/movie").join("movie.mkv"));

        env::remove_var("radarr_moviefile_relativepath");
        env::remove_var("radarr_movie_path");
    }

    #[test]
    /// Runs the resolve media path uses radarr source only operation.
    fn resolve_media_path_uses_radarr_source_only() {
        let _guard = ENV_MUTEX.lock().unwrap();
        env::remove_var("radarr_moviefile_path");
        env::remove_var("radarr_moviefile_relativepath");
        env::remove_var("radarr_movie_path");
        env::set_var("radarr_moviefile_sourcepath", "/tmp/source/movie.mkv");

        let resolved = resolve_media_paths(IntegrationKind::Radarr)
            .unwrap()
            .into_iter()
            .next()
            .unwrap();
        assert_eq!(resolved, PathBuf::from("/tmp/source/movie.mkv"));

        env::remove_var("radarr_moviefile_sourcepath");
    }

    #[test]
    /// Runs the resolve media path uses radarr moviefile paths operation.
    fn resolve_media_path_uses_radarr_moviefile_paths() {
        let _guard = ENV_MUTEX.lock().unwrap();
        env::remove_var("radarr_moviefile_path");
        env::set_var("radarr_moviefile_paths", "/tmp/movie/movie.mkv");

        let resolved = resolve_media_paths(IntegrationKind::Radarr)
            .unwrap()
            .into_iter()
            .next()
            .unwrap();
        assert_eq!(resolved, PathBuf::from("/tmp/movie/movie.mkv"));

        env::remove_var("radarr_moviefile_paths");
    }

    #[test]
    /// Runs the resolve media paths handles multiple radarr entries operation.
    fn resolve_media_paths_handles_multiple_radarr_entries() {
        let _guard = ENV_MUTEX.lock().unwrap();
        env::remove_var("radarr_moviefile_path");
        env::set_var(
            "radarr_moviefile_paths",
            "/tmp/movie/part1.mkv|/tmp/movie/part2.mkv",
        );

        let resolved = resolve_media_paths(IntegrationKind::Radarr).unwrap();
        assert_eq!(resolved.len(), 2);
        assert_eq!(resolved[0], PathBuf::from("/tmp/movie/part1.mkv"));
        assert_eq!(resolved[1], PathBuf::from("/tmp/movie/part2.mkv"));

        env::remove_var("radarr_moviefile_paths");
    }

    #[test]
    /// Runs the sonarr default suffix is fixed operation.
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
    /// Runs the radarr default suffix is empty operation.
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

    #[test]
    /// Runs the finalize success restores original when promote fails operation.
    fn finalize_success_restores_original_when_promote_fails() {
        let tmp = tempdir().unwrap();
        let input = tmp.path().join("Episode.mkv");
        let backup = tmp.path().join("Episode.backup.mkv");
        let temp_output = tmp.path().join("Episode.tmp.mp4");
        let final_output = tmp.path().join("missing").join("Episode.mp4");

        std::fs::write(&input, b"original").unwrap();
        std::fs::write(&temp_output, b"converted").unwrap();

        let plan = ReplacePlan {
            kind: IntegrationKind::Sonarr,
            event_type: "Download".to_string(),
            display_name: None,
            is_upgrade: None,
            input_path: input.clone(),
            final_output_path: final_output,
            temp_output_path: temp_output.clone(),
            backup_path: backup.clone(),
            input_cstring: CString::new(input.to_string_lossy().to_string()).unwrap(),
            temp_output_cstring: CString::new(temp_output.to_string_lossy().to_string()).unwrap(),
        };

        let err = plan
            .finalize_success()
            .expect_err("promotion should fail when target parent directory is missing");
        assert!(err.to_string().contains("could not promote"));
        assert!(input.exists(), "original input should be restored");
        assert!(!backup.exists(), "backup should not remain after restore");
    }
}
