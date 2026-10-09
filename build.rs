fn main() {
    if std::env::var("CARGO_CFG_TARGET_VENDOR").as_deref() == Ok("apple") {
        for framework in [
            "AudioToolbox",
            "AppKit",
            "CoreFoundation",
            "CoreGraphics",
            "CoreImage",
            "CoreMedia",
            "CoreVideo",
            "Foundation",
            "OpenGL",
            "Security",
            "VideoToolbox",
        ] {
            println!("cargo:rustc-link-lib=framework={framework}");
        }
    }

    println!("cargo:rerun-if-env-changed=FFMPEG_PKG_CONFIG_PATH");
    println!("cargo:rerun-if-env-changed=FFMPEG_LIBS_DIR");
    if std::env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("linux")
        && std::env::var_os("FFMPEG_PKG_CONFIG_PATH").is_none()
        && std::env::var_os("FFMPEG_LIBS_DIR").is_none()
    {
        // Only repair the default vcpkg path, not externally supplied FFmpeg.
        // vcpkg emits freetype before fontconfig and z before png16. Repeat
        // these archives after their consumers, in dependency order.
        // link-arg (not link-lib) keeps them after FFmpeg's native libraries.
        // vcpkg ffmpeg[vaapi] also needs shared libva, omitted by rusty_ffmpeg;
        // va follows va-drm because va-drm depends on it.
        for lib in ["freetype", "png16", "z", "va-drm", "va"] {
            println!("cargo:rustc-link-arg=-l{lib}");
        }
    }

    if std::env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("windows") {
        // Advapi32: vcpkg's ffmpeg[qsv] links Intel libvpl, which reads the
        // driver store from the registry.
        for lib in [
            "Mfplat", "Strmiids", "Mfuuid", "Bcrypt", "Ncrypt", "Crypt32", "Secur32", "Ole32",
            "User32", "Advapi32",
        ] {
            println!("cargo:rustc-link-lib={lib}");
        }
    }
}
