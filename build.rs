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

    if std::env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("linux") {
        // vcpkg ffmpeg[vaapi] references libva, which rusty_ffmpeg does not emit.
        // link-arg (not link-lib) so the static archives land after FFmpeg's
        // in link order; va last since va-drm depends on it.
        for lib in ["va-drm", "va"] {
            println!("cargo:rustc-link-arg=-l{lib}");
        }
    }

    if std::env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("windows") {
        for lib in [
            "Mfplat", "Strmiids", "Mfuuid", "Bcrypt", "Ncrypt", "Crypt32", "Secur32", "Ole32",
            "User32",
        ] {
            println!("cargo:rustc-link-lib={lib}");
        }
    }
}
