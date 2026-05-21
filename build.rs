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

    if std::env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("windows") {
        for lib in [
            "Mfplat", "Strmiids", "Mfuuid", "Bcrypt", "Ncrypt", "Crypt32", "Secur32", "Ole32",
            "User32",
        ] {
            println!("cargo:rustc-link-lib={lib}");
        }
    }
}
