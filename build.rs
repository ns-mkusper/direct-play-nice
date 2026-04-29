fn main() {
    if std::env::var("CARGO_CFG_TARGET_VENDOR").as_deref() == Ok("apple") {
        for framework in [
            "AudioToolbox",
            "CoreFoundation",
            "CoreMedia",
            "CoreVideo",
            "Security",
            "VideoToolbox",
        ] {
            println!("cargo:rustc-link-lib=framework={framework}");
        }
    }
}
