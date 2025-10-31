use mime_guess::MimeGuess;
use std::path::Path;

pub const DEFAULT_MIME: &str = "application/octet-stream";

pub struct MimeConfig {
    pub mime_type: String,
    pub is_compressible: bool,
    pub is_templatable: bool,
}

impl MimeConfig {
    pub fn new(mime_type: String) -> Self {
        let is_compressible = is_compressible(&mime_type);
        let is_templatable = is_templatable(&mime_type);

        Self {
            mime_type,
            is_compressible,
            is_templatable,
        }
    }
}

pub fn get_mime_config<P: AsRef<Path>>(path: P) -> MimeConfig {
    let mime = MimeGuess::from_path(path)
        .first()
        .map(|m| m.to_string())
        .unwrap_or_else(|| DEFAULT_MIME.to_string());

    MimeConfig::new(mime)
}

pub fn is_compressible(mime_type: &str) -> bool {
    match mime_type {
        // Text-based formats
        "text/html"
        | "text/css"
        | "text/javascript"
        | "text/plain"
        | "text/csv"
        | "text/markdown"
        | "text/cache-manifest"
        | "application/json"
        | "application/ld+json"
        | "application/manifest+json"
        | "text/xml"
        | "application/xml"
        | "application/rss+xml"
        | "application/atom+xml"
        | "image/svg+xml" => true,

        // Already compressed or binary
        "image/jpeg"
        | "image/png"
        | "image/gif"
        | "image/webp"
        | "image/avif"
        | "image/heic"
        | "image/heif"
        | "image/bmp"
        | "image/tiff"
        | "image/x-icon"
        | "audio/mpeg"
        | "audio/mp4"
        | "audio/aac"
        | "audio/ogg"
        | "audio/flac"
        | "audio/opus"
        | "audio/wav"
        | "video/mp4"
        | "video/webm"
        | "video/x-msvideo"
        | "video/quicktime"
        | "video/x-ms-wmv"
        | "video/x-flv"
        | "video/x-matroska"
        | "font/woff"
        | "font/woff2"
        | "font/ttf"
        | "font/otf"
        | "application/vnd.ms-fontobject"
        | "application/zip"
        | "application/gzip"
        | "application/x-bzip2"
        | "application/vnd.rar"
        | "application/x-7z-compressed"
        | "application/x-xz"
        | "application/x-tar"
        | "application/pdf"
        | "application/msword"
        | "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
        | "application/vnd.ms-excel"
        | "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
        | "application/vnd.ms-powerpoint"
        | "application/vnd.openxmlformats-officedocument.presentationml.presentation"
        | "application/vnd.oasis.opendocument.text"
        | "application/vnd.oasis.opendocument.spreadsheet"
        | "application/vnd.oasis.opendocument.presentation"
        | "application/octet-stream" => false,

        _ => false,
    }
}

pub fn is_templatable(mime_type: &str) -> bool {
    mime_type == "text/html"
}

pub fn is_asset(mime_type: &str) -> bool {
    match mime_type {
        // CSS and JavaScript
        "text/css" | "text/javascript" => true,

        // Images
        m if m.starts_with("image/") => true,

        // Fonts
        m if m.starts_with("font/") => true,
        "application/vnd.ms-fontobject" => true,

        // Audio and Video
        m if m.starts_with("audio/") || m.starts_with("video/") => true,

        _ => false,
    }
}

pub fn get_cache_control(mime_type: &str) -> &'static str {
    if is_asset(mime_type) {
        "public, max-age=31536000, immutable" // 1 year
    } else if mime_type == "text/html" {
        "public, max-age=900" // 15 minutes
    } else {
        "public, max-age=3600" // 1 hour
    }
}
