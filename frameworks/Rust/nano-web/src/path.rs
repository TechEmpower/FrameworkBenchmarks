use anyhow::{bail, Result};

/// Maximum path length to prevent buffer overflows
pub const MAX_PATH_LENGTH: usize = 1024;

/// Maximum number of path components to prevent deeply nested attacks
pub const MAX_PATH_COMPONENTS: usize = 32;

/// Validate and sanitize HTTP request path to prevent directory traversal attacks
pub fn validate_request_path(path: &str) -> Result<String> {
    // Basic length check
    if path.len() > MAX_PATH_LENGTH {
        bail!("Path too long");
    }

    // Must start with /
    if !path.starts_with('/') {
        bail!("Path must start with /");
    }

    // Decode URL encoding safely
    let decoded = match urlencoding::decode(path) {
        Ok(decoded) => decoded.into_owned(),
        Err(_) => bail!("Invalid URL encoding"),
    };

    // Check for null bytes (security)
    if decoded.contains('\0') {
        bail!("Path contains null bytes");
    }

    // Split into components and validate each
    let components: Vec<&str> = decoded.split('/').skip(1).collect(); // Skip first empty component

    if components.len() > MAX_PATH_COMPONENTS {
        bail!("Too many path components");
    }

    let mut sanitized_components = Vec::new();

    for component in components {
        // Skip empty components (double slashes)
        if component.is_empty() {
            continue;
        }

        // Reject dangerous components
        if component == ".." || component == "." {
            bail!("Path traversal attempt detected");
        }

        // Check for dangerous characters
        if component.contains(['\\', '\0', '<', '>', '|', '?', '*']) {
            bail!("Invalid characters in path component");
        }

        // Reject hidden files/directories (starting with .) except .well-known
        if component.starts_with('.') && component != ".well-known" {
            bail!("Access to hidden files denied");
        }

        // Reject overly long components
        if component.len() > 255 {
            bail!("Path component too long");
        }

        sanitized_components.push(component);
    }

    // Reconstruct safe path
    let safe_path = if sanitized_components.is_empty() {
        "/".to_string()
    } else {
        format!("/{}", sanitized_components.join("/"))
    };

    Ok(safe_path)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_path_validation() {
        // Valid paths
        assert!(validate_request_path("/").is_ok());
        assert!(validate_request_path("/index.html").is_ok());
        assert!(validate_request_path("/assets/style.css").is_ok());
        assert!(validate_request_path("/.well-known/acme-challenge/token").is_ok());
        assert!(validate_request_path("/.well-known/security.txt").is_ok());

        // Invalid paths
        assert!(validate_request_path("../etc/passwd").is_err());
        assert!(validate_request_path("/.env").is_err());
        assert!(validate_request_path("/.secret").is_err());
        assert!(validate_request_path("/path/with/../../traversal").is_err());
        assert!(validate_request_path("/path\0null").is_err());
    }

    // Note: parse_request_line_secure was removed as it's not used in the Axum implementation
    // The Axum HTTP stack handles request parsing internally
}
