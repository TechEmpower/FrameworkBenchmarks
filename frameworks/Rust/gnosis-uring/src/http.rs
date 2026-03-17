//! Minimal HTTP/1.1 parser — just enough for static file serving.
//!
//! No allocations in the hot path. Parses method + path from the
//! request buffer without copying. This is all the HTTP we need:
//!
//!   GET /path HTTP/1.1\r\n
//!   Host: ...\r\n
//!   \r\n
//!
//! Everything else (headers, body) is ignored for static serving.

/// Parsed HTTP request (zero-copy — borrows from the buffer).
pub struct Request<'a> {
    pub method: &'a [u8],
    pub path: &'a [u8],
    pub keep_alive: bool,
}

/// Parse an HTTP request from raw bytes.
///
/// Returns None if the request is incomplete (need more data).
/// Returns Some(request, consumed_bytes) on success.
#[inline]
pub fn parse_request(buf: &[u8]) -> Option<(Request<'_>, usize)> {
    // Find end of request (\r\n\r\n)
    let end = find_header_end(buf)?;

    // Parse method (GET, HEAD, etc.)
    let space1 = memchr(b' ', buf)?;
    let method = &buf[..space1];

    // Parse path
    let path_start = space1 + 1;
    let space2 = memchr(b' ', &buf[path_start..]).map(|i| i + path_start)?;
    let path = &buf[path_start..space2];

    // Check Connection: keep-alive (default for HTTP/1.1)
    let keep_alive = !contains_ci(buf, b"connection: close");

    Some((Request { method, path, keep_alive }, end))
}

/// Pre-built HTTP response for plaintext benchmark.
pub const PLAINTEXT_RESPONSE: &[u8] = b"HTTP/1.1 200 OK\r\n\
    Content-Type: text/plain\r\n\
    Content-Length: 13\r\n\
    Server: gnosis-uring\r\n\
    \r\n\
    Hello, World!";

/// Pre-built HTTP response for JSON benchmark.
pub const JSON_RESPONSE: &[u8] = b"HTTP/1.1 200 OK\r\n\
    Content-Type: application/json\r\n\
    Content-Length: 27\r\n\
    Server: gnosis-uring\r\n\
    \r\n\
    {\"message\":\"Hello, World!\"}";

/// HTTP 404 response.
pub const NOT_FOUND_RESPONSE: &[u8] = b"HTTP/1.1 404 Not Found\r\n\
    Content-Length: 9\r\n\
    \r\n\
    Not Found";

/// Build a 200 OK response with the given body and content type.
pub fn build_response(body: &[u8], content_type: &str) -> Vec<u8> {
    let mut resp = Vec::with_capacity(256 + body.len());
    resp.extend_from_slice(b"HTTP/1.1 200 OK\r\n");
    resp.extend_from_slice(b"Content-Type: ");
    resp.extend_from_slice(content_type.as_bytes());
    resp.extend_from_slice(b"\r\nContent-Length: ");
    resp.extend_from_slice(body.len().to_string().as_bytes());
    resp.extend_from_slice(b"\r\nServer: gnosis-uring\r\n\r\n");
    resp.extend_from_slice(body);
    resp
}

/// Build a 200 OK response with pre-compressed body and encoding header.
pub fn build_compressed_response(body: &[u8], content_type: &str, encoding: &str) -> Vec<u8> {
    let mut resp = Vec::with_capacity(256 + body.len());
    resp.extend_from_slice(b"HTTP/1.1 200 OK\r\n");
    resp.extend_from_slice(b"Content-Type: ");
    resp.extend_from_slice(content_type.as_bytes());
    resp.extend_from_slice(b"\r\nContent-Encoding: ");
    resp.extend_from_slice(encoding.as_bytes());
    resp.extend_from_slice(b"\r\nContent-Length: ");
    resp.extend_from_slice(body.len().to_string().as_bytes());
    resp.extend_from_slice(b"\r\nServer: gnosis-uring\r\n\r\n");
    resp.extend_from_slice(body);
    resp
}

// ── Helpers ──────────────────────────────────────────────────────

#[inline]
fn memchr(needle: u8, haystack: &[u8]) -> Option<usize> {
    haystack.iter().position(|&b| b == needle)
}

#[inline]
fn find_header_end(buf: &[u8]) -> Option<usize> {
    if buf.len() < 4 {
        return None;
    }
    for i in 0..buf.len() - 3 {
        if buf[i] == b'\r' && buf[i + 1] == b'\n' && buf[i + 2] == b'\r' && buf[i + 3] == b'\n' {
            return Some(i + 4);
        }
    }
    None
}

#[inline]
fn contains_ci(haystack: &[u8], needle: &[u8]) -> bool {
    if needle.len() > haystack.len() {
        return false;
    }
    haystack.windows(needle.len()).any(|window| {
        window.iter().zip(needle.iter()).all(|(a, b)| a.to_ascii_lowercase() == *b)
    })
}
