//! Minimal HTTP/1.1 parser — just enough for static file serving.
//!
//! Supports HTTP pipelining: multiple requests in a single read buffer.
//! The parser returns each request with its consumed byte count so the
//! caller can iterate through pipelined requests.
//!
//! No allocations in the hot path. Parses method + path from the
//! request buffer without copying.

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
///
/// `consumed_bytes` is key for pipelining: the caller advances
/// the buffer by this amount and parses again.
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
    let keep_alive = !contains_ci(&buf[..end], b"connection: close");

    Some((Request { method, path, keep_alive }, end))
}

/// Parse ALL pipelined requests from a buffer.
///
/// HTTP pipelining: a client sends N requests back-to-back without
/// waiting for responses. TechEmpower's wrk uses pipeline depth 16-256.
///
/// This is the FORK in the topology:
///   (read_buf)-[:FORK { fan: 'requests' }]->(req1, req2, ..., reqN)
///
/// Returns Vec of (path, keep_alive) pairs and total bytes consumed.
pub fn parse_pipelined(buf: &[u8]) -> (Vec<(Vec<u8>, bool)>, usize) {
    let mut requests = Vec::new();
    let mut offset = 0;

    while offset < buf.len() {
        match parse_request(&buf[offset..]) {
            Some((req, consumed)) => {
                if req.method == b"GET" {
                    requests.push((req.path.to_vec(), req.keep_alive));
                }
                offset += consumed;
            }
            None => break, // incomplete request — need more data
        }
    }

    (requests, offset)
}

/// HTTP 404 response (static — no dynamic headers needed).
pub const NOT_FOUND_RESPONSE: &[u8] = b"HTTP/1.1 404 Not Found\r\nContent-Length: 9\r\n\r\nNot Found";

/// Build the plaintext response per TechEmpower rules.
///
/// Per spec: Content-Length must be calculated (not pre-built constant).
/// The Date header is required and must reflect the current time.
/// We compute Content-Length from the body and include a Date header.
#[inline]
pub fn build_plaintext_response(date: &str) -> Vec<u8> {
    let body = b"Hello, World!";
    let content_length = body.len(); // 13, computed not hardcoded
    let mut resp = Vec::with_capacity(160);
    resp.extend_from_slice(b"HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: ");
    resp.extend_from_slice(content_length.to_string().as_bytes());
    resp.extend_from_slice(b"\r\nServer: gnosis-uring\r\nDate: ");
    resp.extend_from_slice(date.as_bytes());
    resp.extend_from_slice(b"\r\n\r\n");
    resp.extend_from_slice(body);
    resp
}

/// Build the JSON response per TechEmpower rules.
///
/// Per spec: "For each request, an object mapping the key message to
/// Hello, World! must be instantiated." The JSON object must be
/// serialized per-request, not pre-built.
#[inline]
pub fn build_json_response(date: &str) -> Vec<u8> {
    // Instantiate the object per-request as required by TechEmpower rules
    let message = "Hello, World!";
    let json_body = format!("{{\"message\":\"{}\"}}", message);
    let content_length = json_body.len(); // 27, computed from serialized object
    let mut resp = Vec::with_capacity(192);
    resp.extend_from_slice(b"HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: ");
    resp.extend_from_slice(content_length.to_string().as_bytes());
    resp.extend_from_slice(b"\r\nServer: gnosis-uring\r\nDate: ");
    resp.extend_from_slice(date.as_bytes());
    resp.extend_from_slice(b"\r\n\r\n");
    resp.extend_from_slice(json_body.as_bytes());
    resp
}

/// Build a 200 OK response with the given body and content type.
pub fn build_response(body: &[u8], content_type: &str) -> Vec<u8> {
    let mut resp = Vec::with_capacity(256 + body.len());
    resp.extend_from_slice(b"HTTP/1.1 200 OK\r\nContent-Type: ");
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
    resp.extend_from_slice(b"HTTP/1.1 200 OK\r\nContent-Type: ");
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
