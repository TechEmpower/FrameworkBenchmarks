//! Pre-built HTTP response templates.
//!
//! For benchmark endpoints, the entire response (minus Date header)
//! is a compile-time constant. This eliminates all response generation cost.

use crate::date::DateCache;

/// Pre-built plaintext response components.
pub struct PlaintextResponse;

impl PlaintextResponse {
    /// Write the complete plaintext response (single copy from DateCache).
    #[inline]
    pub fn write(buf: &mut [u8], date: &DateCache) -> usize {
        let resp = date.plaintext_response();
        buf[..resp.len()].copy_from_slice(resp);
        resp.len()
    }
}

/// Pre-built JSON response components.
pub struct JsonResponse;

impl JsonResponse {
    /// Write the complete JSON response (single copy from DateCache).
    #[inline]
    pub fn write(buf: &mut [u8], date: &DateCache) -> usize {
        let resp = date.json_response();
        buf[..resp.len()].copy_from_slice(resp);
        resp.len()
    }
}

/// HTTP 404 Not Found response.
pub struct NotFoundResponse;

impl NotFoundResponse {
    const RESPONSE: &'static [u8] = b"HTTP/1.1 404 Not Found\r\nServer: V\r\nContent-Length: 0\r\n\r\n";

    /// Write the complete 404 response.
    #[inline]
    pub fn write(buf: &mut [u8]) -> usize {
        let resp = Self::RESPONSE;
        buf[..resp.len()].copy_from_slice(resp);
        resp.len()
    }
}

/// Dynamic JSON response (for /db, /queries, /updates).
/// Content-Length is computed at runtime.
pub struct DynJsonResponse;

impl DynJsonResponse {
    const HEADERS_PREFIX: &'static [u8] = b"HTTP/1.1 200 OK\r\nServer: V\r\nContent-Type: application/json\r\n";

    /// Write a dynamic JSON response. `body` is the JSON body bytes.
    /// Returns total bytes written.
    #[inline]
    pub fn write(buf: &mut [u8], date: &DateCache, body: &[u8]) -> usize {
        let mut offset = 0;

        let prefix = Self::HEADERS_PREFIX;
        buf[offset..offset + prefix.len()].copy_from_slice(prefix);
        offset += prefix.len();

        // Content-Length header
        let cl_prefix = b"Content-Length: ";
        buf[offset..offset + cl_prefix.len()].copy_from_slice(cl_prefix);
        offset += cl_prefix.len();

        let mut itoa_buf = itoa::Buffer::new();
        let cl_val = itoa_buf.format(body.len());
        buf[offset..offset + cl_val.len()].copy_from_slice(cl_val.as_bytes());
        offset += cl_val.len();

        buf[offset..offset + 2].copy_from_slice(b"\r\n");
        offset += 2;

        // Date header
        let date_bytes = date.header_bytes();
        buf[offset..offset + date_bytes.len()].copy_from_slice(date_bytes);
        offset += date_bytes.len();

        // End of headers
        buf[offset..offset + 2].copy_from_slice(b"\r\n");
        offset += 2;

        // Body
        buf[offset..offset + body.len()].copy_from_slice(body);
        offset += body.len();

        offset
    }
}

/// Dynamic HTML response (for /fortunes).
pub struct DynHtmlResponse;

impl DynHtmlResponse {
    const HEADERS_PREFIX: &'static [u8] = b"HTTP/1.1 200 OK\r\nServer: V\r\nContent-Type: text/html; charset=utf-8\r\n";

    /// Write a dynamic HTML response. `body` is the HTML body bytes.
    /// Returns total bytes written.
    #[inline]
    pub fn write(buf: &mut [u8], date: &DateCache, body: &[u8]) -> usize {
        let mut offset = 0;

        let prefix = Self::HEADERS_PREFIX;
        buf[offset..offset + prefix.len()].copy_from_slice(prefix);
        offset += prefix.len();

        let cl_prefix = b"Content-Length: ";
        buf[offset..offset + cl_prefix.len()].copy_from_slice(cl_prefix);
        offset += cl_prefix.len();

        let mut itoa_buf = itoa::Buffer::new();
        let cl_val = itoa_buf.format(body.len());
        buf[offset..offset + cl_val.len()].copy_from_slice(cl_val.as_bytes());
        offset += cl_val.len();

        buf[offset..offset + 2].copy_from_slice(b"\r\n");
        offset += 2;

        let date_bytes = date.header_bytes();
        buf[offset..offset + date_bytes.len()].copy_from_slice(date_bytes);
        offset += date_bytes.len();

        buf[offset..offset + 2].copy_from_slice(b"\r\n");
        offset += 2;

        buf[offset..offset + body.len()].copy_from_slice(body);
        offset += body.len();

        offset
    }
}
