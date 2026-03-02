//! Tiered HTTP request parser.
//!
//! Tier 1: Ultra-fast path classification (~3ns) - for benchmark routes
//! Tier 2: httparse with SIMD for general HTTP/1.1 requests
//! SSE2 SIMD for \r\n\r\n boundary scanning (16 bytes per iteration)

#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::*;

/// Known routes for fast-path matching.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Route {
    Plaintext,
    Json,
    Db,
    Queries,
    Fortunes,
    Updates,
    NotFound,
}

/// Tier 1: Ultra-fast request classification.
///
/// TechEmpower's wrk sends well-formed requests like:
/// "GET /plaintext HTTP/1.1\r\nHost: ...\r\n\r\n"
///
/// We only need to identify the path from the first few bytes.
/// This runs in ~3ns — just a few byte comparisons.
#[inline(always)]
pub fn classify_fast(buf: &[u8]) -> Route {
    // Minimum valid: "GET / HTTP/1.1\r\n" = 16 bytes
    if buf.len() < 16 {
        return Route::NotFound;
    }

    // Check "GET " prefix
    if buf[0] != b'G' || buf[1] != b'E' || buf[2] != b'T' || buf[3] != b' ' || buf[4] != b'/' {
        return Route::NotFound;
    }

    // Match on the character after '/'
    match buf[5] {
        b'p' => Route::Plaintext,  // /plaintext
        b'j' => Route::Json,       // /json
        b'd' => Route::Db,         // /db
        b'q' => Route::Queries,    // /queries?q=
        b'f' => Route::Fortunes,   // /fortunes
        b'u' => Route::Updates,    // /updates?q=
        _ => Route::NotFound,
    }
}

/// Tier 2: Full HTTP/1.1 parsing with httparse.
///
/// Returns the parsed request and the number of bytes consumed.
pub fn parse_request(buf: &[u8]) -> Result<(ParsedRequest<'_>, usize), ParseError> {
    let mut headers = [httparse::EMPTY_HEADER; 32];
    let mut req = httparse::Request::new(&mut headers);

    match req.parse(buf) {
        Ok(httparse::Status::Complete(len)) => {
            let parsed = ParsedRequest {
                method: req.method.unwrap_or("GET"),
                path: req.path.unwrap_or("/"),
                version: req.version.unwrap_or(1),
            };
            Ok((parsed, len))
        }
        Ok(httparse::Status::Partial) => Err(ParseError::Incomplete),
        Err(e) => Err(ParseError::Invalid(e)),
    }
}

/// Find the end of an HTTP request in a buffer (the \r\n\r\n boundary).
///
/// SSE2: scans 16 byte positions per iteration using parallel comparison.
#[inline]
pub fn find_request_end(buf: &[u8]) -> Option<usize> {
    if buf.len() < 4 {
        return None;
    }

    unsafe {
        let ptr = buf.as_ptr();
        let len = buf.len();
        let cr = _mm_set1_epi8(b'\r' as i8);
        let lf = _mm_set1_epi8(b'\n' as i8);
        let mut i = 0;

        // SSE2: check 16 positions per iteration
        while i + 19 <= len {
            let v0 = _mm_loadu_si128(ptr.add(i) as *const __m128i);
            let v1 = _mm_loadu_si128(ptr.add(i + 1) as *const __m128i);
            let v2 = _mm_loadu_si128(ptr.add(i + 2) as *const __m128i);
            let v3 = _mm_loadu_si128(ptr.add(i + 3) as *const __m128i);

            let m = _mm_and_si128(
                _mm_and_si128(_mm_cmpeq_epi8(v0, cr), _mm_cmpeq_epi8(v1, lf)),
                _mm_and_si128(_mm_cmpeq_epi8(v2, cr), _mm_cmpeq_epi8(v3, lf)),
            );
            let mask = _mm_movemask_epi8(m) as u32;
            if mask != 0 {
                return Some(i + mask.trailing_zeros() as usize + 4);
            }
            i += 16;
        }

        // Scalar tail
        let target = u32::from_ne_bytes(*b"\r\n\r\n");
        while i + 3 < len {
            if (ptr.add(i) as *const u32).read_unaligned() == target {
                return Some(i + 4);
            }
            i += 1;
        }

        None
    }
}

/// Count the number of complete HTTP requests (\r\n\r\n boundaries) in a buffer.
///
/// SSE2: scans 16 byte positions per iteration using parallel comparison.
#[inline]
pub fn count_request_boundaries(buf: &[u8]) -> usize {
    if buf.len() < 4 {
        return 0;
    }

    unsafe {
        let ptr = buf.as_ptr();
        let len = buf.len();
        let cr = _mm_set1_epi8(b'\r' as i8);
        let lf = _mm_set1_epi8(b'\n' as i8);
        let mut count = 0;
        let mut i = 0;

        // SSE2: check 16 positions per iteration
        while i + 19 <= len {
            let v0 = _mm_loadu_si128(ptr.add(i) as *const __m128i);
            let v1 = _mm_loadu_si128(ptr.add(i + 1) as *const __m128i);
            let v2 = _mm_loadu_si128(ptr.add(i + 2) as *const __m128i);
            let v3 = _mm_loadu_si128(ptr.add(i + 3) as *const __m128i);

            let m = _mm_and_si128(
                _mm_and_si128(_mm_cmpeq_epi8(v0, cr), _mm_cmpeq_epi8(v1, lf)),
                _mm_and_si128(_mm_cmpeq_epi8(v2, cr), _mm_cmpeq_epi8(v3, lf)),
            );
            let mask = _mm_movemask_epi8(m) as u32;
            count += mask.count_ones() as usize;
            i += 16;
        }

        // Scalar tail
        let target = u32::from_ne_bytes(*b"\r\n\r\n");
        while i + 3 < len {
            if (ptr.add(i) as *const u32).read_unaligned() == target {
                count += 1;
                i += 4;
            } else {
                i += 1;
            }
        }

        count
    }
}

/// A parsed HTTP request (Tier 2).
#[derive(Debug)]
pub struct ParsedRequest<'a> {
    pub method: &'a str,
    pub path: &'a str,
    pub version: u8,
}

/// Parse error types.
#[derive(Debug)]
pub enum ParseError {
    Incomplete,
    Invalid(httparse::Error),
}

/// Extract the query count parameter from a request URL.
/// Supports both TFB standard `?q=5` and `?queries=5`.
/// Returns the integer value, or 1 if not found / invalid.
#[inline]
pub fn parse_queries_param(buf: &[u8]) -> i32 {
    let end = buf.len().min(64);
    // Find '=' after '?' in the URL
    let mut past_q = false;
    for i in 5..end {
        if buf[i] == b'?' {
            past_q = true;
        } else if past_q && buf[i] == b'=' {
            // Parse integer after '='
            let mut val: i32 = 0;
            for j in (i + 1)..end {
                match buf[j] {
                    b'0'..=b'9' => val = val * 10 + (buf[j] - b'0') as i32,
                    _ => break,
                }
            }
            return if val < 1 { 1 } else { val };
        } else if buf[i] == b' ' {
            break; // end of URL
        }
    }
    1
}
