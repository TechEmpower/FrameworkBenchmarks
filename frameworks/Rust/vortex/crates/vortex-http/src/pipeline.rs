//! HTTP pipelining support.
//!
//! TechEmpower's wrk client pipelines up to 16 requests per connection.
//! Fast path: classify first request, count boundaries, memcpy × N.
//! Slow path: per-request classify + response for mixed routes.

use crate::parser::{self, Route};
use crate::response::NotFoundResponse;
use crate::date::DateCache;

#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::{_mm_prefetch, _MM_HINT_T0};

/// Process all pipelined requests in a buffer and write responses.
///
/// Returns (requests_processed, response_bytes_written).
#[inline]
pub fn process_pipelined(
    recv_buf: &[u8],
    send_buf: &mut [u8],
    date: &DateCache,
) -> (usize, usize) {
    let route = parser::classify_fast(recv_buf);
    match route {
        Route::Plaintext => {
            let count = parser::count_request_boundaries(recv_buf);
            if count == 0 { return (0, 0); }
            let resp = date.plaintext_response();
            let resp_len = resp.len();
            send_buf[..resp_len].copy_from_slice(resp);
            let mut offset = resp_len;
            for _ in 1..count {
                #[cfg(target_arch = "x86_64")]
                unsafe { _mm_prefetch(send_buf.as_ptr().add(offset + resp_len) as *const i8, _MM_HINT_T0); }
                send_buf[offset..offset + resp_len].copy_from_slice(resp);
                offset += resp_len;
            }
            (count, offset)
        }
        Route::Json => {
            let count = parser::count_request_boundaries(recv_buf);
            if count == 0 { return (0, 0); }
            let resp = date.json_response();
            let resp_len = resp.len();
            send_buf[..resp_len].copy_from_slice(resp);
            let mut offset = resp_len;
            for _ in 1..count {
                #[cfg(target_arch = "x86_64")]
                unsafe { _mm_prefetch(send_buf.as_ptr().add(offset + resp_len) as *const i8, _MM_HINT_T0); }
                send_buf[offset..offset + resp_len].copy_from_slice(resp);
                offset += resp_len;
            }
            (count, offset)
        }
        _ => process_pipelined_slow(recv_buf, send_buf, date),
    }
}

/// Slow path: per-request classify + response generation.
fn process_pipelined_slow(
    recv_buf: &[u8],
    send_buf: &mut [u8],
    date: &DateCache,
) -> (usize, usize) {
    let mut recv_offset = 0;
    let mut send_offset = 0;
    let mut count = 0;

    while recv_offset < recv_buf.len() {
        let remaining = &recv_buf[recv_offset..];
        let req_end = match parser::find_request_end(remaining) {
            Some(end) => end,
            None => break,
        };

        let route = parser::classify_fast(remaining);
        let written = match route {
            Route::Plaintext => {
                let resp = date.plaintext_response();
                send_buf[send_offset..send_offset + resp.len()].copy_from_slice(resp);
                resp.len()
            }
            Route::Json => {
                let resp = date.json_response();
                send_buf[send_offset..send_offset + resp.len()].copy_from_slice(resp);
                resp.len()
            }
            _ => NotFoundResponse::write(&mut send_buf[send_offset..]),
        };

        send_offset += written;
        recv_offset += req_end;
        count += 1;
    }

    (count, send_offset)
}
