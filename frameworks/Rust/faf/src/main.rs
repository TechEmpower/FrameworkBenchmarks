#![feature(core_intrinsics)]

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use faf::const_concat_bytes;
use faf::const_config::*;
use faf::const_http::*;
use faf::extern_http_date;
use faf::util::{const_len, memcmp};
use std::intrinsics::likely;

const ROUTE_PLAINTEXT: &[u8] = b"/p";
const ROUTE_PLAINTEXT_LEN: usize = const_len(ROUTE_PLAINTEXT);
// const ROUTE_JSON: &[u8] = b"/j";
// const ROUTE_JSON_LEN: usize = const_len(ROUTE_JSON);

const TEXT_PLAIN_CONTENT_TYPE: &[u8] = b"Content-Type: text/plain";
const CONTENT_LENGTH: &[u8] = b"Content-Length: ";
const PLAINTEXT_BODY: &[u8] = b"Hello, World!";
const PLAINTEXT_BODY_LEN: usize = const_len(PLAINTEXT_BODY);
const PLAINTEXT_BODY_SIZE: &[u8] = b"13";

const PLAINTEXT_BASE: &[u8] = const_concat_bytes!(
   HTTP_200_OK,
   CRLF,
   SERVER,
   CRLF,
   TEXT_PLAIN_CONTENT_TYPE,
   CRLF,
   CONTENT_LENGTH,
   PLAINTEXT_BODY_SIZE,
   CRLF
);

const PLAINTEXT_BASE_LEN: usize = const_len(PLAINTEXT_BASE);

#[inline]
fn cb(
   method: *const i8,
   method_len: usize,
   path: *const i8,
   path_len: usize,
   _headers: &[faf::phr_header; MAX_HEADERS_TO_PARSE],
   _num_headers: usize,
   response_buffer: *mut u8,
) -> usize {
   unsafe {
      if likely(method_len >= GET_LEN && path_len >= ROUTE_PLAINTEXT_LEN) {
         if likely(memcmp(GET.as_ptr() as *const i8, method, GET_LEN) == 0) {
            // For performance purposes, this will successfully match '/p' to '/plaintext' and '/pickle'. Use with caution
            if likely(memcmp(ROUTE_PLAINTEXT.as_ptr() as *const i8, path, ROUTE_PLAINTEXT_LEN) == 0) {
               let mut date_buff = crate::extern_http_date::get_buff_with_date();
               extern_http_date::get_http_date(&mut date_buff);
               std::ptr::copy_nonoverlapping(PLAINTEXT_BASE.as_ptr(), response_buffer, PLAINTEXT_BASE_LEN);
               std::ptr::copy_nonoverlapping(
                  date_buff.as_ptr(),
                  response_buffer.add(PLAINTEXT_BASE_LEN),
                  DATE_LEN,
               );
               std::ptr::copy_nonoverlapping(
                  CRLFCRLF.as_ptr(),
                  response_buffer.add(PLAINTEXT_BASE_LEN + DATE_LEN),
                  CRLFCRLF_LEN,
               );
               std::ptr::copy_nonoverlapping(
                  PLAINTEXT_BODY.as_ptr(),
                  response_buffer.add(PLAINTEXT_BASE_LEN + DATE_LEN + CRLFCRLF_LEN),
                  PLAINTEXT_BODY_LEN,
               );

               PLAINTEXT_BASE_LEN + DATE_LEN + CRLFCRLF_LEN + PLAINTEXT_BODY_LEN
            } else {
               std::ptr::copy_nonoverlapping(
                  HTTP_404_NOTFOUND.as_ptr(),
                  response_buffer,
                  HTTP_404_NOTFOUND_LEN,
               );
               HTTP_404_NOTFOUND_LEN
            }
         } else {
            std::ptr::copy_nonoverlapping(
               HTTP_405_NOTALLOWED.as_ptr(),
               response_buffer,
               HTTP_405_NOTALLOWED_LEN,
            );
            HTTP_405_NOTALLOWED_LEN
         }
      } else {
         0
      }
   }
}

#[inline]
fn main() {
   faf::epoll::go(8089, cb);
}
