use crate::const_concat_bytes;
use crate::epoll_config::*;
use crate::epoll_const::*;
use crate::extern_httpdate;
use crate::util::memcmp;

const GET: &[u8] = b"GET";
const GET_LEN: usize = const_len(GET);
const ROUTE_PLAINTEXT: &[u8] = b"/p";
const ROUTE_PLAINTEXT_LEN: usize = const_len(ROUTE_PLAINTEXT);
const ROUTE_JSON: &[u8] = b"/j";
const ROUTE_JSON_LEN: usize = const_len(ROUTE_JSON);

const HTTP11: &[u8] = b"HTTP/1.1 200 OK";
const SERVER: &[u8] = b"Server: F";
const CRLF: &[u8] = b"\r\n";
const CRLFCRLF: &[u8] = b"\r\n\r\n";
const PLAINTEXT_CONTENT_TYPE: &[u8] = b"Content-Type: text/plain";
const CONTENT_LENGTH: &[u8] = b"Content-Length: ";
const PLAINTEXT_BODY: &[u8] = b"Hello, World!";
const PLAINTEXT_BODY_LEN: &[u8] = b"13";

const PLAINTEXT_BASE: &[u8] = const_concat_bytes!(
   HTTP11,
   CRLF,
   SERVER,
   CRLF,
   PLAINTEXT_CONTENT_TYPE,
   CRLF,
   CONTENT_LENGTH,
   PLAINTEXT_BODY_LEN,
   CRLF
);

const PLAINTEXT_BASE_LEN: usize = const_len(PLAINTEXT_BASE);

#[inline]
pub fn cb(
   method: *const i8,
   method_len: usize,
   path: *const i8,
   path_len: usize,
   _headers: &[faf_pico_sys::phr_header; MAX_HEADERS_TO_PARSE],
   _num_headers: usize,
   response_buffer: &mut [u8; REQ_RES_BUFF_SIZE],
) -> usize {
   if method_len < GET_LEN || path_len < ROUTE_PLAINTEXT_LEN {
      return 0;
   }
   unsafe {
      if memcmp(GET.as_ptr() as *const i8, method, GET_LEN) == 0 {
         // Note, this will successfully match '/p' to '/plaintext' and '/pickle'. Use with caution
         if memcmp(ROUTE_PLAINTEXT.as_ptr() as *const i8, path, ROUTE_PLAINTEXT_LEN) == 0 {
            let mut date_buff = crate::extern_httpdate::get_buff_with_date();
            extern_httpdate::get_http_date(&mut date_buff);
            std::ptr::copy_nonoverlapping(PLAINTEXT_BASE.as_ptr(), response_buffer.as_mut_ptr(), PLAINTEXT_BASE_LEN);
            std::ptr::copy_nonoverlapping(date_buff.as_ptr(), response_buffer.as_mut_ptr().add(PLAINTEXT_BASE_LEN), 35);
            std::ptr::copy_nonoverlapping(
               CRLFCRLF.as_ptr(),
               response_buffer.as_mut_ptr().add(PLAINTEXT_BASE_LEN + 35),
               4,
            );
            std::ptr::copy_nonoverlapping(
               PLAINTEXT_BODY.as_ptr(),
               response_buffer.as_mut_ptr().add(PLAINTEXT_BASE_LEN + 35 + 4),
               13,
            );

            return PLAINTEXT_BASE_LEN + 35 + 4 + 13;
         } else if memcmp(ROUTE_JSON.as_ptr() as *const i8, path, ROUTE_JSON_LEN) == 0 {
         } else {
            std::ptr::copy_nonoverlapping(HTTP_404_NOTFOUND.as_ptr(), response_buffer.as_mut_ptr(), HTTP_404_NOTFOUND_LEN);
            return HTTP_404_NOTFOUND_LEN;
         }
      } else {
         std::ptr::copy_nonoverlapping(HTTP_405_NOTALLOWED.as_ptr(), response_buffer.as_mut_ptr(), HTTP_405_NOTALLOWED_LEN);
         return HTTP_405_NOTALLOWED_LEN;
      }
   };

   0
}
