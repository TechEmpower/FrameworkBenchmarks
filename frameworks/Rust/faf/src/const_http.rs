#![allow(dead_code)]

use crate::util::const_len;

pub const GET: &[u8] = b"GET";
pub const GET_LEN: usize = const_len(GET);
pub const HEAD: &[u8] = b"HEAD";
pub const HEAD_LEN: usize = const_len(HEAD);
pub const POST: &[u8] = b"POST";
pub const POST_LEN: usize = const_len(POST);
pub const PUT: &[u8] = b"PUT";
pub const PUT_LEN: usize = const_len(PUT);
pub const DELETE: &[u8] = b"DELETE";
pub const DELETE_LEN: usize = const_len(DELETE);
pub const CONNECT: &[u8] = b"CONNECT";
pub const CONNECT_LEN: usize = const_len(CONNECT);
pub const OPTIONS: &[u8] = b"OPTIONS";
pub const OPTIONS_LEN: usize = const_len(OPTIONS);
pub const TRACE: &[u8] = b"TRACE";
pub const TRACE_LEN: usize = const_len(TRACE);
pub const PATCH: &[u8] = b"PATCH";
pub const PATCH_LEN: usize = const_len(PATCH);

pub const CRLF: &[u8] = b"\r\n";
pub const CRLF_LEN: usize = 2;
pub const CRLFCRLF: &[u8] = b"\r\n\r\n";
pub const CRLFCRLF_LEN: usize = 4;

pub const SERVER: &[u8] = b"Server: F";
pub const DATE_LEN: usize = 35;

pub const HTTP_200_OK: &[u8] = b"HTTP/1.1 200 OK";
pub const HTTP_200_OK_LEN: usize = const_len(HTTP_200_OK);

pub const HTTP_404_NOTFOUND: &[u8] = b"HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n";
pub const HTTP_404_NOTFOUND_LEN: usize = const_len(HTTP_404_NOTFOUND);

pub const HTTP_405_NOTALLOWED: &[u8] = b"HTTP/1.1 405 Method Not Allowed\r\nContent-Length: 0\r\n\r\n";
pub const HTTP_405_NOTALLOWED_LEN: usize = const_len(HTTP_405_NOTALLOWED);

pub const HTTP_500_NOTFOUND: &[u8] = b"HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n";
pub const HTTP_500_NOTFOUND_LEN: usize = const_len(HTTP_500_NOTFOUND);

pub const EXAMPLE_HTTP_RESPONSE: &[u8] = b"HTTP/1.1 200 OK\r\nContent-Length: 15\r\nContent-Type: text/plain\r\nServer: F\r\nDate: Wed, 24 Feb 2021 12:00:00 GMT\r\n\r\nHello, World!";
pub const EXAMPLE_HTTP_RESPONSE_LEN: usize = const_len(EXAMPLE_HTTP_RESPONSE);
