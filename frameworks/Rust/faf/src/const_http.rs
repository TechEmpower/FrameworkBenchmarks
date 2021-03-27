#![allow(dead_code)]

use crate::util::const_len;

pub const GET: &[u8] = b"GET";
pub const GET_LEN: usize = const_len(GET);

pub const HTTP11: &[u8] = b"HTTP/1.1 200 OK";
pub const SERVER: &[u8] = b"Server: F";
pub const CRLF: &[u8] = b"\r\n";
pub const CRLFCRLF: &[u8] = b"\r\n\r\n";

pub const EXAMPLE_HTTP_RESPONSE: &[u8] = b"HTTP/1.1 200 OK\r\nContent-Length: 15\r\nContent-Type: text/plain\r\nServer: F\r\nDate: Wed, 24 Feb 2021 12:00:00 GMT\r\n\r\nHello, World!";
pub const EXAMPLE_HTTP_RESPONSE_LEN: usize = const_len(EXAMPLE_HTTP_RESPONSE);
pub const HTTP_404_NOTFOUND: &[u8] = b"HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n";
pub const HTTP_404_NOTFOUND_LEN: usize = const_len(HTTP_404_NOTFOUND);

pub const HTTP_405_NOTALLOWED: &[u8] = b"HTTP/1.1 405 Method Not Allowed\r\nContent-Length: 0\r\n\r\n";
pub const HTTP_405_NOTALLOWED_LEN: usize = const_len(HTTP_405_NOTALLOWED);

pub const HTTP_500_NOTFOUND: &[u8] = b"HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n";
pub const HTTP_500_NOTFOUND_LEN: usize = const_len(HTTP_500_NOTFOUND);
