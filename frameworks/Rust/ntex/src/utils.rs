#![allow(dead_code)]
use std::{cmp, io};

use atoi::FromRadix10;
use ntex::http::header::HeaderValue;
use ntex::util::{BufMut, Bytes, BytesMut};

pub const HDR_SERVER: HeaderValue = HeaderValue::from_static("N");
pub const HDR_JSON_CONTENT_TYPE: HeaderValue = HeaderValue::from_static("application/json");
pub const HDR_TEXT_CONTENT_TYPE: HeaderValue = HeaderValue::from_static("text/plain");
pub const HDR_HTML_CONTENT_TYPE: HeaderValue =
    HeaderValue::from_static("text/html; charset=utf-8");
pub const BODY_PLAIN_TEXT: Bytes = Bytes::from_static(b"Hello, World!");

pub const SIZE: usize = 27;

pub fn get_query_param(query: Option<&str>) -> u16 {
    let query = query.unwrap_or("");
    let q = if let Some(pos) = query.find('q') {
        u16::from_radix_10(query.split_at(pos + 2).1.as_ref()).0
    } else {
        1
    };
    cmp::min(500, cmp::max(1, q))
}

pub struct Writer<'a>(pub &'a mut BytesMut);

impl<'a> io::Write for Writer<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.put_slice(buf);
        Ok(buf.len())
    }
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}
