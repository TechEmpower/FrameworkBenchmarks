#![allow(dead_code)]
use std::{cmp, io, io::Write, mem::MaybeUninit, slice::from_raw_parts_mut};

use atoi::FromRadix10;
use ntex::{http::header::HeaderValue, util::BufMut, util::Bytes, util::BytesMut};
use sonic_rs::writer::WriteExt;

pub const HDR_SERVER: HeaderValue = HeaderValue::from_static("N");
pub const HDR_JSON_CONTENT_TYPE: HeaderValue = HeaderValue::from_static("application/json");
pub const HDR_TEXT_CONTENT_TYPE: HeaderValue = HeaderValue::from_static("text/plain");
pub const HDR_HTML_CONTENT_TYPE: HeaderValue =
    HeaderValue::from_static("text/html; charset=utf-8");
pub const BODY_PLAIN_TEXT: Bytes = Bytes::from_static(b"Hello, World!");

const HW: usize = 128 * 1024;
pub const SIZE: usize = 27;

pub fn get_query_param(query: Option<&str>) -> usize {
    let query = query.unwrap_or("");
    let q = if let Some(pos) = query.find('q') {
        u16::from_radix_10(query.split_at(pos + 2).1.as_ref()).0
    } else {
        1
    };
    cmp::min(500, cmp::max(1, q) as usize)
}

pub fn reserve(buf: &mut BytesMut, lw: usize) {
    let remaining = buf.remaining_mut();
    if remaining < lw {
        buf.reserve(HW);
    }
}

pub struct BytesWriter<'a>(pub &'a mut BytesMut);

impl<'a> Write for BytesWriter<'a> {
    fn write(&mut self, src: &[u8]) -> Result<usize, io::Error> {
        self.0.extend_from_slice(src);
        Ok(src.len())
    }

    fn flush(&mut self) -> Result<(), io::Error> {
        Ok(())
    }
}

impl<'a> WriteExt for BytesWriter<'a> {
    #[inline(always)]
    fn reserve_with(&mut self, additional: usize) -> Result<&mut [MaybeUninit<u8>], io::Error> {
        self.0.reserve(additional);

        unsafe {
            let ptr = self.0.as_mut_ptr().add(self.0.len()) as *mut MaybeUninit<u8>;
            Ok(from_raw_parts_mut(ptr, additional))
        }
    }

    #[inline(always)]
    unsafe fn flush_len(&mut self, additional: usize) {
        unsafe {
            let new_len = self.0.len() + additional;
            self.0.set_len(new_len);
        }
    }
}
