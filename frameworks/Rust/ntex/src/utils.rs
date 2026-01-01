#![allow(dead_code)]
use std::{cmp, io, io::Write, mem::MaybeUninit, slice::from_raw_parts_mut};

use atoi::FromRadix10;
use ntex::http::{header::HeaderValue, HttpServiceConfig, KeepAlive};
use ntex::{io::IoConfig, time::Seconds, util::BufMut, util::Bytes, util::BytesMut, SharedCfg};
use sonic_rs::writer::WriteExt;

pub const HDR_SERVER: HeaderValue = HeaderValue::from_static("N");
pub const HDR_JSON_CONTENT_TYPE: HeaderValue = HeaderValue::from_static("application/json");
pub const HDR_TEXT_CONTENT_TYPE: HeaderValue = HeaderValue::from_static("text/plain");
pub const HDR_HTML_CONTENT_TYPE: HeaderValue =
    HeaderValue::from_static("text/html; charset=utf-8");
pub const BODY_PLAIN_TEXT: Bytes = Bytes::from_static(b"Hello, World!");

const HW: usize = 128 * 1024;
pub const SIZE: usize = 27;

pub fn config() -> SharedCfg {
    thread_local! {
        static CFG: SharedCfg = SharedCfg::new("tfb")
        .add(
            IoConfig::new()
                .set_read_buf(65535, 2048, 128)
                .set_write_buf(65535, 2048, 128),
        )
        .add(
            HttpServiceConfig::new()
                .set_keepalive(KeepAlive::Os)
                .set_client_timeout(Seconds::ZERO)
                .set_headers_read_rate(Seconds::ZERO, Seconds::ZERO, 0)
                .set_payload_read_rate(Seconds::ZERO, Seconds::ZERO, 0),
        ).into();
    }
    CFG.with(|cfg| *cfg)
}

pub fn db_config() -> SharedCfg {
    thread_local! {
        static CFG: SharedCfg = SharedCfg::new("tfb-db")
        .add(
            IoConfig::new()
                .set_read_buf(65535, 2048, 128)
                .set_write_buf(65535, 2048, 128),
        ).into()
    }
    CFG.with(|cfg| *cfg)
}

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

impl Write for BytesWriter<'_> {
    fn write(&mut self, src: &[u8]) -> Result<usize, io::Error> {
        self.0.extend_from_slice(src);
        Ok(src.len())
    }

    fn flush(&mut self) -> Result<(), io::Error> {
        Ok(())
    }
}

impl WriteExt for BytesWriter<'_> {
    #[inline(always)]
    fn reserve_with(&mut self, additional: usize) -> Result<&mut [MaybeUninit<u8>], io::Error> {
        self.0.reserve(additional);

        unsafe {
            let ptr = self.0.as_mut_ptr().add(self.0.len()) as *mut MaybeUninit<u8>;
            Ok(from_raw_parts_mut(ptr, additional))
        }
    }

    #[inline(always)]
    unsafe fn flush_len(&mut self, additional: usize) -> io::Result<()> {
        unsafe {
            let new_len = self.0.len() + additional;
            self.0.set_len(new_len);
        }
        Ok(())
    }
}
