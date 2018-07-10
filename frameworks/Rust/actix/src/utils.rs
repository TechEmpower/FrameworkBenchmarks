#![allow(dead_code)]
use bytes::BytesMut;
use std::{cmp, fmt, io};

use actix_web::http::Uri;
use url::form_urlencoded;

pub const SIZE: usize = 31;

#[derive(Serialize, Deserialize)]
pub struct Message {
    pub message: &'static str,
}

pub struct Writer<'a>(pub &'a mut BytesMut);

impl<'a> io::Write for Writer<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.extend_from_slice(buf);
        Ok(buf.len())
    }
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

pub struct StackWriter<'a>(pub &'a mut [u8], pub usize);

impl<'a> io::Write for StackWriter<'a> {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let l = buf.len();
        let new = self.1 + l;
        self.0[self.1..new].copy_from_slice(buf);
        self.1 = new;
        Ok(l)
    }
    #[inline]
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl<'a> fmt::Write for StackWriter<'a> {
    #[inline]
    fn write_str(&mut self, s: &str) -> Result<(), fmt::Error> {
        if !s.is_empty() {
            let b = s.as_bytes();
            let l = b.len();
            let new = self.1 + l;
            self.0[self.1..new].copy_from_slice(b);
            self.1 = new;
        }
        Ok(())
    }
}

pub fn get_query_param(uri: &Uri) -> u16 {
    let mut q = None;
    let q_str = if let Some(s) = uri.query() { s } else { "" };
    for (key, val) in form_urlencoded::parse(q_str.as_ref()) {
        if key == "q" {
            q = Some(val);
            break;
        }
    }

    q.map(|q| cmp::min(500, cmp::max(1, q.parse::<u16>().ok().unwrap_or(1))))
        .unwrap_or(1)
}
