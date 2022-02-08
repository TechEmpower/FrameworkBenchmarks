#![allow(dead_code, unused_braces)]

use std::{borrow::Cow, cmp, io};

use bytes::BufMut;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Debug)]
pub struct Fortune {
    pub id: i32,
    pub message: Cow<'static, str>,
}

pub const SIZE: usize = 27;

#[derive(Serialize, Deserialize)]
pub struct Message {
    pub message: &'static str,
}

/// An `io::Write`r that only requires mutable reference and assumes that there is space available
/// in the buffer for every write operation or that it can be extended implicitly (like
/// `bytes::BytesMut`, for example).
///
/// This is slightly faster (~10%) than `bytes::buf::Writer` in such cases because it does not
/// perform a remaining length check before writing.
pub struct Writer<'a, B>(pub &'a mut B);

impl<'a, B: BufMut> io::Write for Writer<'a, B> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.put_slice(buf);
        Ok(buf.len())
    }
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

pub fn get_query_param(query: &str) -> u16 {
    let q = if let Some(pos) = query.find('q') {
        query.split_at(pos + 2).1.parse::<u16>().ok().unwrap_or(1)
    } else {
        1
    };
    cmp::min(500, cmp::max(1, q))
}
