#![allow(dead_code, unused_braces)]

use std::{borrow::Cow, cmp, fmt::Display, io};

use bytes::BufMut;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Debug)]
pub struct Fortune {
    pub id: i32,
    pub message: Cow<'static, str>,
}

pub const JSON_MSG_SIZE: usize = 27;

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

pub const CONNECTION_POOL_SIZE: usize = 40;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    err: anyhow::Error,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.err.fmt(f)
    }
}

impl actix_web::error::ResponseError for Error {}

impl<T> From<T> for Error
where
    T: Into<anyhow::Error>,
{
    fn from(e: T) -> Self {
        Error { err: e.into() }
    }
}

pub struct Queries {
    pub q: usize,
}

impl<'de> Deserialize<'de> for Queries {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Debug, Deserialize)]
        struct Q {
            q: Option<String>,
        }

        let q = Q::deserialize(deserializer)?;
        let n = match q.q {
            Some(s) => {
                let i: i32 = s.parse().unwrap_or(1);
                std::cmp::max(1, std::cmp::min(500, i)) as usize
            }
            None => 1,
        };
        Ok(Queries { q: n })
    }
}
