#![allow(dead_code)]

use std::cmp;

use xitca_http::http::header::HeaderValue;

pub trait QueryParse {
    fn parse_query(self) -> u16;
}

impl QueryParse for Option<&str> {
    fn parse_query(self) -> u16 {
        let num = self
            .and_then(|this| {
                use atoi::FromRadix10;
                this.find('q')
                    .map(|pos| u16::from_radix_10(this.split_at(pos + 2).1.as_ref()).0)
            })
            .unwrap_or(1);

        cmp::min(500, cmp::max(1, num))
    }
}

#[allow(clippy::declare_interior_mutable_const)]
pub const SERVER_HEADER_VALUE: HeaderValue = HeaderValue::from_static("TFB");

use nanorand::{Rng, WyRand};

pub type Error = Box<dyn std::error::Error + Send + Sync + 'static>;
pub type HandleResult<T> = Result<T, Error>;

pub const DB_URL: &str = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

#[derive(Default)]
pub struct Rand(WyRand);

impl Rand {
    #[inline]
    pub fn gen_id(&mut self) -> i32 {
        (self.0.generate::<u32>() % 10_000 + 1) as _
    }
}
