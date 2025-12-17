#![allow(dead_code)]

use rand::{Rng, SeedableRng, distr::Uniform, rngs::SmallRng};
use xitca_http::{bytes::BytesMut, http::header::HeaderValue};

pub trait QueryParse {
    fn parse_query(self) -> u16;
}

impl QueryParse for Option<&str> {
    fn parse_query(self) -> u16 {
        self.and_then(|q| q.find('q').map(|pos| q.split_at(pos + 2).1.parse_query()))
            .unwrap_or(1)
    }
}

impl QueryParse for &str {
    fn parse_query(self) -> u16 {
        use atoi::FromRadix10;
        u16::from_radix_10(self.as_bytes()).0.clamp(1, 500)
    }
}

#[allow(clippy::declare_interior_mutable_const)]
pub const SERVER_HEADER_VALUE: HeaderValue = HeaderValue::from_static("X");

pub type Error = Box<dyn std::error::Error + Send + Sync + 'static>;

pub type HandleResult<T> = Result<T, Error>;

pub const DB_URL: &str = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

pub struct State<DB> {
    pub client: DB,
    pub write_buf: core::cell::RefCell<BytesMut>,
}

impl<DB> State<DB> {
    pub fn new(client: DB) -> Self {
        Self {
            client,
            write_buf: Default::default(),
        }
    }
}

pub struct Rand(SmallRng);

impl Default for Rand {
    fn default() -> Self {
        Self(SmallRng::from_os_rng())
    }
}

impl Clone for Rand {
    #[inline]
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl Rand {
    #[inline]
    pub fn gen_id(&mut self) -> i32 {
        self.0.random_range(1..=10000)
    }

    #[inline]
    pub fn gen_multi(&mut self) -> impl Iterator<Item = i32> {
        (&mut self.0).sample_iter(Uniform::new(1, 10001).unwrap())
    }
}
