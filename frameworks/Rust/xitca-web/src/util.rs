#![allow(dead_code)]

use core::cell::RefCell;

use xitca_http::{bytes::BytesMut, http::header::HeaderValue};

pub trait QueryParse {
    fn parse_query(self) -> u16;
}

impl QueryParse for Option<&str> {
    fn parse_query(self) -> u16 {
        self.and_then(|this| {
            use atoi::FromRadix10;
            this.find('q')
                .map(|pos| u16::from_radix_10(this.split_at(pos + 2).1.as_ref()).0)
        })
        .unwrap_or(1)
        .clamp(1, 500)
    }
}

pub fn bulk_update_gen<F>(func: F) -> String
where
    F: FnOnce(&mut String),
{
    const PREFIX: &str = "UPDATE world SET randomNumber = w.r FROM (VALUES ";
    const SUFFIX: &str = ") AS w (i,r) WHERE world.id = w.i";

    let mut query = String::from(PREFIX);

    func(&mut query);

    if query.ends_with(',') {
        query.pop();
    }

    query.push_str(SUFFIX);

    query
}

#[allow(clippy::declare_interior_mutable_const)]
pub const SERVER_HEADER_VALUE: HeaderValue = HeaderValue::from_static("X");

pub type Error = Box<dyn std::error::Error + Send + Sync + 'static>;

pub type HandleResult<T> = Result<T, Error>;

pub const DB_URL: &str = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

pub struct State<DB> {
    pub client: DB,
    pub write_buf: RefCell<BytesMut>,
}

impl<DB> State<DB> {
    pub fn new(client: DB) -> Self {
        Self {
            client,
            write_buf: Default::default(),
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
mod non_wasm {
    use rand::{rngs::SmallRng, Rng, SeedableRng};

    pub struct Rand(SmallRng);

    impl Default for Rand {
        fn default() -> Self {
            Self(SmallRng::from_entropy())
        }
    }

    impl Rand {
        #[inline]
        pub fn gen_id(&mut self) -> i32 {
            self.0.gen_range(1..=10000)
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub use non_wasm::*;
