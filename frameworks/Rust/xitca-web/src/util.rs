#![allow(dead_code)]

use core::{cell::RefCell, cmp};

use xitca_http::{bytes::BytesMut, http::header::HeaderValue};

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
pub const SERVER_HEADER_VALUE: HeaderValue = HeaderValue::from_static("X");

pub type Error = Box<dyn std::error::Error + Send + Sync + 'static>;

pub type HandleResult<T> = Result<T, Error>;

pub const DB_URL: &str = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

pub struct State<DB> {
    pub client: DB,
    pub write_buf: RefCell<BytesMut>,
}

#[cfg(not(target_arch = "wasm32"))]
mod non_wasm {
    #[derive(Default)]
    pub struct Rand(nanorand::WyRand);

    impl Rand {
        #[inline]
        pub fn gen_id(&mut self) -> i32 {
            use nanorand::Rng;
            (self.0.generate::<u32>() % 10_000 + 1) as _
        }
    }

    #[cfg(feature = "pg")]
    mod pg_state {
        use core::{cell::RefCell, future::Future, pin::Pin};

        use xitca_http::{
            bytes::BytesMut,
            util::middleware::context::{Context, ContextBuilder},
        };

        use crate::{
            db::{self, Client},
            util::{HandleResult, State},
        };

        pub type Ctx<'a, Req> = Context<'a, Req, State<Client>>;

        pub fn context_mw() -> ContextBuilder<impl Fn() -> Pin<Box<dyn Future<Output = HandleResult<State<Client>>>>>> {
            ContextBuilder::new(|| {
                Box::pin(async {
                    db::create().await.map(|client| State {
                        client,
                        write_buf: RefCell::new(BytesMut::new()),
                    })
                }) as _
            })
        }
    }

    #[cfg(feature = "pg")]
    pub use pg_state::*;
}

#[cfg(not(target_arch = "wasm32"))]
pub use non_wasm::*;
