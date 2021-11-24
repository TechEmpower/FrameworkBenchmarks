#![allow(clippy::declare_interior_mutable_const)]

use std::{
    cell::{RefCell, RefMut},
    cmp,
    convert::Infallible,
    io,
};

use xitca_web::{
    dev::bytes::{Bytes, BytesMut},
    http::{
        header::{HeaderValue, SERVER},
        StatusCode,
    },
    response::{WebResponse, WebResponseBuilder},
};

pub(super) type HandleResult = Result<WebResponse, Infallible>;

pub(super) struct Writer<'a>(RefMut<'a, BytesMut>);

impl Writer<'_> {
    #[inline]
    pub fn take(mut self) -> Bytes {
        self.0.split().freeze()
    }
}

impl io::Write for &mut Writer<'_> {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.extend_from_slice(buf);
        Ok(buf.len())
    }

    #[inline]
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

pub(super) trait QueryParse {
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

pub(super) struct AppState<C> {
    client: C,
    // a re-usable buffer for write response data.
    write_buf: RefCell<BytesMut>,
}

impl<C> AppState<C> {
    pub(super) fn new(client: C) -> Self {
        let write_buf = RefCell::new(BytesMut::new());
        Self { client, write_buf }
    }

    #[inline]
    pub(super) fn writer(&self) -> Writer<'_> {
        Writer(self.write_buf.borrow_mut())
    }

    #[inline]
    pub(super) fn client(&self) -> &C {
        &self.client
    }
}

pub const SERVER_HEADER_VALUE: HeaderValue = HeaderValue::from_static("TFB");

pub const HTML_HEADER_VALUE: HeaderValue = HeaderValue::from_static("text/html; charset=utf-8");

pub const TEXT_HEADER_VALUE: HeaderValue = HeaderValue::from_static("text/plain");

pub const JSON_HEADER_VALUE: HeaderValue = HeaderValue::from_static("application/json");

macro_rules! error {
    ($error: ident, $code: path) => {
        #[cold]
        #[inline(never)]
        pub(super) fn $error() -> HandleResult {
            Ok(WebResponseBuilder::new()
                .status($code)
                .header(SERVER, SERVER_HEADER_VALUE)
                .body(Bytes::new().into())
                .unwrap())
        }
    };
}

error!(not_found, StatusCode::NOT_FOUND);
error!(internal, StatusCode::INTERNAL_SERVER_ERROR);
