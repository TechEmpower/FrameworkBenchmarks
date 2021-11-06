#![allow(clippy::declare_interior_mutable_const)]

use std::{
    cell::{RefCell, RefMut},
    cmp,
    convert::Infallible,
    io,
};

use serde::Serialize;
use xitca_web::{
    dev::bytes::{Bytes, BytesMut},
    http::{
        header::{HeaderValue, CONTENT_TYPE, SERVER},
        StatusCode,
    },
    request::WebRequest,
    response::{WebResponse, WebResponseBuilder},
};

use super::ser::Message;

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

const TEXT_HEADER_VALUE: HeaderValue = HeaderValue::from_static("text/plain");

const JSON_HEADER_VALUE: HeaderValue = HeaderValue::from_static("application/json");

pub(super) fn plain_text<D>(req: &mut WebRequest<'_, D>) -> HandleResult {
    let mut res = req.as_response(Bytes::from_static(b"Hello, World!"));

    res.headers_mut().append(SERVER, SERVER_HEADER_VALUE);
    res.headers_mut().append(CONTENT_TYPE, TEXT_HEADER_VALUE);

    Ok(res)
}

#[inline(always)]
pub(super) fn json<D>(req: &mut WebRequest<'_, AppState<D>>) -> HandleResult {
    json_response(req, &Message::new())
}

#[inline]
pub(super) fn json_response<S, D>(req: &mut WebRequest<'_, AppState<D>>, value: &S) -> HandleResult
where
    S: ?Sized + Serialize,
{
    let mut writer = req.state().writer();
    simd_json::to_writer(&mut writer, value).unwrap();
    let body = writer.take();

    let mut res = req.as_response(body);
    res.headers_mut().append(SERVER, SERVER_HEADER_VALUE);
    res.headers_mut().append(CONTENT_TYPE, JSON_HEADER_VALUE);

    Ok(res)
}

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
