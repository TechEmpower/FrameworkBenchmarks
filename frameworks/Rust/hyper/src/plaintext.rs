use std::convert::Infallible;

use http::header::{CONTENT_LENGTH, CONTENT_TYPE, SERVER};
use http::Response;
use http_body_util::combinators::BoxBody;
use http_body_util::{BodyExt, Full};
use hyper::body::Bytes;

use crate::{Error, Result, SERVER_HEADER, TEXT_PLAIN};

static CONTENT: &[u8] = b"Hello, world!";

pub fn get() -> Result<Response<BoxBody<Bytes, Infallible>>> {
    Response::builder()
        .header(SERVER, SERVER_HEADER.clone())
        .header(CONTENT_TYPE, TEXT_PLAIN.clone())
        .header(CONTENT_LENGTH, CONTENT.len())
        .body(Full::from(CONTENT).boxed())
        .map_err(Error::from)
}
