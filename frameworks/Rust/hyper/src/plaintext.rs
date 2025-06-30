use http::header::{CONTENT_LENGTH, CONTENT_TYPE};
use http::Response;
use http_body_util::Full;
use hyper::body::Bytes;

use crate::{Error, Result, TEXT_PLAIN};

static CONTENT: &[u8] = b"Hello, world!";

pub fn get() -> Result<Response<Full<Bytes>>> {
    Response::builder()
        .header(CONTENT_TYPE, TEXT_PLAIN.clone())
        .header(CONTENT_LENGTH, CONTENT.len())
        .body(CONTENT.into())
        .map_err(Error::from)
}
