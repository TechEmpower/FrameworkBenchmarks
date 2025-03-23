use std::convert::Infallible;

use http::header::{CONTENT_LENGTH, CONTENT_TYPE, SERVER};
use http::{HeaderValue, Response};
use http_body_util::combinators::BoxBody;
use http_body_util::{BodyExt, Full};
use hyper::body::Bytes;
use serde::Serialize;

use crate::{Error, Result, APPLICATION_JSON, SERVER_HEADER};

#[derive(Serialize)]
struct JsonResponse<'a> {
    message: &'a str,
}

static CONTENT: JsonResponse = JsonResponse {
    message: "Hello, world!",
};

/// The `Content-Length` header value for the JSON response.
///
/// This is a static value because the length of the JSON response is known at compile time.
static CONTENT_LENGTH_HEADER: HeaderValue = HeaderValue::from_static("27");

pub fn get() -> Result<Response<BoxBody<Bytes, Infallible>>> {
    let content = serde_json::to_vec(&CONTENT)?;
    Response::builder()
        .header(SERVER, SERVER_HEADER.clone())
        .header(CONTENT_TYPE, APPLICATION_JSON.clone())
        .header(CONTENT_LENGTH, CONTENT_LENGTH_HEADER.clone())
        .body(Full::from(content).boxed())
        .map_err(Error::from)
}
