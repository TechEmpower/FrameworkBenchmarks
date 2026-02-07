use std::convert::Infallible;

use http::header::{CONTENT_LENGTH, CONTENT_TYPE, SERVER};
use http::Response;
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

pub fn get() -> Result<Response<BoxBody<Bytes, Infallible>>> {
    let content = serde_json::to_vec(&CONTENT)?;
    Response::builder()
        .header(SERVER, SERVER_HEADER.clone())
        .header(CONTENT_TYPE, APPLICATION_JSON.clone())
        .header(CONTENT_LENGTH, content.len())
        .body(Full::from(content).boxed())
        .map_err(Error::from)
}
