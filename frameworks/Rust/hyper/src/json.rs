use http::header::{CONTENT_LENGTH, CONTENT_TYPE};
use http::Response;
use http_body_util::Full;
use hyper::body::Bytes;
use serde::Serialize;

use crate::{Error, Result, APPLICATION_JSON};

#[derive(Serialize)]
struct JsonResponse<'a> {
    message: &'a str,
}

static CONTENT: JsonResponse = JsonResponse {
    message: "Hello, world!",
};

pub fn get() -> Result<Response<Full<Bytes>>> {
    let content = serde_json::to_vec(&CONTENT)?;

    Response::builder()
        .header(CONTENT_TYPE, APPLICATION_JSON.clone())
        .header(CONTENT_LENGTH, content.len())
        .body(content.into())
        .map_err(Error::from)
}
