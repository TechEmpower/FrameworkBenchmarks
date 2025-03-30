#![allow(clippy::unused_async)]

use serde::Serialize;
use viz::{
    header::{HeaderValue, CONTENT_TYPE, SERVER},
    Bytes, Error, Request, Response, ResponseExt, Result, Router,
};

mod server;
mod utils;

#[derive(Serialize)]
struct Message {
    message: &'static str,
}

async fn plaintext(_: Request) -> Result<Response> {
    let mut res = Response::text("Hello, World!");
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Viz"));
    Ok(res)
}

async fn json(_: Request) -> Result<Response> {
    let mut resp = Response::builder()
        .body(
            http_body_util::Full::new(Bytes::from(
                serde_json::to_vec(&Message {
                    message: "Hello, World!",
                })
                .unwrap(),
            ))
            .into(),
        )
        .unwrap();
    let headers = resp.headers_mut();
    headers.insert(SERVER, HeaderValue::from_static("Viz"));
    headers.insert(
        CONTENT_TYPE,
        HeaderValue::from_static(mime::APPLICATION_JSON.as_ref()),
    );
    Ok(resp)
}

#[tokio::main]
async fn main() -> Result<()> {
    let app = Router::new()
        .get("/plaintext", plaintext)
        .get("/json", json);

    server::serve(app).await.map_err(Error::Boxed)
}
