#![allow(clippy::unused_async)]

use serde::Serialize;
use vidi::{
    Bytes, Request, Response, ResponseExt, Result, Router,
    header::{CONTENT_TYPE, HeaderValue},
};

mod server;
mod utils;

#[derive(Serialize)]
struct Message {
    message: &'static str,
}

const HELLO_WORLD: &str = "Hello, World!";

#[inline(always)]
async fn plaintext(_: Request) -> Result<Response> {
    Ok(Response::text(HELLO_WORLD))
}

#[inline(always)]
async fn json(_: Request) -> Result<Response> {
    let mut res = Response::builder()
        .body(
            http_body_util::Full::new(Bytes::from(
                serde_json::to_vec(&Message {
                    message: HELLO_WORLD,
                })
                .unwrap(),
            ))
            .into(),
        )
        .unwrap();
    let headers = res.headers_mut();
    headers.insert(
        CONTENT_TYPE,
        HeaderValue::from_static(mime::APPLICATION_JSON.as_ref()),
    );
    Ok(res)
}

async fn app() {
    let app = Router::new()
        .get("/plaintext", plaintext)
        .get("/json", json);

    server::serve(app).await.unwrap();
}

fn main() {
    server::run(app)
}
