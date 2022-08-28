// #[global_allocator]
// static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::sync::Arc;

use bytes::Bytes;
use salvo::http::header::{self, HeaderValue};
use salvo::http::response::Body;
use salvo::prelude::*;
use serde::Serialize;

mod server;

#[derive(Serialize)]
pub struct Message {
    pub message: &'static str,
}

#[handler]
fn json(res: &mut Response) {
    let headers = res.headers_mut();
    headers.insert(header::SERVER, HeaderValue::from_static("S"));
    headers.insert(
        header::CONTENT_TYPE,
        HeaderValue::from_static("application/json"),
    );
    let data = serde_json::to_vec(&Message {
        message: "Hello, World!",
    })
    .unwrap();
    res.set_body(Body::Once(Bytes::from(data)));
}

#[handler]
fn plaintext(res: &mut Response) {
    let headers = res.headers_mut();
    headers.insert(header::SERVER, HeaderValue::from_static("S"));
    headers.insert(header::CONTENT_TYPE, HeaderValue::from_static("text/plain"));
    res.set_body(Body::Once(Bytes::from_static(b"Hello, world!")));
}

#[tokio::main]
async fn main() {
    let router = Arc::new(
        Router::new()
            .push(Router::with_path("plaintext").get(plaintext))
            .push(Router::with_path("json").get(json)),
    );

    server::builder()
        .http1_pipeline_flush(true)
        .serve(Service::new(router))
        .await
        .unwrap();
}
