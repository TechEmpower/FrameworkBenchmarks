#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::sync::Arc;
use std::thread::available_parallelism;

use bytes::BytesMut;
use salvo::http::header::{self, HeaderValue};
use salvo::http::response::Body;
use salvo::prelude::*;
use serde::Serialize;

mod server;

static HELLO_WORLD: &'static [u8] = b"Hello, world!";
#[derive(Serialize)]
pub struct Message {
    pub message: &'static str,
}

#[fn_handler]
fn json(res: &mut Response) {
    let headers = res.headers_mut();
    headers.insert(header::SERVER, HeaderValue::from_static("S"));
    headers.insert(header::CONTENT_TYPE, HeaderValue::from_static("application/json"));
    let data = serde_json::to_vec(&Message {
        message: "Hello, World!",
    })
    .unwrap();
    res.set_body(Body::Bytes(BytesMut::from(data.as_slice())));
}

#[fn_handler]
fn plaintext(res: &mut Response) {
    let headers = res.headers_mut();
    headers.insert(header::SERVER, HeaderValue::from_static("S"));
    headers.insert(header::CONTENT_TYPE, HeaderValue::from_static("text/plain"));
    res.set_body(Body::Bytes(BytesMut::from(HELLO_WORLD)));
}

fn main() {
    let router = Arc::new(
        Router::new()
            .push(Router::with_path("plaintext").get(plaintext))
            .push(Router::with_path("json").get(json)),
    );

    for _ in 1..available_parallelism().map(|n| n.get()).unwrap_or(16) {
        let router = router.clone();
        std::thread::spawn(move || {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap();
            rt.block_on(serve(router));
        });
    }
    println!("Started http server: 127.0.0.1:8080");
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    rt.block_on(serve(router));
}

async fn serve(router: Arc<Router>) {
    server::builder()
        .http1_pipeline_flush(true)
        .serve(Service::new(router))
        .await
        .unwrap();
}
