// #[global_allocator]
// static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::sync::Arc;

use bytes::Bytes;
use salvo::conn::tcp::TcpAcceptor;
use salvo::http::body::ResBody;
use salvo::http::header::{self, HeaderValue};
use salvo::prelude::*;
use serde::Serialize;

mod utils;

static SERVER_HEADER: HeaderValue = HeaderValue::from_static("salvo");
static JSON_HEADER: HeaderValue = HeaderValue::from_static("application/json");
static PLAIN_HEADER: HeaderValue = HeaderValue::from_static("text/plain");
static HELLO_WORD: Bytes = Bytes::from_static(b"Hello, world!");

#[derive(Serialize)]
pub struct Message {
    pub message: &'static str,
}

#[handler]
fn json(res: &mut Response) {
    let headers = res.headers_mut();
    headers.insert(header::SERVER, SERVER_HEADER.clone());
    headers.insert(header::CONTENT_TYPE, JSON_HEADER.clone());
    let data = serde_json::to_vec(&Message {
        message: "Hello, World!",
    })
    .unwrap();
    res.body(ResBody::Once(Bytes::from(data)));
}

#[handler]
fn plaintext(res: &mut Response) {
    let headers = res.headers_mut();
    headers.insert(header::SERVER, SERVER_HEADER.clone());
    headers.insert(header::CONTENT_TYPE, PLAIN_HEADER.clone());
    res.body(ResBody::Once(HELLO_WORD.clone()));
}
#[tokio::main]
async 
fn main() {
    let router = Arc::new(
        Router::new()
            .push(Router::with_path("plaintext").get(plaintext))
            .push(Router::with_path("json").get(json)),
    );

    println!("Started http server: 127.0.0.1:8080");
    serve(router).await;
}

async fn serve(router: Arc<Router>) {
    let acceptor: TcpAcceptor = utils::reuse_listener().unwrap().try_into().unwrap();
    let mut server = Server::new(acceptor);
    let http1 = server.http1_mut();
    http1.pipeline_flush(true);
    server.serve(router).await
}
