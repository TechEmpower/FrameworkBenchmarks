// #[global_allocator]
// static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::sync::Arc;
use std::thread::available_parallelism;

use bytes::Bytes;
use salvo::conn::tcp::TcpAcceptor;
use salvo::http::body::ResBody;
use salvo::http::header::{self, HeaderValue};
use salvo::prelude::*;
use serde::Serialize;

mod utils;


#[derive(Serialize)]
pub struct Message {
    pub message: &'static str,
}

#[handler]
fn json(res: &mut Response) {
    let headers = res.headers_mut();
    headers.insert(header::SERVER, HeaderValue::from_static("salvo"));
    headers.insert(header::CONTENT_TYPE, HeaderValue::from_static("application/json"));
    let data = serde_json::to_vec(&Message {
        message: "Hello, World!",
    })
    .unwrap();
    res.body(ResBody::Once(Bytes::from(data)));
}

#[handler]
fn plaintext(res: &mut Response) {
    let headers = res.headers_mut();
    headers.insert(header::SERVER, HeaderValue::from_static("salvo"));
    headers.insert(header::CONTENT_TYPE, HeaderValue::from_static("text/plain"));
    res.body(ResBody::Once(Bytes::from_static(b"Hello, world!")));
}

fn main() {
    let router = Arc::new(
        Router::new()
            .push(Router::with_path("plaintext").get(plaintext))
            .push(Router::with_path("json").get(json)),
    );

    let thread_count = available_parallelism().map(|n| n.get()).unwrap_or(16);
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    for _ in 1..thread_count {
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
    rt.block_on(serve(router));
}

async fn serve(router: Arc<Router>) {
    // let acceptor: TcpAcceptor = utils::reuse_listener().unwrap().try_into().unwrap();
    let acceptor: TcpAcceptor = utils::reuse_listener().unwrap().try_into().unwrap();
    let mut server = Server::new(acceptor);
    let http1 = server.http1_mut();
    http1.pipeline_flush(true);
    server.serve(router).await
}
