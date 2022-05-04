// #[global_allocator]
// static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;
// #[global_allocator]
// static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[macro_use]
extern crate serde_derive;
extern crate serde_json;

use std::sync::Arc;
use std::thread::available_parallelism;

use salvo::http::header::{self, HeaderValue};
use salvo::prelude::*;

mod server;

static HELLO_WORLD: &'static [u8] = b"Hello, world!";
#[derive(Serialize)]
pub struct Message {
    pub message: &'static str,
}

#[fn_handler]
async fn json(res: &mut Response) {
    res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
    res.render(Json(Message {
        message: "Hello, World!",
    }));
}

#[fn_handler]
async fn plaintext(res: &mut Response) {
    res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
    res.headers_mut().insert(header::CONTENT_TYPE, HeaderValue::from_static("text/plain"));
    res.write_body(HELLO_WORLD).ok();
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
