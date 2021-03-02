#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

use salvo::http::header::{self, HeaderValue};
use salvo::prelude::*;
use simd_json_derive::Serialize;

#[derive(Serialize)]
pub struct Message {
    pub message: &'static str,
}

#[fn_handler]
async fn json(res: &mut Response) {
    res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
    let msg = Message {
        message: "Hello, World!",
    };
    res.render_binary(HeaderValue::from_static("application/json"), &msg.json_vec().unwrap());
}

#[fn_handler]
async fn plaintext(res: &mut Response) {
    res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
    res.render_binary(HeaderValue::from_static("text/plain"), b"Hello, World!");
}

#[tokio::main]
async fn main() {
    println!("Started http server: 127.0.0.1:8080");
    let router = Router::new()
        .push(Router::new().path("json").get(json))
        .push(Router::new().path("plaintext").get(plaintext));
    Server::new(router).bind(([0, 0, 0, 0], 8080)).await;
}
