#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

use http::header;
use salvo::prelude::*;
use serde::Serialize;

#[derive(Serialize)]
pub struct Message {
    pub message: &'static str,
}

#[fn_handler]
async fn json(res: &mut Response) {
    res.headers_mut()
        .insert(header::SERVER, header::HeaderValue::from_static("Salvo"));
    let msg = Message {
        message: "Hello, World!",
    };
    res.render_json(&msg);
}

#[fn_handler]
async fn plaintext(res: &mut Response) {
    res.headers_mut()
        .insert(header::SERVER, header::HeaderValue::from_static("Salvo"));
    res.render_plain_text("Hello, World!");
}

#[tokio::main]
async fn main() {
    println!("Started http server: 127.0.0.1:8080");
    let router = Router::new()
        .push(Router::new().path("json").get(json))
        .push(Router::new().path("plaintext").get(plaintext));
    Server::new(router).bind(([0, 0, 0, 0], 8080)).await;
}
