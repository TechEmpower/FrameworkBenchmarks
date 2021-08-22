#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

#[macro_use]
extern crate serde_derive;
extern crate serde_json;

use salvo::http::header::{self, HeaderValue};
use salvo::prelude::*;
use hyper::server::conn::AddrIncoming;

static HELLO_WORLD: &'static [u8] = b"Hello, world!";
#[derive(Serialize)]
pub struct Message {
    pub message: &'static str,
}

#[fn_handler]
async fn json(res: &mut Response) {
    res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
    res.render_json(&Message {
        message: "Hello, World!",
    });
}

#[fn_handler]
async fn plaintext(res: &mut Response) {
    res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
    res.render_binary(HeaderValue::from_static("text/plain"), HELLO_WORLD);
}

#[tokio::main]
async fn main() {
    println!("Started http server: 127.0.0.1:8080");
    let router = Router::new()
        .push(Router::new().path("plaintext").get(plaintext))
        .push(Router::new().path("json").get(json));
    // Server::new(router).bind(([0, 0, 0, 0], 8080)).await;

    let mut incoming = AddrIncoming::bind(&(([0, 0, 0, 0], 8080)).into()).unwrap();
    incoming.set_nodelay(true);
    salvo::server::builder(incoming).http1_pipeline_flush(true).serve(Service::new(router)).await.unwrap();
}
