//! Control benchmark against roa.
//!
//! Should not be included in benchmark_config.json!

use hyper::header::{HeaderValue, CONTENT_LENGTH, CONTENT_TYPE, SERVER};
use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Response, Server};
use lazy_static::lazy_static;
use serde::Serialize;
use std::convert::Infallible;
use std::net::SocketAddr;

static HELLO_WORLD: &'static str = "Hello, world!";

lazy_static! {
    static ref SERVER_HEADER: HeaderValue = HeaderValue::from_static("hyper");
    static ref JSON_LEN: HeaderValue = HeaderValue::from_static("27");
    static ref PLAINTEXT_LEN: HeaderValue = HeaderValue::from_static("13");
    static ref JSON_CT: HeaderValue = HeaderValue::from_static("application/json");
    static ref PLAINTEXT_CT: HeaderValue = HeaderValue::from_static("text/plain");
}

#[derive(Serialize)]
struct JsonResponse<'a> {
    message: &'a str,
}

#[tokio::main]
async fn main() {
    // We'll bind to 127.0.0.1:3000
    let addr = SocketAddr::from(([0, 0, 0, 0], 8080));

    // A `Service` is needed for every connection, so this
    // creates one from our `hello_world` function.
    let make_svc = make_service_fn(move |_conn| async {
        // service_fn converts our function into a `Service`
        Ok::<_, Infallible>(service_fn(|mut req| async move {
            let mut headers = std::mem::take(req.headers_mut());
            headers.clear();
            headers.insert(SERVER, SERVER_HEADER.clone());
            let body = match req.uri().path() {
                "/plaintext" => {
                    headers.insert(CONTENT_LENGTH, PLAINTEXT_LEN.clone());
                    headers.insert(CONTENT_TYPE, PLAINTEXT_CT.clone());
                    Body::from(HELLO_WORLD)
                }
                _ => {
                    headers.insert(CONTENT_LENGTH, JSON_LEN.clone());
                    headers.insert(CONTENT_TYPE, JSON_CT.clone());
                    Body::from(
                        serde_json::to_vec(&JsonResponse {
                            message: HELLO_WORLD,
                        })
                        .unwrap(),
                    )
                }
            };
            let mut resp = Response::new(body);
            *resp.headers_mut() = headers;
            Ok::<_, Infallible>(resp)
        }))
    });

    let server = Server::bind(&addr).serve(make_svc);

    // Run this server for... forever!
    if let Err(e) = server.await {
        eprintln!("server error: {}", e);
    }
}
