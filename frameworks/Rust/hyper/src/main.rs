extern crate futures;
extern crate hyper;
extern crate net2;
extern crate num_cpus;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate tokio_core;

use futures::Future;

use hyper::header::{HeaderValue, CONTENT_LENGTH, CONTENT_TYPE, SERVER};
use hyper::service::service_fn_ok;
use hyper::{Body, Response, StatusCode};

mod server;

static HELLO_WORLD: &'static [u8] = b"Hello, world!";

#[derive(Serialize)]
struct JsonResponse<'a> {
    message: &'a str,
}

fn main() {
    // It seems most of the other benchmarks create static header values
    // for performance, so just play by the same rules here...
    let plaintext_len = HeaderValue::from_static("13");
    let plaintext_ct = HeaderValue::from_static("text/plain");
    let json_len = HeaderValue::from_static("27");
    let json_ct = HeaderValue::from_static("application/json");
    let server_header = HeaderValue::from_static("hyper");

    server::run(move |socket, http, handle| {
        // This closure is run for each connection...

        // The plaintext benchmarks use pipelined requests.
        http.pipeline_flush(true);

        // Gotta clone these to be able to move into the Service...
        let plaintext_len = plaintext_len.clone();
        let plaintext_ct = plaintext_ct.clone();
        let json_len = json_len.clone();
        let json_ct = json_ct.clone();
        let server_header = server_header.clone();

        // This is the `Service` that will handle the connection.
        // `service_fn_ok` is a helper to convert a function that
        // returns a Response into a `Service`.
        let svc = service_fn_ok(move |req| {
            let (req, _body) = req.into_parts();
            // For speed, reuse the allocated header map from the request,
            // instead of allocating a new one. Because.
            let mut headers = req.headers;
            headers.clear();

            let body = match req.uri.path() {
                // Apparently, other benchmarks don't check the method, so we
                // don't either. Yay?
                "/plaintext" => {
                    headers.insert(CONTENT_LENGTH, plaintext_len.clone());
                    headers.insert(CONTENT_TYPE, plaintext_ct.clone());
                    Body::from(HELLO_WORLD)
                }
                "/json" => {
                    let rep = JsonResponse {
                        message: "Hello, world!",
                    };
                    let rep_body = serde_json::to_vec(&rep).unwrap();
                    headers.insert(CONTENT_LENGTH, json_len.clone());
                    headers.insert(CONTENT_TYPE, json_ct.clone());
                    Body::from(rep_body)
                }
                _ => {
                    let mut res = Response::new(Body::empty());
                    *res.status_mut() = StatusCode::NOT_FOUND;
                    *res.headers_mut() = headers;
                    return res;
                }
            };

            headers.insert(SERVER, server_header.clone());

            let mut res = Response::new(body);
            *res.headers_mut() = headers;
            res
        });

        // Spawn the `serve_connection` future into the runtime.
        handle.spawn(
            http.serve_connection(socket, svc)
                .map_err(|e| eprintln!("connection error: {}", e)),
        );
    })
}
