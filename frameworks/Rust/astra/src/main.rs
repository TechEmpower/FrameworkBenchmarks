#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use astra::{Body, Request, Response, ResponseBuilder, Server};
use http::StatusCode;

fn main() {
    Server::bind("0.0.0.0:8080")
        .max_workers(num_cpus::get() * 20)
        .http1_pipeline_flush(true)
        .http1_only(true)
        .serve(serve)
        .expect("failed to start server");
}

fn serve(req: Request) -> Response {
    let (req, _) = req.into_parts();

    let mut headers = req.headers;
    headers.clear();

    let body = match req.uri.path() {
        "/plaintext" => {
            static HELLO_WORLD: &'static [u8] = b"Hello, world!";
            headers.insert(header::CONTENT_LENGTH, header::THIRTEEN.clone());
            headers.insert(header::CONTENT_TYPE, header::PLAIN_TEXT.clone());
            Body::new(HELLO_WORLD)
        }
        "/json" => {
            let response = unsafe {
                simd_json::to_vec(&Json {
                    message: "Hello, world!",
                })
                .unwrap_unchecked()
            };

            headers.insert(header::CONTENT_LENGTH, header::TWENTY_SEVEN.clone());
            headers.insert(header::CONTENT_TYPE, header::JSON.clone());
            Body::new(response)
        }
        _ => {
            return not_found();
        }
    };

    headers.insert(header::SERVER, header::ASTRA.clone());
    let mut res = Response::new(body);
    *res.headers_mut() = headers;
    res
}

#[cold]
#[inline(never)]
fn not_found() -> Response {
    unsafe {
        ResponseBuilder::new()
            .status(StatusCode::NOT_FOUND)
            .body(Body::empty())
            .unwrap_unchecked()
    }
}

#[derive(serde::Serialize, serde::Deserialize)]
struct Json {
    message: &'static str,
}

mod header {
    pub use http::header::*;

    pub static ASTRA: HeaderValue = HeaderValue::from_static("astra");
    pub static THIRTEEN: HeaderValue = HeaderValue::from_static("13");
    pub static TWENTY_SEVEN: HeaderValue = HeaderValue::from_static("27");
    pub static PLAIN_TEXT: HeaderValue = HeaderValue::from_static("text/plain");
    pub static JSON: HeaderValue = HeaderValue::from_static("application/json");
}
