#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

use bytes::{Bytes, BytesMut};
use ntex::{http, web};
use yarte::Serialize;

mod utils;
use utils::SIZE;

#[derive(Serialize)]
pub struct Message {
    pub message: &'static str,
}

async fn json() -> web::HttpResponse {
    let mut res = web::HttpResponse::with_body(
        http::StatusCode::OK,
        http::body::Body::Bytes(
            Message {
                message: "Hello, World!",
            }
            .to_bytes::<BytesMut>(SIZE),
        ),
    );
    res.headers_mut().insert(
        http::header::SERVER,
        http::header::HeaderValue::from_static("N"),
    );
    res.headers_mut().insert(
        http::header::CONTENT_TYPE,
        http::header::HeaderValue::from_static("application/json"),
    );
    res
}

async fn plaintext() -> web::HttpResponse {
    let mut res = web::HttpResponse::with_body(
        http::StatusCode::OK,
        http::body::Body::Bytes(Bytes::from_static(b"Hello, World!")),
    );
    res.headers_mut().insert(
        http::header::SERVER,
        http::header::HeaderValue::from_static("N"),
    );
    res.headers_mut().insert(
        http::header::CONTENT_TYPE,
        http::header::HeaderValue::from_static("text/plain"),
    );
    res
}

#[ntex::main]
async fn main() -> std::io::Result<()> {
    println!("Started http server: 127.0.0.1:8080");

    // start http server
    ntex::server::build()
        .backlog(1024)
        .bind("techempower", "0.0.0.0:8080", || {
            http::HttpService::build()
                .keep_alive(http::KeepAlive::Os)
                .client_timeout(0)
                .h1(ntex::map_config(
                    web::App::new()
                        .service(web::resource("/json").to(json))
                        .service(web::resource("/plaintext").to(plaintext)),
                    |_| web::dev::AppConfig::default(),
                ))
                .tcp()
        })?
        .start()
        .await
}
