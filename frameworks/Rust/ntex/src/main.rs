#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use ntex::http::header::{HeaderValue, CONTENT_TYPE, SERVER};
use ntex::{http, util::Bytes, web};
use yarte::Serialize;

mod utils;

#[derive(Serialize)]
pub struct Message {
    pub message: &'static str,
}

#[web::get("/json")]
async fn json() -> web::HttpResponse {
    let mut body = Vec::with_capacity(utils::SIZE);
    Message {
        message: "Hello, World!",
    }
    .to_bytes_mut(&mut body);

    web::HttpResponse::Ok()
        .header(SERVER, HeaderValue::from_static("N"))
        .header(CONTENT_TYPE, HeaderValue::from_static("application/json"))
        .body(body)
}

#[web::get("/plaintext")]
async fn plaintext() -> web::HttpResponse {
    web::HttpResponse::Ok()
        .header(SERVER, HeaderValue::from_static("N"))
        .header(CONTENT_TYPE, HeaderValue::from_static("text/plain"))
        .body(Bytes::from_static(b"Hello, World!"))
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
                .disconnect_timeout(0)
                .buffer_params(65535, 65535, 1024)
                .h1(web::App::new()
                    .service(json)
                    .service(plaintext)
                    .with_config(web::dev::AppConfig::default()))
                .tcp()
        })?
        .start()
        .await
}
