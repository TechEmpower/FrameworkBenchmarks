#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use ntex::http::header::{CONTENT_TYPE, SERVER};
use ntex::{http, time::Seconds, util::PoolId, web};
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

    let mut response = web::HttpResponse::with_body(http::StatusCode::OK, body.into());
    response.headers_mut().append(SERVER, utils::HDR_SERVER);
    response
        .headers_mut()
        .append(CONTENT_TYPE, utils::HDR_JSON_CONTENT_TYPE);
    response
}

#[web::get("/plaintext")]
async fn plaintext() -> web::HttpResponse {
    let mut response = web::HttpResponse::with_body(
        http::StatusCode::OK,
        http::body::Body::Bytes(utils::BODY_PLAIN_TEXT),
    );
    response.headers_mut().append(SERVER, utils::HDR_SERVER);
    response
        .headers_mut()
        .append(CONTENT_TYPE, utils::HDR_TEXT_CONTENT_TYPE);
    response
}

#[ntex::main]
async fn main() -> std::io::Result<()> {
    println!("Started http server: 127.0.0.1:8080");

    // start http server
    ntex::server::build()
        .backlog(1024)
        .bind("techempower", "0.0.0.0:8080", |cfg| {
            cfg.memory_pool(PoolId::P1);
            PoolId::P1.set_read_params(65535, 8192);
            PoolId::P1.set_write_params(65535, 8192);

            http::HttpService::build()
                .keep_alive(http::KeepAlive::Os)
                .client_timeout(Seconds(0))
                .h1(web::App::new().service(json).service(plaintext).finish())
        })?
        .run()
        .await
}
