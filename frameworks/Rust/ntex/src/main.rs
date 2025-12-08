#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use ntex::http::header::{CONTENT_TYPE, SERVER};
use ntex::{http, util::BytesMut, web};
use sonic_rs::Serialize;

mod utils;

#[derive(Serialize)]
pub struct Message {
    pub message: &'static str,
}

#[web::get("/json")]
async fn json() -> web::HttpResponse {
    let mut body = BytesMut::with_capacity(utils::SIZE);
    sonic_rs::to_writer(
        utils::BytesWriter(&mut body),
        &Message {
            message: "Hello, World!",
        },
    )
    .unwrap();

    let mut response = web::HttpResponse::with_body(http::StatusCode::OK, body.into());
    response.headers_mut().insert(SERVER, utils::HDR_SERVER);
    response
        .headers_mut()
        .insert(CONTENT_TYPE, utils::HDR_JSON_CONTENT_TYPE);
    response
}

#[web::get("/plaintext")]
async fn plaintext() -> web::HttpResponse {
    let mut response = web::HttpResponse::with_body(
        http::StatusCode::OK,
        http::body::Body::Bytes(utils::BODY_PLAIN_TEXT),
    );
    response.headers_mut().insert(SERVER, utils::HDR_SERVER);
    response
        .headers_mut()
        .insert(CONTENT_TYPE, utils::HDR_TEXT_CONTENT_TYPE);
    response
}

#[ntex::main]
async fn main() -> std::io::Result<()> {
    println!("Started http server: 127.0.0.1:8080");

    // start http server
    ntex::server::build()
        .backlog(1024)
        .enable_affinity()
        .bind("tfb", "0.0.0.0:8080", async |_| {
            http::HttpService::h1(web::App::new().service(json).service(plaintext).finish())
        })?
        .config("tfb", utils::config())
        .run()
        .await
}
