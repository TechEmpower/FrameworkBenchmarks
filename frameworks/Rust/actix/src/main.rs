#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

#[macro_use]
extern crate serde_derive;

use actix_http::{HttpService, KeepAlive};
use actix_service::map_config;
use actix_web::dev::{AppConfig, Body, Server};
use actix_web::http::header::{CONTENT_TYPE, SERVER};
use actix_web::http::{HeaderValue, StatusCode};
use actix_web::{web, App, HttpResponse};
use bytes::{Bytes, BytesMut};

mod utils;
use utils::{Message, Writer, SIZE};

async fn json() -> HttpResponse {
    let message = Message {
        message: "Hello, World!",
    };
    let mut body = BytesMut::with_capacity(SIZE);
    serde_json::to_writer(Writer(&mut body), &message).unwrap();

    let mut res = HttpResponse::with_body(StatusCode::OK, Body::Bytes(body.freeze()));
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Actix"));
    res.headers_mut()
        .insert(CONTENT_TYPE, HeaderValue::from_static("application/json"));
    res
}

async fn plaintext() -> HttpResponse {
    let mut res = HttpResponse::with_body(
        StatusCode::OK,
        Body::Bytes(Bytes::from_static(b"Hello, World!")),
    );
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Actix"));
    res.headers_mut()
        .insert(CONTENT_TYPE, HeaderValue::from_static("text/plain"));
    res
}

#[actix_rt::main]
async fn main() -> std::io::Result<()> {
    println!("Started http server: 127.0.0.1:8080");

    // start http server
    Server::build()
        .backlog(1024)
        .bind("techempower", "0.0.0.0:8080", || {
            HttpService::build()
                .keep_alive(KeepAlive::Os)
                .h1(map_config(
                    App::new()
                        .service(web::resource("/json").to(json))
                        .service(web::resource("/plaintext").to(plaintext)),
                    |_| AppConfig::default(),
                ))
                .tcp()
        })?
        .start()
        .await
}
