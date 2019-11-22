use actix_web::{web, App, HttpServer, HttpResponse, Responder};
use actix_web::dev::Body;
use actix_web::http::header::{CONTENT_TYPE, SERVER};
use actix_web::http::{HeaderValue, StatusCode};
use bytes::{Bytes, BytesMut};

async fn index(info: web::Path<(u32, String)>) -> impl Responder {
    format!("Hello {}! id:{}", info.1, info.0)
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

fn main() -> std::io::Result<()> {
    HttpServer::new(
        || App::new().service(
              web::resource("plaintext").to(plaintext)))
        .bind("127.0.0.1:8080")?
        .run()
}