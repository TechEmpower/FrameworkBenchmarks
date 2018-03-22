extern crate actix;
extern crate actix_web;
extern crate bytes;
extern crate http;
extern crate futures;
extern crate serde;
extern crate serde_json;
#[macro_use] extern crate serde_derive;

use actix_web::*;
use actix::prelude::*;
use bytes::BytesMut;
use http::StatusCode;
use http::header::{self, HeaderValue};

mod utils;
use utils::Writer;

const SIZE: usize = 29;

#[derive(Serialize, Deserialize)]
pub struct Message {
    pub message: &'static str,
}

fn json(_: HttpRequest) -> HttpResponse {
    let message = Message {
        message: "Hello, World!"
    };
    let mut body = BytesMut::with_capacity(SIZE);
    serde_json::to_writer(Writer(&mut body), &message).unwrap();

    let mut resp = HttpResponse::new(StatusCode::OK, body.into());
    resp.headers_mut().insert(
        header::SERVER, HeaderValue::from_static("Actix"));
    resp.headers_mut().insert(
        header::CONTENT_TYPE, HeaderValue::from_static("application/json"));
    resp
}

fn plaintext(_: HttpRequest) -> HttpResponse {
    let mut resp = HttpResponse::new(StatusCode::OK, "Hello, World!".into());
    resp.headers_mut().insert(
        header::SERVER, HeaderValue::from_static("Actix"));
    resp.headers_mut().insert(
        header::CONTENT_TYPE, HeaderValue::from_static("text/plain"));
    resp
}

fn main() {
    let sys = System::new("techempower");

    // start http server
    HttpServer::new(
        move || Application::new()
            .resource("/json", |r| r.f(json))
            .resource("/plaintext", |r| r.f(plaintext)))
        .backlog(8192)
        .bind("0.0.0.0:8080").unwrap()
        .start();

    println!("Started http server: 127.0.0.1:8080");
    let _ = sys.run();
}
