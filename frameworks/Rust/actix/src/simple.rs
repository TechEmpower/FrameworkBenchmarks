extern crate actix;
extern crate actix_web;
extern crate bytes;
extern crate futures;
extern crate serde;
extern crate serde_json;
#[macro_use] extern crate serde_derive;

use bytes::BytesMut;
use actix::prelude::*;
use actix_web::{http, server, App, HttpRequest, HttpResponse};

mod utils;
use utils::Writer;

const SIZE: usize = 29;

#[derive(Serialize, Deserialize)]
pub struct Message {
    pub message: &'static str,
}

fn json(req: HttpRequest) -> HttpResponse {
    let message = Message {
        message: "Hello, World!"
    };
    let mut body = BytesMut::with_capacity(SIZE);
    serde_json::to_writer(Writer(&mut body), &message).unwrap();

    HttpResponse::build_from(&req)
        .header(http::header::SERVER, "Actix")
        .header(http::header::CONTENT_TYPE, "application/json")
        .body(body)
}

fn plaintext(req: HttpRequest) -> HttpResponse {
    HttpResponse::build_from(&req)
        .header(http::header::SERVER, "Actix")
        .header(http::header::CONTENT_TYPE, "text/plain")
        .body("Hello, World!")
}

fn main() {
    let sys = System::new("techempower");

    // start http server
    server::new(
        move || App::new()
            .resource("/plaintext", |r| r.f(plaintext))
            .resource("/json", |r| r.f(json)))
        .backlog(8192)
        .bind("0.0.0.0:8080").unwrap()
        .start();

    println!("Started http server: 127.0.0.1:8080");
    let _ = sys.run();
}
