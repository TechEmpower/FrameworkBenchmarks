#[macro_use]
extern crate serde_derive;

use actix_web::{http, web, App, HttpResponse, HttpServer};
use bytes::BytesMut;

mod utils;
use utils::{Message, Writer, SIZE};

fn json() -> HttpResponse {
    let message = Message {
        message: "Hello, World!",
    };
    let mut body = BytesMut::with_capacity(SIZE);
    serde_json::to_writer(Writer(&mut body), &message).unwrap();

    HttpResponse::Ok()
        .header(http::header::SERVER, "Actix")
        .header(http::header::CONTENT_TYPE, "application/json")
        .body(body)
}

fn plaintext() -> HttpResponse {
    HttpResponse::Ok()
        .header(http::header::SERVER, "Actix")
        .header(http::header::CONTENT_TYPE, "text/plain")
        .body("Hello, World!")
}

fn main() -> std::io::Result<()> {
    env_logger::init();
    let sys = actix_rt::System::new("techempower");

    // start http server
    HttpServer::new(move || {
        App::new()
            .service(web::resource("/json").to(json))
            .service(web::resource("/plaintext").to(plaintext))
    })
    .backlog(1024)
    .bind("0.0.0.0:8080")?
    .start();

    println!("Started http server: 127.0.0.1:8080");
    sys.run()
}
