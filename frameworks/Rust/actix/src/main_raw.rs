extern crate actix;
extern crate actix_web;
extern crate bytes;
extern crate futures;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

use actix::prelude::*;
use actix_web::{HttpRequest, Error};
use actix_web::server::{HttpServer, HttpHandler, HttpHandlerTask, Writer};
use bytes::BytesMut;
use futures::{Async, Poll};

mod utils;
use utils::{Message, SIZE, Writer as JsonWriter};

const HTTPOK: &[u8] = b"HTTP/1.1 200 OK\r\n";
const HDR_SERVER: &[u8] = b"Server: Actix\r\n";
const HDR_CTPLAIN: &[u8] = b"Content-Type: text/plain";
const HDR_CTJSON: &[u8] = b"Content-Type: application/json";
const HDR_CTHTML: &[u8] = b"Content-Type: text/html";
const BODY: &[u8] = b"Hello, World!";

struct App;

impl HttpHandler for App {
    fn handle(&mut self, req: HttpRequest) -> Result<Box<HttpHandlerTask>, HttpRequest> {
        match req.path() {
            "/plaintext" => Ok(Box::new(Plaintext)),
            "/json" => Ok(Box::new(Json)),
            _ => Err(req),
        }
    }
}

struct Plaintext;

impl HttpHandlerTask for Plaintext {
    fn poll_io(&mut self, io: &mut Writer) -> Poll<bool, Error> {
        let mut bytes = io.buffer();
        bytes.reserve(196);
        bytes.extend_from_slice(HTTPOK);
        bytes.extend_from_slice(HDR_SERVER);
        bytes.extend_from_slice(HDR_CTPLAIN);
        server::write_content_length(BODY.len(), &mut bytes);
        io.set_date(bytes);
        bytes.extend_from_slice(BODY);
        Ok(Async::Ready(true))
    }
}

struct Json;

impl HttpHandlerTask for Json {
    fn poll_io(&mut self, io: &mut Writer) -> Poll<bool, Error> {
        let message = Message {
            message: "Hello, World!",
        };
        let mut body = BytesMut::with_capacity(SIZE);
        serde_json::to_writer(JsonWriter(&mut body), &message).unwrap();

        let mut bytes = io.buffer();
        bytes.reserve(196);
        bytes.extend_from_slice(HTTPOK);
        bytes.extend_from_slice(HDR_SERVER);
        bytes.extend_from_slice(HDR_CTJSON);
        server::write_content_length(body.len(), &mut bytes);
        io.set_date(bytes);
        bytes.extend_from_slice(&body[..]);
        Ok(Async::Ready(true))
    }
}

fn main() {
    let sys = System::new("techempower");

    // start http server
    HttpServer::new(|| vec![App])
        .backlog(8192)
        .workers(1)
        .bind("0.0.0.0:8080")
        .unwrap()
        .start();

    println!("Started http server: 127.0.0.1:8080");
    let _ = sys.run();
}
