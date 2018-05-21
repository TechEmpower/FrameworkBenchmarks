extern crate actix;
extern crate actix_web;
extern crate bytes;
extern crate futures;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate askama;
extern crate num_cpus;
extern crate postgres;
extern crate rand;
#[macro_use]
extern crate diesel;

use std::io;

use actix::prelude::*;
use actix_web::server::{self, HttpHandler, HttpHandlerTask, HttpServer, Writer};
use actix_web::{Error, HttpRequest};
use askama::Template;
use bytes::BytesMut;
use futures::{Async, Future, Poll};
use postgres::{Connection, TlsMode};

mod db_pg;
mod models;
mod utils;

use db_pg::{PgConnection, TellFortune};
use utils::{Message, Writer as JsonWriter, SIZE};

const HTTPOK: &[u8] = b"HTTP/1.1 200 OK\r\n";
const HDR_SERVER: &[u8] = b"Server: Actix\r\n";
const HDR_CTPLAIN: &[u8] = b"Content-Type: text/plain";
const HDR_CTJSON: &[u8] = b"Content-Type: application/json";
const HDR_CTHTML: &[u8] = b"Content-Type: text/html";
const BODY: &[u8] = b"Hello, World!";

struct App {
    db: Addr<Syn, PgConnection>,
}

impl HttpHandler for App {
    fn handle(&mut self, req: HttpRequest) -> Result<Box<HttpHandlerTask>, HttpRequest> {
        match req.path() {
            "/plaintext" => Ok(Box::new(Plaintext)),
            "/json" => Ok(Box::new(Json)),
            "/fortune" => {
                let fut = Box::new(self.db.send(TellFortune));
                Ok(Box::new(Fortune { fut }))
            }
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

struct Fortune {
    fut: Box<
        Future<Item = io::Result<Vec<models::Fortune>>, Error = actix::MailboxError>,
    >,
}

#[derive(Template)]
#[template(path = "fortune.html")]
struct FortuneTemplate<'a> {
    items: &'a Vec<models::Fortune>,
}

impl HttpHandlerTask for Fortune {
    fn poll_io(&mut self, io: &mut Writer) -> Poll<bool, Error> {
        match self.fut.poll() {
            Ok(Async::Ready(Ok(rows))) => {
                let tmpl = FortuneTemplate { items: &rows };
                let body = tmpl.render().unwrap();

                let mut bytes = io.buffer();
                bytes.reserve(196 + body.len());
                bytes.extend_from_slice(HTTPOK);
                bytes.extend_from_slice(HDR_SERVER);
                bytes.extend_from_slice(HDR_CTHTML);
                server::write_content_length(body.len(), &mut bytes);
                io.set_date(bytes);
                bytes.extend_from_slice(body.as_ref());
                Ok(Async::Ready(true))
            }
            Ok(Async::NotReady) => Ok(Async::NotReady),
            Ok(Async::Ready(Err(e))) => Err(e.into()),
            Err(e) => Err(e.into()),
        }
    }
}

fn main() {
    let sys = System::new("techempower");
    let db_url = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

    // Avoid triggering "FATAL: the database system is starting up" error from
    // postgres.
    {
        if Connection::connect(db_url, TlsMode::None).is_err() {
            std::thread::sleep(std::time::Duration::from_secs(5));
        }
    }

    // Start db executor actors
    let addr = SyncArbiter::start(num_cpus::get() * 4, move || {
        db_pg::PgConnection::new(db_url)
    });

    // start http server
    HttpServer::new(move || vec![App { db: addr.clone() }])
        .backlog(8192)
        .bind("0.0.0.0:8080")
        .unwrap()
        .start();

    println!("Started http server: 127.0.0.1:8080");
    let _ = sys.run();
}
