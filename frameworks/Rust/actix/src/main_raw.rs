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

use std::{cmp, mem};

use actix::prelude::*;
use actix_web::server::{self, HttpHandler, HttpHandlerTask, HttpServer, Writer};
use actix_web::{Error, HttpRequest};
use askama::Template;
use futures::{Async, Future, Poll};
use postgres::{Connection, TlsMode};

mod db_pg;
mod models;
mod utils;

use db_pg::{PgConnection, RandomWorld, RandomWorlds, TellFortune, UpdateWorld};
use utils::{Message, StackWriter, Writer as JsonWriter};

const HTTPOK: &[u8] = b"HTTP/1.1 200 OK\r\n";
const HDR_SERVER: &[u8] = b"Server: Actix\r\n";
const HDR_CTPLAIN: &[u8] = b"Content-Type: text/plain";
const HDR_CTJSON: &[u8] = b"Content-Type: application/json";
const HDR_CTHTML: &[u8] = b"Content-Type: text/html; charset=utf-8";
const BODY: &[u8] = b"Hello, World!";

struct App {
    db: Addr<Syn, PgConnection>,
}

impl HttpHandler for App {
    fn handle(&mut self, req: HttpRequest) -> Result<Box<HttpHandlerTask>, HttpRequest> {
        {
            let path = req.path();
            match path.len() {
                10 if path == "/plaintext" => return Ok(Box::new(Plaintext)),
                5 if path == "/json" => return Ok(Box::new(Json)),
                3 if path == "/db" => {
                    return Ok(Box::new(World {
                        fut: self.db.send(RandomWorld),
                    }))
                }
                8 if path == "/fortune" => {
                    return Ok(Box::new(Fortune {
                        fut: self.db.send(TellFortune),
                    }));
                }
                8 if path == "/queries" => {
                    let q = req
                        .query()
                        .get("q")
                        .map(|q| {
                            cmp::min(
                                500,
                                cmp::max(1, q.parse::<u16>().ok().unwrap_or(1)),
                            )
                        })
                        .unwrap_or(1);
                    return Ok(Box::new(Queries {
                        fut: self.db.send(RandomWorlds(q)),
                    }));
                }
                8 if path == "/updates" => {
                    let q = req
                        .query()
                        .get("q")
                        .map(|q| {
                            cmp::min(
                                500,
                                cmp::max(1, q.parse::<u16>().ok().unwrap_or(1)),
                            )
                        })
                        .unwrap_or(1);
                    return Ok(Box::new(Updates {
                        fut: self.db.send(UpdateWorld(q)),
                    }));
                }
                _ => (),
            }
        }
        Err(req)
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
        server::write_content_length(13, &mut bytes);
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

        let mut bytes = io.buffer();
        bytes.reserve(196);
        bytes.extend_from_slice(HTTPOK);
        bytes.extend_from_slice(HDR_SERVER);
        bytes.extend_from_slice(HDR_CTJSON);
        server::write_content_length(27, &mut bytes);
        io.set_date(bytes);
        serde_json::to_writer(JsonWriter(bytes), &message).unwrap();
        Ok(Async::Ready(true))
    }
}

struct Fortune {
    fut: actix::dev::Request<Syn, PgConnection, TellFortune>,
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
                let mut body: [u8; 2048] = unsafe { mem::uninitialized() };
                let len = {
                    let mut writer = StackWriter(&mut body, 0);
                    let tmpl = FortuneTemplate { items: &rows };
                    tmpl.render_into(&mut writer).unwrap();
                    writer.1
                };

                let mut bytes = io.buffer();
                bytes.reserve(196 + len);
                bytes.extend_from_slice(HTTPOK);
                bytes.extend_from_slice(HDR_SERVER);
                bytes.extend_from_slice(HDR_CTHTML);
                server::write_content_length(len, &mut bytes);
                io.set_date(bytes);
                bytes.extend_from_slice(&body[..len]);
                Ok(Async::Ready(true))
            }
            Ok(Async::NotReady) => Ok(Async::NotReady),
            Ok(Async::Ready(Err(e))) => Err(e.into()),
            Err(e) => Err(e.into()),
        }
    }
}

struct World {
    fut: actix::dev::Request<Syn, PgConnection, RandomWorld>,
}

impl HttpHandlerTask for World {
    fn poll_io(&mut self, io: &mut Writer) -> Poll<bool, Error> {
        match self.fut.poll() {
            Ok(Async::Ready(Ok(row))) => {
                let mut body: [u8; 48] = unsafe { mem::uninitialized() };
                let len = {
                    let mut writer = StackWriter(&mut body, 0);
                    serde_json::to_writer(&mut writer, &row).unwrap();
                    writer.1
                };

                let mut bytes = io.buffer();
                bytes.reserve(196);
                bytes.extend_from_slice(HTTPOK);
                bytes.extend_from_slice(HDR_SERVER);
                bytes.extend_from_slice(HDR_CTJSON);
                server::write_content_length(len, &mut bytes);
                io.set_date(bytes);
                bytes.extend_from_slice(&body[..len]);
                Ok(Async::Ready(true))
            }
            Ok(Async::NotReady) => Ok(Async::NotReady),
            Ok(Async::Ready(Err(e))) => Err(e.into()),
            Err(e) => Err(e.into()),
        }
    }
}

struct Queries {
    fut: actix::dev::Request<Syn, PgConnection, RandomWorlds>,
}

impl HttpHandlerTask for Queries {
    fn poll_io(&mut self, io: &mut Writer) -> Poll<bool, Error> {
        match self.fut.poll() {
            Ok(Async::Ready(Ok(worlds))) => {
                let mut body: [u8; 24576] = unsafe { mem::uninitialized() };
                let len = {
                    let mut writer = StackWriter(&mut body, 0);
                    serde_json::to_writer(&mut writer, &worlds).unwrap();
                    writer.1
                };

                let mut bytes = io.buffer();
                bytes.reserve(196 + len);
                bytes.extend_from_slice(HTTPOK);
                bytes.extend_from_slice(HDR_SERVER);
                bytes.extend_from_slice(HDR_CTJSON);
                server::write_content_length(len, &mut bytes);
                io.set_date(bytes);
                bytes.extend_from_slice(&body[..len]);
                Ok(Async::Ready(true))
            }
            Ok(Async::NotReady) => Ok(Async::NotReady),
            Ok(Async::Ready(Err(e))) => Err(e.into()),
            Err(e) => Err(e.into()),
        }
    }
}

struct Updates {
    fut: actix::dev::Request<Syn, PgConnection, UpdateWorld>,
}

impl HttpHandlerTask for Updates {
    fn poll_io(&mut self, io: &mut Writer) -> Poll<bool, Error> {
        match self.fut.poll() {
            Ok(Async::Ready(Ok(worlds))) => {
                let mut body: [u8; 24576] = unsafe { mem::uninitialized() };
                let len = {
                    let mut writer = StackWriter(&mut body, 0);
                    serde_json::to_writer(&mut writer, &worlds).unwrap();
                    writer.1
                };

                let mut bytes = io.buffer();
                bytes.reserve(196 + len);
                bytes.extend_from_slice(HTTPOK);
                bytes.extend_from_slice(HDR_SERVER);
                bytes.extend_from_slice(HDR_CTJSON);
                server::write_content_length(len, &mut bytes);
                io.set_date(bytes);
                bytes.extend_from_slice(&body[..len]);
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
    let addr = SyncArbiter::start(num_cpus::get() * 3, move || {
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
