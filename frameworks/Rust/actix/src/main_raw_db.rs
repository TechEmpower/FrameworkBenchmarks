extern crate actix;
extern crate actix_web;
extern crate bytes;
extern crate futures;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
extern crate num_cpus;
extern crate postgres;
extern crate rand;
#[macro_use]
extern crate diesel;

use std::{cmp, io};

use actix::prelude::*;
use actix_web::server::{self, HttpHandler, HttpHandlerTask, HttpServer, Writer};
use actix_web::{Error, HttpRequest};
use bytes::BytesMut;
use futures::{Async, Future, Poll};
use postgres::{Connection, TlsMode};

mod db_pg;
mod models;
mod utils;

use db_pg::{PgConnection, RandomWorld, RandomWorlds, UpdateWorld};
use utils::Writer as JsonWriter;

const HTTPOK: &[u8] = b"HTTP/1.1 200 OK\r\n";
const HDR_SERVER: &[u8] = b"Server: Actix\r\n";
const HDR_JSON: &[u8] = b"Content-Type: application/json";

struct App {
    db: Addr<Syn, PgConnection>,
}

impl HttpHandler for App {
    fn handle(&mut self, req: HttpRequest) -> Result<Box<HttpHandlerTask>, HttpRequest> {
        match req.path() {
            "/db" => Ok(Box::new(World {
                fut: Box::new(self.db.send(RandomWorld)),
            })),
            "/queries" => {
                let q = req
                    .query()
                    .get("q")
                    .map(|q| {
                        cmp::min(500, cmp::max(1, q.parse::<u16>().ok().unwrap_or(1)))
                    })
                    .unwrap_or(1);
                Ok(Box::new(Queries {
                    fut: Box::new(self.db.send(RandomWorlds(q))),
                }))
            }
            "/updates" => {
                let q = req
                    .query()
                    .get("q")
                    .map(|q| {
                        cmp::min(500, cmp::max(1, q.parse::<u16>().ok().unwrap_or(1)))
                    })
                    .unwrap_or(1);
                Ok(Box::new(Updates {
                    fut: Box::new(self.db.send(UpdateWorld(q))),
                }))
            }
            _ => Err(req),
        }
    }
}

struct World {
    fut: Box<Future<Item = io::Result<models::World>, Error = actix::MailboxError>>,
}

impl HttpHandlerTask for World {
    fn poll_io(&mut self, io: &mut Writer) -> Poll<bool, Error> {
        match self.fut.poll() {
            Ok(Async::Ready(Ok(row))) => {
                let mut body = BytesMut::with_capacity(31);
                serde_json::to_writer(JsonWriter(&mut body), &row).unwrap();

                let mut bytes = io.buffer();
                bytes.reserve(196);
                bytes.extend_from_slice(HTTPOK);
                bytes.extend_from_slice(HDR_SERVER);
                bytes.extend_from_slice(HDR_JSON);
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

struct Queries {
    fut: Box<Future<Item = io::Result<Vec<models::World>>, Error = actix::MailboxError>>,
}

impl HttpHandlerTask for Queries {
    fn poll_io(&mut self, io: &mut Writer) -> Poll<bool, Error> {
        match self.fut.poll() {
            Ok(Async::Ready(Ok(worlds))) => {
                let mut body = BytesMut::with_capacity(35 * worlds.len());
                serde_json::to_writer(JsonWriter(&mut body), &worlds).unwrap();

                let mut bytes = io.buffer();
                bytes.reserve(196);
                bytes.extend_from_slice(HTTPOK);
                bytes.extend_from_slice(HDR_SERVER);
                bytes.extend_from_slice(HDR_JSON);
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

struct Updates {
    fut: Box<Future<Item = io::Result<Vec<models::World>>, Error = actix::MailboxError>>,
}

impl HttpHandlerTask for Updates {
    fn poll_io(&mut self, io: &mut Writer) -> Poll<bool, Error> {
        match self.fut.poll() {
            Ok(Async::Ready(Ok(worlds))) => {
                let mut body = BytesMut::with_capacity(35 * worlds.len());
                serde_json::to_writer(JsonWriter(&mut body), &worlds).unwrap();

                let mut bytes = io.buffer();
                bytes.reserve(196 + body.len());
                bytes.extend_from_slice(HTTPOK);
                bytes.extend_from_slice(HDR_SERVER);
                bytes.extend_from_slice(HDR_JSON);
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
