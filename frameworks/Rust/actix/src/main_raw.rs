extern crate actix;
extern crate actix_web;
extern crate bytes;
extern crate futures;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
extern crate num_cpus;
extern crate rand;
extern crate url;
#[macro_use]
extern crate diesel;
extern crate tokio_postgres;

use std::{mem, io};
use std::cell::RefCell;
use std::rc::Rc;
use std::io::Write;

use actix::prelude::*;
use actix_web::server::{
    self, HttpHandler, HttpHandlerTask, HttpServer, Request, Writer,
};
use actix_web::Error;
use futures::{Async, Future, Poll};
use rand::{thread_rng, Rng};

mod db_pg_direct;
mod models;
mod utils;

use db_pg_direct::PgConnection;
use utils::{Message, StackWriter, Writer as JsonWriter, escape};

const HTTPOK: &[u8] = b"HTTP/1.1 200 OK\r\n";
const HDR_SERVER: &[u8] = b"Server: Actix\r\n";
const HDR_CTPLAIN: &[u8] = b"Content-Type: text/plain";
const HDR_CTJSON: &[u8] = b"Content-Type: application/json";
const HDR_CTHTML: &[u8] = b"Content-Type: text/html; charset=utf-8";
const BODY: &[u8] = b"Hello, World!";

struct App {
    dbs: Rc<RefCell<Vec<PgConnection>>>,
}

impl HttpHandler for App {
    type Task = Box<HttpHandlerTask>;

    fn handle(&self, req: Request) -> Result<Box<HttpHandlerTask>, Request> {
        {
            let path = req.path();
            match path.len() {
                10 if path == "/plaintext" => return Ok(Box::new(Plaintext)),
                5 if path == "/json" => return Ok(Box::new(Json)),
                3 if path == "/db" => {
                    if let Some(db) = thread_rng().choose(&*self.dbs.borrow()) {
                        return Ok(Box::new(World {fut: Box::new(db.get_world())}))
                    }
                }
                8 if path == "/fortune" => {
                    if let Some(db) = thread_rng().choose(&*self.dbs.borrow()) {
                        return Ok(Box::new(Fortune {fut: Box::new(db.tell_fortune())}));
                    }
                }
                8 if path == "/queries" => {
                    let q = utils::get_query_param(req.uri());
                    if let Some(db) = thread_rng().choose(&*self.dbs.borrow()) {
                        return Ok(Box::new(Queries {fut: Box::new(db.get_worlds(q as usize))}));
                    }
                }
                8 if path == "/updates" => {
                    let q = utils::get_query_param(req.uri());
                    if let Some(db) = thread_rng().choose(&*self.dbs.borrow()) {
                        return Ok(Box::new(Updates {fut: Box::new(db.update(q as usize))}));
                    }
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
        {
            let mut bytes = io.buffer();
            bytes.reserve(196);
            bytes.extend_from_slice(HTTPOK);
            bytes.extend_from_slice(HDR_SERVER);
            bytes.extend_from_slice(HDR_CTPLAIN);
            server::write_content_length(13, &mut bytes);
        }
        io.set_date();
        io.buffer().extend_from_slice(BODY);
        Ok(Async::Ready(true))
    }
}

struct Json;

impl HttpHandlerTask for Json {
    fn poll_io(&mut self, io: &mut Writer) -> Poll<bool, Error> {
        let message = Message {
            message: "Hello, World!",
        };

        {
            let mut bytes = io.buffer();
            bytes.reserve(196);
            bytes.extend_from_slice(HTTPOK);
            bytes.extend_from_slice(HDR_SERVER);
            bytes.extend_from_slice(HDR_CTJSON);
            server::write_content_length(27, &mut bytes);
        }
        io.set_date();
        serde_json::to_writer(JsonWriter(io.buffer()), &message).unwrap();
        Ok(Async::Ready(true))
    }
}

struct Fortune {
    fut: Box<Future<Item=Vec<models::Fortune>, Error=io::Error>>,
}

const FORTUNES_START: &[u8] = b"<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";
const FORTUNES_ROW_START: &[u8] = b"<tr><td>";
const FORTUNES_COLUMN: &[u8] = b"</td><td>";
const FORTUNES_ROW_END: &[u8] = b"</td></tr>";
const FORTUNES_END: &[u8] = b"</table></body></html>";


impl HttpHandlerTask for Fortune {
    fn poll_io(&mut self, io: &mut Writer) -> Poll<bool, Error> {
        match self.fut.poll() {
            Ok(Async::Ready(rows)) => {
                let mut body: [u8; 2048] = unsafe { mem::uninitialized() };
                let len = {
                    let mut writer = StackWriter(&mut body, 0);
                    let _ = writer.write(FORTUNES_START);
                    for row in rows {
                        let _ = writer.write(FORTUNES_ROW_START);
                        let _ = write!(&mut writer, "{}", row.id);
                        let _ = writer.write(FORTUNES_COLUMN);
                        escape(&mut writer, row.message);
                        let _ = writer.write(FORTUNES_ROW_END);
                    }
                    let _ = writer.write(FORTUNES_END);
                    writer.1
                };

                {
                    let mut bytes = io.buffer();
                    bytes.reserve(196 + len);
                    bytes.extend_from_slice(HTTPOK);
                    bytes.extend_from_slice(HDR_SERVER);
                    bytes.extend_from_slice(HDR_CTHTML);
                    server::write_content_length(len, &mut bytes);
                }
                io.set_date();
                io.buffer().extend_from_slice(&body[..len]);
                Ok(Async::Ready(true))
            }
            Ok(Async::NotReady) => Ok(Async::NotReady),
            Err(e) => Err(e.into()),
        }
    }
}

struct World {
    fut: Box<Future<Item=models::World, Error=io::Error>>,
}

impl HttpHandlerTask for World {
    fn poll_io(&mut self, io: &mut Writer) -> Poll<bool, Error> {
        match self.fut.poll() {
            Ok(Async::Ready(row)) => {
                let mut body: [u8; 48] = unsafe { mem::uninitialized() };
                let len = {
                    let mut writer = StackWriter(&mut body, 0);
                    serde_json::to_writer(&mut writer, &row).unwrap();
                    writer.1
                };

                {
                    let mut bytes = io.buffer();
                    bytes.reserve(196);
                    bytes.extend_from_slice(HTTPOK);
                    bytes.extend_from_slice(HDR_SERVER);
                    bytes.extend_from_slice(HDR_CTJSON);
                    server::write_content_length(len, &mut bytes);
                }
                io.set_date();
                io.buffer().extend_from_slice(&body[..len]);
                Ok(Async::Ready(true))
            }
            Ok(Async::NotReady) => Ok(Async::NotReady),
            Err(e) => Err(e.into()),
        }
    }
}

struct Queries {
    fut: Box<Future<Item=Vec<models::World>, Error=io::Error>>,
}

impl HttpHandlerTask for Queries {
    fn poll_io(&mut self, io: &mut Writer) -> Poll<bool, Error> {
        match self.fut.poll() {
            Ok(Async::Ready(worlds)) => {
                let mut body: [u8; 24576] = unsafe { mem::uninitialized() };
                let len = {
                    let mut writer = StackWriter(&mut body, 0);
                    serde_json::to_writer(&mut writer, &worlds).unwrap();
                    writer.1
                };

                {
                    let mut bytes = io.buffer();
                    bytes.reserve(196 + len);
                    bytes.extend_from_slice(HTTPOK);
                    bytes.extend_from_slice(HDR_SERVER);
                    bytes.extend_from_slice(HDR_CTJSON);
                    server::write_content_length(len, &mut bytes);
                }
                io.set_date();
                io.buffer().extend_from_slice(&body[..len]);
                Ok(Async::Ready(true))
            }
            Ok(Async::NotReady) => Ok(Async::NotReady),
            Err(e) => Err(e.into()),
        }
    }
}

struct Updates {
    fut: Box<Future<Item=Vec<models::World>, Error=io::Error>>,
}

impl HttpHandlerTask for Updates {
    fn poll_io(&mut self, io: &mut Writer) -> Poll<bool, Error> {
        match self.fut.poll() {
            Ok(Async::Ready(worlds)) => {
                let mut body: [u8; 24576] = unsafe { mem::uninitialized() };
                let len = {
                    let mut writer = StackWriter(&mut body, 0);
                    serde_json::to_writer(&mut writer, &worlds).unwrap();
                    writer.1
                };

                {
                    let mut bytes = io.buffer();
                    bytes.reserve(196 + len);
                    bytes.extend_from_slice(HTTPOK);
                    bytes.extend_from_slice(HDR_SERVER);
                    bytes.extend_from_slice(HDR_CTJSON);
                    server::write_content_length(len, &mut bytes);
                }
                io.set_date();
                io.buffer().extend_from_slice(&body[..len]);
                Ok(Async::Ready(true))
            }
            Ok(Async::NotReady) => Ok(Async::NotReady),
            Err(e) => Err(e.into()),
        }
    }
}

fn main() {
    let sys = System::new("techempower");
    let db_url = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

    // start http server
    HttpServer::new(move || {
        let dbs = Rc::new(RefCell::new(Vec::new()));

        let db = dbs.clone();
        Arbiter::spawn(
            PgConnection::connect(db_url)
                .and_then(move |conn| {
                    db.borrow_mut().push(conn);
                    Ok(())
                }));

        vec![App { dbs }]
    }).backlog(8192)
        .bind("0.0.0.0:8080")
        .unwrap()
        .start();

    println!("Started http server: 127.0.0.1:8080");
    let _ = sys.run();
}
