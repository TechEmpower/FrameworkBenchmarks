#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate diesel;

use std::io::Write;

use actix_http::body::Body;
use actix_http::http::header::{CONTENT_TYPE, SERVER};
use actix_http::http::{HeaderValue, StatusCode};
use actix_http::{Error, HttpService, KeepAlive, Request, Response};
use actix_server::{Server, ServerConfig};
use actix_service::{NewService, Service};
use bytes::BytesMut;
use futures::future::ok;
use futures::{Async, Future, Poll};
use serde_json::to_writer;

mod db_pg_direct;
mod models;
mod utils;

use crate::db_pg_direct::PgConnection;
use crate::utils::{FortunesTemplate, Writer};

struct App {
    hdr_srv: HeaderValue,
    hdr_ctjson: HeaderValue,
    hdr_cthtml: HeaderValue,
    db: PgConnection,
}

impl Service for App {
    type Request = Request;
    type Response = Response;
    type Error = Error;
    type Future = Box<Future<Item = Response, Error = Error>>;

    #[inline]
    fn poll_ready(&mut self) -> Poll<(), Self::Error> {
        Ok(Async::Ready(()))
    }

    fn call(&mut self, req: Request) -> Self::Future {
        let path = req.path();
        match path.len() {
            3 if path == "/db" => {
                let fut = self.db.get_world();
                let h_srv = self.hdr_srv.clone();
                let h_ct = self.hdr_ctjson.clone();

                Box::new(fut.map(move |body| {
                    let mut res = Response::with_body(StatusCode::OK, Body::Bytes(body));
                    let hdrs = res.headers_mut();
                    hdrs.insert(SERVER, h_srv);
                    hdrs.insert(CONTENT_TYPE, h_ct);
                    res
                }))
            }
            8 if path == "/fortune" => {
                let fut = self.db.tell_fortune();
                let h_srv = self.hdr_srv.clone();
                let h_ct = self.hdr_cthtml.clone();

                Box::new(fut.from_err().map(move |fortunes| {
                    let mut body = BytesMut::with_capacity(2048);
                    let mut writer = Writer(&mut body);
                    let _ = write!(writer, "{}", FortunesTemplate { fortunes });
                    let mut res =
                        Response::with_body(StatusCode::OK, Body::Bytes(body.freeze()));
                    let hdrs = res.headers_mut();
                    hdrs.insert(SERVER, h_srv);
                    hdrs.insert(CONTENT_TYPE, h_ct);
                    res
                }))
            }
            8 if path == "/queries" => {
                let q = utils::get_query_param(req.uri().query().unwrap_or("")) as usize;
                let fut = self.db.get_worlds(q);
                let h_srv = self.hdr_srv.clone();
                let h_ct = self.hdr_ctjson.clone();

                Box::new(fut.from_err().map(move |worlds| {
                    let mut body = BytesMut::with_capacity(35 * worlds.len());
                    to_writer(Writer(&mut body), &worlds).unwrap();
                    let mut res =
                        Response::with_body(StatusCode::OK, Body::Bytes(body.freeze()));
                    let hdrs = res.headers_mut();
                    hdrs.insert(SERVER, h_srv);
                    hdrs.insert(CONTENT_TYPE, h_ct);
                    res
                }))
            }
            8 if path == "/updates" => {
                let q = utils::get_query_param(req.uri().query().unwrap_or("")) as usize;
                let fut = self.db.update(q);
                let h_srv = self.hdr_srv.clone();
                let h_ct = self.hdr_ctjson.clone();

                Box::new(fut.from_err().map(move |worlds| {
                    let mut body = BytesMut::with_capacity(35 * worlds.len());
                    to_writer(Writer(&mut body), &worlds).unwrap();
                    let mut res =
                        Response::with_body(StatusCode::OK, Body::Bytes(body.freeze()));
                    let hdrs = res.headers_mut();
                    hdrs.insert(SERVER, h_srv);
                    hdrs.insert(CONTENT_TYPE, h_ct);
                    res
                }))
            }
            _ => Box::new(ok(Response::new(http::StatusCode::NOT_FOUND))),
        }
    }
}

#[derive(Clone)]
struct AppFactory;

impl NewService for AppFactory {
    type Config = ServerConfig;
    type Request = Request;
    type Response = Response;
    type Error = Error;
    type Service = App;
    type InitError = ();
    type Future = Box<Future<Item = Self::Service, Error = Self::InitError>>;

    fn new_service(&self, _: &ServerConfig) -> Self::Future {
        const DB_URL: &str =
            "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

        Box::new(PgConnection::connect(DB_URL).map(|db| App {
            db,
            hdr_srv: HeaderValue::from_static("Actix"),
            hdr_ctjson: HeaderValue::from_static("application/json"),
            hdr_cthtml: HeaderValue::from_static("text/html; charset=utf-8"),
        }))
    }
}

fn main() -> std::io::Result<()> {
    let sys = actix_rt::System::builder().stop_on_panic(false).build();

    // start http server
    Server::build()
        .backlog(1024)
        .bind("techempower", "0.0.0.0:8080", || {
            HttpService::build()
                .keep_alive(KeepAlive::Os)
                .h1(AppFactory)
        })?
        .start();

    println!("Started http server: 127.0.0.1:8080");
    sys.run()
}
