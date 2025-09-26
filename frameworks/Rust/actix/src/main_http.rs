#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

use std::{io, rc::Rc, time::Duration};

use actix_http::{
    body::BoxBody,
    header::{HeaderValue, CONTENT_TYPE, SERVER},
    HttpService, KeepAlive, Request, Response, StatusCode,
};
use actix_server::Server;
use actix_service::{Service, ServiceFactory};
use bytes::{Bytes, BytesMut};
use futures::future::{ok, LocalBoxFuture};
use yarte::ywrite_html;

mod db;
mod models;
mod utils;

use crate::{
    db::{PgConnection, PgError},
    utils::Writer,
};

#[derive(Debug)]
enum Error {
    Pg(PgError),
    Io(io::Error),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::Io(err)
    }
}

impl From<PgError> for Error {
    fn from(err: PgError) -> Self {
        Error::Pg(err)
    }
}

impl From<Error> for Response<BoxBody> {
    fn from(_err: Error) -> Self {
        Response::internal_server_error()
    }
}

struct App {
    db: Rc<PgConnection>,
    hdr_srv: HeaderValue,
    hdr_ctjson: HeaderValue,
    hdr_cthtml: HeaderValue,
}

impl Service<Request> for App {
    type Response = Response<Bytes>;
    type Error = Error;
    type Future = LocalBoxFuture<'static, Result<Self::Response, Self::Error>>;

    actix_service::always_ready!();

    fn call(&self, req: Request) -> Self::Future {
        match req.path() {
            "/db" => {
                let h_srv = self.hdr_srv.clone();
                let h_ct = self.hdr_ctjson.clone();
                let db = Rc::clone(&self.db);

                Box::pin(async move {
                    let body = db.get_world().await?;
                    let mut res = Response::with_body(StatusCode::OK, body);
                    let hdrs = res.headers_mut();
                    hdrs.insert(SERVER, h_srv);
                    hdrs.insert(CONTENT_TYPE, h_ct);
                    Ok(res)
                })
            }

            "/fortunes" => {
                let h_srv = self.hdr_srv.clone();
                let h_ct = self.hdr_cthtml.clone();
                let db = Rc::clone(&self.db);

                Box::pin(async move {
                    let fortunes = db.tell_fortune().await?;

                    let mut body = Vec::with_capacity(2048);
                    ywrite_html!(body, "{{> fortune }}");

                    let mut res = Response::with_body(StatusCode::OK, Bytes::from(body));
                    let hdrs = res.headers_mut();
                    hdrs.insert(SERVER, h_srv);
                    hdrs.insert(CONTENT_TYPE, h_ct);
                    Ok(res)
                })
            }

            "/queries" => {
                let q = utils::get_query_param(req.uri().query().unwrap_or("")) as usize;
                let h_srv = self.hdr_srv.clone();
                let h_ct = self.hdr_ctjson.clone();
                let db = Rc::clone(&self.db);

                Box::pin(async move {
                    let worlds = db.get_worlds(q).await?;
                    let mut body = BytesMut::with_capacity(35 * worlds.len());
                    serde_json::to_writer(Writer(&mut body), &worlds).unwrap();
                    let mut res = Response::with_body(StatusCode::OK, body.freeze());
                    let hdrs = res.headers_mut();
                    hdrs.insert(SERVER, h_srv);
                    hdrs.insert(CONTENT_TYPE, h_ct);
                    Ok(res)
                })
            }

            "/updates" => {
                let q = utils::get_query_param(req.uri().query().unwrap_or(""));
                let h_srv = self.hdr_srv.clone();
                let h_ct = self.hdr_ctjson.clone();
                let db = Rc::clone(&self.db);

                Box::pin(async move {
                    let worlds = db.update(q).await?;
                    let mut body = BytesMut::with_capacity(35 * worlds.len());
                    serde_json::to_writer(Writer(&mut body), &worlds).unwrap();
                    let mut res = Response::with_body(StatusCode::OK, body.freeze());
                    let hdrs = res.headers_mut();
                    hdrs.insert(SERVER, h_srv);
                    hdrs.insert(CONTENT_TYPE, h_ct);
                    Ok(res)
                })
            }

            _ => Box::pin(ok(Response::with_body(
                http::StatusCode::NOT_FOUND,
                Bytes::new(),
            ))),
        }
    }
}

#[derive(Clone)]
struct AppFactory;

impl ServiceFactory<Request> for AppFactory {
    type Config = ();
    type Response = Response<Bytes>;
    type Error = Error;
    type Service = App;
    type InitError = ();
    type Future = LocalBoxFuture<'static, Result<Self::Service, Self::InitError>>;

    fn new_service(&self, _: ()) -> Self::Future {
        const DB_URL: &str =
            "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

        Box::pin(async move {
            let db = PgConnection::connect(DB_URL).await;
            Ok(App {
                db,
                hdr_srv: HeaderValue::from_static("Actix"),
                hdr_ctjson: HeaderValue::from_static("application/json"),
                hdr_cthtml: HeaderValue::from_static("text/html; charset=utf-8"),
            })
        })
    }
}

fn main() -> std::io::Result<()> {
    println!("Starting HTTP server on http://127.0.0.1:8080");

    actix_rt::System::new().block_on(
        Server::build()
            .backlog(1024)
            .bind("tfb-actix-http", "0.0.0.0:8080", || {
                HttpService::build()
                    .keep_alive(KeepAlive::Os)
                    .client_request_timeout(Duration::ZERO)
                    .h1(AppFactory)
                    .tcp()
            })?
            .run(),
    )
}
