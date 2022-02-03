#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};

use actix_http::body::Body;
use actix_http::http::header::{CONTENT_TYPE, SERVER};
use actix_http::http::{HeaderValue, StatusCode};
use actix_http::{Error, HttpService, KeepAlive, Request, Response};
use actix_server::Server;
use actix_service::{Service, ServiceFactory};
use bytes::{Bytes, BytesMut};
use futures::future::{ok, LocalBoxFuture};
use serde_json::to_writer;
use yarte::ywrite_html;

mod db_pg_direct;
mod models;
mod utils;

use crate::db_pg_direct::PgConnection;
use crate::utils::Writer;

struct App {
    db: PgConnection,
    hdr_srv: HeaderValue,
    hdr_ctjson: HeaderValue,
    hdr_cthtml: HeaderValue,
}

impl Service for App {
    type Request = Request;
    type Response = Response;
    type Error = Error;
    type Future = Pin<Box<dyn Future<Output = Result<Response, Error>>>>;

    #[inline]
    fn poll_ready(&mut self, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn call(&mut self, req: Request) -> Self::Future {
        let path = req.path();
        match path {
            "/db" => {
                let h_srv = self.hdr_srv.clone();
                let h_ct = self.hdr_ctjson.clone();
                let fut = self.db.get_world();

                Box::pin(async move {
                    let body = fut.await?;
                    let mut res = Response::with_body(StatusCode::OK, Body::Bytes(body));
                    let hdrs = res.headers_mut();
                    hdrs.insert(SERVER, h_srv);
                    hdrs.insert(CONTENT_TYPE, h_ct);
                    Ok(res)
                })
            }
            "/fortunes" => {
                let h_srv = self.hdr_srv.clone();
                let h_ct = self.hdr_cthtml.clone();
                let fut = self.db.tell_fortune();

                Box::pin(async move {
                    let fortunes = fut.await?;

                    let mut body = Vec::with_capacity(2048);
                    ywrite_html!(body, "{{> fortune }}");

                    let mut res = Response::with_body(
                        StatusCode::OK,
                        Body::Bytes(Bytes::from(body)),
                    );
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
                let fut = self.db.get_worlds(q);

                Box::pin(async move {
                    let worlds = fut.await?;
                    let mut body = BytesMut::with_capacity(35 * worlds.len());
                    to_writer(Writer(&mut body), &worlds).unwrap();
                    let mut res =
                        Response::with_body(StatusCode::OK, Body::Bytes(body.freeze()));
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
                let fut = self.db.update(q);

                Box::pin(async move {
                    let worlds = fut.await?;
                    let mut body = BytesMut::with_capacity(35 * worlds.len());
                    to_writer(Writer(&mut body), &worlds).unwrap();
                    let mut res =
                        Response::with_body(StatusCode::OK, Body::Bytes(body.freeze()));
                    let hdrs = res.headers_mut();
                    hdrs.insert(SERVER, h_srv);
                    hdrs.insert(CONTENT_TYPE, h_ct);
                    Ok(res)
                })
            }
            _ => Box::pin(ok(Response::new(http::StatusCode::NOT_FOUND))),
        }
    }
}

#[derive(Clone)]
struct AppFactory;

impl ServiceFactory for AppFactory {
    type Config = ();
    type Request = Request;
    type Response = Response;
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
    let sys = actix_rt::System::builder().stop_on_panic(false).build();

    Server::build()
        .backlog(1024)
        .bind("techempower", "0.0.0.0:8080", || {
            HttpService::build()
                .keep_alive(KeepAlive::Os)
                .client_timeout(0)
                .h1(AppFactory)
                .tcp()
        })?
        .start();

    println!("Started http server: 127.0.0.1:8080");
    sys.run()
}
