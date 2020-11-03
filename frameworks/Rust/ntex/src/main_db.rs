#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};

use bytes::BytesMut;
use futures::future::ok;
use ntex::http::body::Body;
use ntex::http::header::{HeaderValue, CONTENT_TYPE, SERVER};
use ntex::http::{HttpService, KeepAlive, Request, Response, StatusCode};
use ntex::service::{Service, ServiceFactory};
use ntex::web::Error;
use yarte::Serialize;

mod db;
mod utils;

use crate::db::PgConnection;

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
    fn poll_ready(&self, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn call(&self, req: Request) -> Self::Future {
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
                    let body = fut.await?;
                    let mut res = Response::with_body(StatusCode::OK, Body::Bytes(body));
                    let hdrs = res.headers_mut();
                    hdrs.insert(SERVER, h_srv);
                    hdrs.insert(CONTENT_TYPE, h_ct);
                    Ok(res)
                })
            }
            "/query" => {
                let q = utils::get_query_param(req.uri().query().unwrap_or("")) as usize;
                let h_srv = self.hdr_srv.clone();
                let h_ct = self.hdr_ctjson.clone();
                let fut = self.db.get_worlds(q);

                Box::pin(async move {
                    let worlds = fut.await?;
                    let size = 35 * worlds.len();
                    let mut res = Response::with_body(
                        StatusCode::OK,
                        Body::Bytes(worlds.to_bytes::<BytesMut>(size)),
                    );
                    let hdrs = res.headers_mut();
                    hdrs.insert(SERVER, h_srv);
                    hdrs.insert(CONTENT_TYPE, h_ct);
                    Ok(res)
                })
            }
            "/update" => {
                let q = utils::get_query_param(req.uri().query().unwrap_or(""));
                let h_srv = self.hdr_srv.clone();
                let h_ct = self.hdr_ctjson.clone();
                let fut = self.db.update(q);

                Box::pin(async move {
                    let worlds = fut.await?;
                    let size = 35 * worlds.len();
                    let mut res = Response::with_body(
                        StatusCode::OK,
                        Body::Bytes(worlds.to_bytes::<BytesMut>(size)),
                    );
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
    type Future = Pin<Box<dyn Future<Output = Result<Self::Service, Self::InitError>>>>;

    fn new_service(&self, _: ()) -> Self::Future {
        const DB_URL: &str =
            "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

        Box::pin(async move {
            let db = PgConnection::connect(DB_URL).await;
            Ok(App {
                db,
                hdr_srv: HeaderValue::from_static("N"),
                hdr_ctjson: HeaderValue::from_static("application/json"),
                hdr_cthtml: HeaderValue::from_static("text/html; charset=utf-8"),
            })
        })
    }
}

#[ntex::main]
async fn main() -> std::io::Result<()> {
    println!("Starting http server: 127.0.0.1:8080");

    ntex::server::build()
        .backlog(1024)
        .bind("techempower", "0.0.0.0:8080", || {
            HttpService::build()
                .keep_alive(KeepAlive::Os)
                .client_timeout(0)
                .h1(AppFactory)
                .tcp()
        })?
        .start()
        .await
}
