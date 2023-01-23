#[cfg(not(target_os = "macos"))]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::{future::Future, pin::Pin, rc::Rc, task::Context, task::Poll};

use ntex::http::header::{CONTENT_TYPE, SERVER};
use ntex::http::{HttpService, KeepAlive, Request, Response, StatusCode};
use ntex::service::{Service, ServiceFactory};
use ntex::web::{Error, HttpResponse};
use ntex::{time::Seconds, util::PoolId};

mod db;
mod utils;

struct App(Rc<db::PgConnection>);

impl Service<Request> for App {
    type Response = Response;
    type Error = Error;
    type Future = Pin<Box<dyn Future<Output = Result<Response, Error>>>>;

    #[inline]
    fn poll_ready(&self, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn call(&self, req: Request) -> Self::Future {
        let db = self.0.clone();

        Box::pin(async move {
            match req.path() {
                "/db" => {
                    let body = db.get_world().await;
                    let mut res = HttpResponse::with_body(StatusCode::OK, body.into());
                    res.headers_mut().insert(SERVER, utils::HDR_SERVER);
                    res.headers_mut()
                        .insert(CONTENT_TYPE, utils::HDR_JSON_CONTENT_TYPE);
                    Ok(res)
                }
                "/fortunes" => {
                    let body = db.tell_fortune().await;
                    let mut res = HttpResponse::with_body(StatusCode::OK, body.into());
                    res.headers_mut().insert(SERVER, utils::HDR_SERVER);
                    res.headers_mut()
                        .insert(CONTENT_TYPE, utils::HDR_HTML_CONTENT_TYPE);
                    Ok(res)
                }
                "/query" => {
                    let worlds = db
                        .get_worlds(utils::get_query_param(req.uri().query()))
                        .await;
                    let mut res = HttpResponse::with_body(StatusCode::OK, worlds.into());
                    res.headers_mut().insert(SERVER, utils::HDR_SERVER);
                    res.headers_mut()
                        .insert(CONTENT_TYPE, utils::HDR_JSON_CONTENT_TYPE);
                    Ok(res)
                }
                "/update" => {
                    let worlds = db.update(utils::get_query_param(req.uri().query())).await;
                    let mut res = HttpResponse::with_body(StatusCode::OK, worlds.into());
                    res.headers_mut().insert(SERVER, utils::HDR_SERVER);
                    res.headers_mut()
                        .insert(CONTENT_TYPE, utils::HDR_JSON_CONTENT_TYPE);
                    Ok(res)
                }
                _ => Ok(Response::new(StatusCode::NOT_FOUND)),
            }
        })
    }
}

struct AppFactory;

impl ServiceFactory<Request> for AppFactory {
    type Response = Response;
    type Error = Error;
    type Service = App;
    type InitError = ();
    type Future = Pin<Box<dyn Future<Output = Result<Self::Service, Self::InitError>>>>;

    fn new_service(&self, _: ()) -> Self::Future {
        const DB_URL: &str =
            "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

        Box::pin(async move { Ok(App(Rc::new(db::PgConnection::connect(DB_URL).await))) })
    }
}

#[ntex::main]
async fn main() -> std::io::Result<()> {
    println!("Starting http server: 127.0.0.1:8080");

    ntex::server::build()
        .backlog(1024)
        .bind("techempower", "0.0.0.0:8080", |cfg| {
            cfg.memory_pool(PoolId::P1);
            PoolId::P1.set_read_params(65535, 2048);
            PoolId::P1.set_write_params(65535, 2048);

            HttpService::build()
                .keep_alive(KeepAlive::Os)
                .client_timeout(Seconds(0))
                .h1(AppFactory)
        })?
        .run()
        .await
}
