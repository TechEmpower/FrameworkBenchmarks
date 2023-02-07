#[cfg(not(target_os = "macos"))]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::sync::{Arc, Mutex};

use ntex::http::header::{CONTENT_TYPE, SERVER};
use ntex::http::{HttpService, KeepAlive, Request, Response, StatusCode};
use ntex::service::{Service, ServiceFactory};
use ntex::web::{Error, HttpResponse};
use ntex::{time::Seconds, util::BoxFuture, util::PoolId};

mod db;
mod utils;

struct App(db::PgConnection);

impl Service<Request> for App {
    type Response = Response;
    type Error = Error;
    type Future<'f> = BoxFuture<'f, Result<Response, Error>> where Self: 'f;

    fn call(&self, req: Request) -> Self::Future<'_> {
        Box::pin(async move {
            match req.path() {
                "/db" => {
                    let body = self.0.get_world().await;
                    let mut res = HttpResponse::with_body(StatusCode::OK, body.into());
                    res.headers_mut().insert(SERVER, utils::HDR_SERVER);
                    res.headers_mut()
                        .insert(CONTENT_TYPE, utils::HDR_JSON_CONTENT_TYPE);
                    Ok(res)
                }
                "/fortunes" => {
                    let body = self.0.tell_fortune().await;
                    let mut res = HttpResponse::with_body(StatusCode::OK, body.into());
                    res.headers_mut().insert(SERVER, utils::HDR_SERVER);
                    res.headers_mut()
                        .insert(CONTENT_TYPE, utils::HDR_HTML_CONTENT_TYPE);
                    Ok(res)
                }
                "/query" => {
                    let worlds = self
                        .0
                        .get_worlds(utils::get_query_param(req.uri().query()))
                        .await;
                    let mut res = HttpResponse::with_body(StatusCode::OK, worlds.into());
                    res.headers_mut().insert(SERVER, utils::HDR_SERVER);
                    res.headers_mut()
                        .insert(CONTENT_TYPE, utils::HDR_JSON_CONTENT_TYPE);
                    Ok(res)
                }
                "/update" => {
                    let worlds = self
                        .0
                        .update(utils::get_query_param(req.uri().query()))
                        .await;
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
    type Future<'f> = BoxFuture<'f, Result<Self::Service, Self::InitError>>;

    fn create(&self, _: ()) -> Self::Future<'_> {
        const DB_URL: &str =
            "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

        Box::pin(async move { Ok(App(db::PgConnection::connect(DB_URL).await)) })
    }
}

#[ntex::main]
async fn main() -> std::io::Result<()> {
    println!("Starting http server: 127.0.0.1:8080");

    let cores = core_affinity::get_core_ids().unwrap();
    let total_cores = cores.len();
    let cores = Arc::new(Mutex::new(cores));

    ntex::server::build()
        .backlog(1024)
        .configure(move |cfg| {
            let cores = cores.clone();
            cfg.on_worker_start(move |_| {
                if let Some(core) = cores.lock().unwrap().pop() {
                    // Pin this worker to a single CPU core.
                    core_affinity::set_for_current(core);
                }
                std::future::ready(Ok::<_, &'static str>(()))
            })
        })?
        .bind("techempower", "0.0.0.0:8080", |cfg| {
            cfg.memory_pool(PoolId::P1);
            PoolId::P1.set_read_params(65535, 2048);
            PoolId::P1.set_write_params(65535, 2048);

            HttpService::build()
                .keep_alive(KeepAlive::Os)
                .client_timeout(Seconds(0))
                .h1(AppFactory)
        })?
        .workers(total_cores)
        .run()
        .await
}
