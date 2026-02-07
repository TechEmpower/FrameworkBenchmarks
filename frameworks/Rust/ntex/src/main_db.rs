#[cfg(not(target_os = "macos"))]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use ntex::http::header::{CONTENT_TYPE, SERVER};
use ntex::http::{HttpService, Request, Response, StatusCode};
use ntex::service::{cfg::SharedCfg, Service, ServiceCtx, ServiceFactory};
use ntex::{web::Error, web::HttpResponse};

mod db;
mod utils;

struct App(db::PgConnection);

impl Service<Request> for App {
    type Response = Response;
    type Error = Error;

    async fn call(&self, req: Request, _: ServiceCtx<'_, Self>) -> Result<Response, Error> {
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
    }
}

struct AppFactory;

impl ServiceFactory<Request, SharedCfg> for AppFactory {
    type Response = Response;
    type Error = Error;
    type Service = App;
    type InitError = ();

    async fn create(&self, _: SharedCfg) -> Result<Self::Service, Self::InitError> {
        const DB_URL: &str =
            "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

        Ok(App(db::PgConnection::connect(DB_URL).await))
    }
}

#[ntex::main]
async fn main() -> std::io::Result<()> {
    println!("Starting http server: 127.0.0.1:8080");

    ntex::server::build()
        .backlog(1024)
        .enable_affinity()
        .bind("tfb", "0.0.0.0:8080", async |_| HttpService::h1(AppFactory))?
        .config("tfb", utils::config())
        .run()
        .await
}
