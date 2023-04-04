// #[global_allocator]
// static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::cmp;
use std::fmt::Write;
use std::thread::available_parallelism;

use async_trait::async_trait;
use bytes::Bytes;
use salvo::conn::tcp::TcpAcceptor;
use salvo::http::header::{self, HeaderValue};
use salvo::http::ResBody;
use salvo::prelude::*;
use dotenv::dotenv;
use salvo::routing::FlowCtrl;

mod db_pg;
mod models_pg;
mod utils;
use db_pg::PgConnection;

static SERVER_HEADER: HeaderValue = HeaderValue::from_static("salvo");
static JSON_HEADER: HeaderValue = HeaderValue::from_static("application/json");
static HTML_HEADER: HeaderValue = HeaderValue::from_static("text/html; charset=utf-8");

struct WorldHandler {
    conn: PgConnection,
}
impl WorldHandler {
    async fn new() -> Self {
        let db_url: String = utils::get_env_var("TECHEMPOWER_DATABASE_URL");
        Self {
            conn: PgConnection::create(&db_url)
                .await
                .unwrap_or_else(|_| panic!("Error connecting to {}", &db_url)),
        }
    }
}
#[async_trait]
impl Handler for WorldHandler {
    async fn handle(&self, _req: &mut Request, _depot: &mut Depot, res: &mut Response, _ctrl: &mut FlowCtrl) {
        let world = self.conn.get_world().await.unwrap();
        let data = serde_json::to_vec(&world).unwrap();
        let headers = res.headers_mut();
        headers.insert(header::SERVER, SERVER_HEADER.clone());
        headers.insert(header::CONTENT_TYPE, JSON_HEADER.clone());
        res.set_body(ResBody::Once(Bytes::from(data)));
    }
}
struct WorldsHandler {
    conn: PgConnection,
}
impl WorldsHandler {
    async fn new() -> Self {
        let db_url: String = utils::get_env_var("TECHEMPOWER_DATABASE_URL");
        Self {
            conn: PgConnection::create(&db_url)
                .await
                .unwrap_or_else(|_| panic!("Error connecting to {}", &db_url)),
        }
    }
}
#[async_trait]
impl Handler for WorldsHandler {
    async fn handle(&self, req: &mut Request, _depot: &mut Depot, res: &mut Response, _ctrl: &mut FlowCtrl) {
        let count = req.query::<u16>("q").unwrap_or(1);
        let count = cmp::min(500, cmp::max(1, count));
        let worlds = self.conn.get_worlds(count).await.unwrap();

        let data = serde_json::to_vec(&worlds).unwrap();
        let headers = res.headers_mut();
        headers.insert(header::SERVER, SERVER_HEADER.clone());
        headers.insert(header::CONTENT_TYPE, JSON_HEADER.clone());
        res.set_body(ResBody::Once(Bytes::from(data)));
    }
}
struct UpdatesHandler {
    conn: PgConnection,
}
impl UpdatesHandler {
    async fn new() -> Self {
        let db_url: String = utils::get_env_var("TECHEMPOWER_DATABASE_URL");
        Self {
            conn: PgConnection::create(&db_url)
                .await
                .unwrap_or_else(|_| panic!("Error connecting to {}", &db_url)),
        }
    }
}
#[async_trait]
impl Handler for UpdatesHandler {
    async fn handle(&self, req: &mut Request, _depot: &mut Depot, res: &mut Response, _ctrl: &mut FlowCtrl) {
        let count = req.query::<u16>("q").unwrap_or(1);
        let count = cmp::min(500, cmp::max(1, count));
        res.headers_mut().insert(header::SERVER, SERVER_HEADER.clone());
        let worlds = self.conn.update(count).await.unwrap();

        let data = serde_json::to_vec(&worlds).unwrap();
        let headers = res.headers_mut();
        headers.insert(header::SERVER, SERVER_HEADER.clone());
        headers.insert(header::CONTENT_TYPE, JSON_HEADER.clone());
        res.set_body(ResBody::Once(Bytes::from(data)));
    }
}
struct FortunesHandler {
    conn: PgConnection,
}
impl FortunesHandler {
    async fn new() -> Self {
        let db_url: String = utils::get_env_var("TECHEMPOWER_DATABASE_URL");
        Self {
            conn: PgConnection::create(&db_url)
                .await
                .unwrap_or_else(|_| panic!("Error connecting to {}", &db_url)),
        }
    }
}
#[async_trait]
impl Handler for FortunesHandler {
    async fn handle(&self, _req: &mut Request, _depot: &mut Depot, res: &mut Response, _ctrl: &mut FlowCtrl) {
        let mut data = String::new();
        write!(&mut data, "{}", self.conn.tell_fortune().await.unwrap()).unwrap();

        let headers = res.headers_mut();
        headers.insert(header::SERVER, SERVER_HEADER.clone());
        headers.insert(header::CONTENT_TYPE, HTML_HEADER.clone());
        res.set_body(ResBody::Once(Bytes::from(data)));
    }
}

fn main() {
    dotenv().ok();
    
    let thread_count = available_parallelism().map(|n| n.get()).unwrap_or(16);
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    for _ in 1..thread_count {
        std::thread::spawn(move || {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap();
            rt.block_on(serve());
        });
    }
    println!("Started http server: 127.0.0.1:8080");
    rt.block_on(serve());
}

async fn serve() {
    let router = Router::new()
        .push(Router::with_path("db").get(WorldHandler::new().await))
        .push(Router::with_path("fortunes").get(FortunesHandler::new().await))
        .push(Router::with_path("queries").get(WorldsHandler::new().await))
        .push(Router::with_path("updates").get(UpdatesHandler::new().await));
    let acceptor: TcpAcceptor = utils::reuse_listener().unwrap().try_into().unwrap();
    Server::new(acceptor).serve(router).await
}
