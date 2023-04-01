// #[global_allocator]
// static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::cmp;
use std::fmt::Write;
use std::net::{Ipv4Addr, SocketAddr};

use async_trait::async_trait;
use salvo::http::header::{self, HeaderValue};
use salvo::prelude::*;
use salvo::routing::FlowCtrl;

mod models;
mod pg_conn;
use pg_conn::PgConnection;

const DB_URL: &str = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

struct WorldHandler {
    conn: PgConnection,
}
impl WorldHandler {
    async fn new() -> Self {
        Self {
            conn: PgConnection::create(DB_URL)
                .await
                .unwrap_or_else(|_| panic!("Error connecting to {}", &DB_URL)),
        }
    }
}
#[async_trait]
impl Handler for WorldHandler {
    async fn handle(
        &self,
        _req: &mut Request,
        _depot: &mut Depot,
        res: &mut Response,
        _ctrl: &mut FlowCtrl,
    ) {
        res.headers_mut()
            .insert(header::SERVER, HeaderValue::from_static("S"));
        let world = self.conn.get_world().await.unwrap();
        res.render(Json(world));
    }
}
struct WorldsHandler {
    conn: PgConnection,
}
impl WorldsHandler {
    async fn new() -> Self {
        Self {
            conn: PgConnection::create(DB_URL)
                .await
                .unwrap_or_else(|_| panic!("Error connecting to {}", &DB_URL)),
        }
    }
}
#[async_trait]
impl Handler for WorldsHandler {
    async fn handle(
        &self,
        req: &mut Request,
        _depot: &mut Depot,
        res: &mut Response,
        _ctrl: &mut FlowCtrl,
    ) {
        let count = req.query::<u16>("q").unwrap_or(1);
        let count = cmp::min(500, cmp::max(1, count));
        res.headers_mut()
            .insert(header::SERVER, HeaderValue::from_static("S"));
        let worlds = self.conn.get_worlds(count).await.unwrap();
        res.render(Json(worlds));
    }
}
struct UpdatesHandler {
    conn: PgConnection,
}
impl UpdatesHandler {
    async fn new() -> Self {
        Self {
            conn: PgConnection::create(DB_URL)
                .await
                .unwrap_or_else(|_| panic!("Error connecting to {}", &DB_URL)),
        }
    }
}
#[async_trait]
impl Handler for UpdatesHandler {
    async fn handle(
        &self,
        req: &mut Request,
        _depot: &mut Depot,
        res: &mut Response,
        _ctrl: &mut FlowCtrl,
    ) {
        let count = req.query::<u16>("q").unwrap_or(1);
        let count = cmp::min(500, cmp::max(1, count));
        res.headers_mut()
            .insert(header::SERVER, HeaderValue::from_static("S"));
        let worlds = self.conn.update(count).await.unwrap();
        res.render(Json(worlds));
    }
}
struct FortunesHandler {
    conn: PgConnection,
}
impl FortunesHandler {
    async fn new() -> Self {
        Self {
            conn: PgConnection::create(DB_URL)
                .await
                .unwrap_or_else(|_| panic!("Error connecting to {}", &DB_URL)),
        }
    }
}
#[async_trait]
impl Handler for FortunesHandler {
    async fn handle(
        &self,
        _req: &mut Request,
        _depot: &mut Depot,
        res: &mut Response,
        _ctrl: &mut FlowCtrl,
    ) {
        let mut body = String::new();
        write!(&mut body, "{}", self.conn.tell_fortune().await.unwrap()).unwrap();
        res.headers_mut()
            .insert(header::SERVER, HeaderValue::from_static("S"));
        res.render(Text::Html(body));
    }
}

#[tokio::main]
async fn main() {
    let router = Router::new()
        .push(Router::with_path("db").get(WorldHandler::new().await))
        .push(Router::with_path("fortunes").get(FortunesHandler::new().await))
        .push(Router::with_path("queries").get(WorldsHandler::new().await))
        .push(Router::with_path("updates").get(UpdatesHandler::new().await));

    let addr = SocketAddr::from((Ipv4Addr::UNSPECIFIED, 8080));
    let acceptor = TcpListener::new(addr).bind().await;
    Server::new(acceptor).serve(router).await;
}
