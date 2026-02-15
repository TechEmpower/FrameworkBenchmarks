mod config;
mod db;
mod middleware;
mod route;
mod server;
mod utils;

use {config::*, db::*, middleware::*, route::*, server::*, utils::*};

use std::fmt;

use {
    futures::{executor::block_on, future::join_all},
    hyperlane::{
        tokio::{spawn, task::JoinHandle},
        *,
    },
    hyperlane_time::*,
    once_cell::sync::Lazy,
    rand::{RngExt, SeedableRng, rng, rngs::SmallRng},
    serde::*,
    serde_json::{Value, json},
    sqlx::{
        Pool, Postgres, Row,
        postgres::{PgPoolOptions, PgRow},
        query as db_query,
    },
};

#[tokio::main]
async fn main() {
    init_db().await;
    let server_config: ServerConfig = init_server_config();
    let request_config: RequestConfig = init_request_config();
    let mut server: Server = Server::default();
    server
        .server_config(server_config)
        .request_config(request_config)
        .request_middleware::<RequestMiddleware>()
        .route::<PlaintextRoute>("/plaintext")
        .route::<JsonRoute>("/json")
        .route::<CachedQueryRoute>("/cached-quer")
        .route::<DbRoute>("/db")
        .route::<QueryRoute>("/query")
        .route::<FortunesRoute>("/fortunes")
        .route::<UpdateRoute>("/upda")
        .run()
        .await
        .unwrap()
        .wait()
        .await;
}
