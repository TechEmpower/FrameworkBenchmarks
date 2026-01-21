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
    rand::{Rng, SeedableRng, rng, rngs::SmallRng},
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

    let server_config: ServerConfig = init_server_config().await;
    let request_config: RequestConfig = init_request_config().await;

    let server: Server = Server::new().await;
    server.server_config(server_config).await;
    server.request_config(request_config).await;
    server.request_middleware::<RequestMiddleware>().await;
    server.route::<PlaintextRoute>("/plaintext").await;
    server.route::<JsonRoute>("/json").await;
    server.route::<CachedQueryRoute>("/cached-quer").await;
    server.route::<DbRoute>("/db").await;
    server.route::<QueryRoute>("/query").await;
    server.route::<FortunesRoute>("/fortunes").await;
    server.route::<UpdateRoute>("/upda").await;

    let server_hook: ServerControlHook = server.run().await.unwrap();
    server_hook.wait().await;
}
