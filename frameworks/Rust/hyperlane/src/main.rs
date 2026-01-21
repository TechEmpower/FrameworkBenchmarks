mod config;
mod db;
mod middleware;
mod route;
mod server;
mod utils;

pub(crate) use {config::*, db::*, server::*, utils::*};

pub(crate) use std::fmt;

pub(crate) use {
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

use {middleware::*, route::*};

use std::{u64, usize};

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
