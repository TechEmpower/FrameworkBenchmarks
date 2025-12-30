pub(crate) mod db;
pub(crate) mod middleware;
pub(crate) mod route;
pub(crate) mod server;
pub(crate) mod utils;

pub(crate) use db::*;
pub(crate) use server::*;
pub(crate) use utils::*;

pub(crate) use std::fmt;

pub(crate) use futures::{executor::block_on, future::join_all};
pub(crate) use hyperlane::{
    tokio::{spawn, task::JoinHandle},
    *,
};
pub(crate) use hyperlane_time::*;
pub(crate) use once_cell::sync::Lazy;
pub(crate) use rand::{Rng, SeedableRng, rng, rngs::SmallRng};
pub(crate) use serde::*;
pub(crate) use serde_json::{Value, json};
pub(crate) use sqlx::{
    Pool, Postgres, Row,
    postgres::{PgPoolOptions, PgRow},
    query as db_query,
};

use middleware::*;
use route::*;

#[tokio::main]
async fn main() {
    init_db().await;

    let config: ServerConfig = ServerConfig::new().await;
    config.port(8080).await;
    config.disable_nodelay().await;

    let server: Server = Server::from(config).await;
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
