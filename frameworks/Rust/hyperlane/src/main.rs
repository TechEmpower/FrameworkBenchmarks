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
    let server_config: ServerConfig = init_server_config().await;
    let request_config: RequestConfig = init_request_config().await;
    Server::new()
        .await
        .server_config(server_config)
        .await
        .request_config(request_config)
        .await
        .request_middleware::<RequestMiddleware>()
        .await
        .route::<PlaintextRoute>("/plaintext")
        .await
        .route::<JsonRoute>("/json")
        .await
        .route::<CachedQueryRoute>("/cached-quer")
        .await
        .route::<DbRoute>("/db")
        .await
        .route::<QueryRoute>("/query")
        .await
        .route::<FortunesRoute>("/fortunes")
        .await
        .route::<UpdateRoute>("/upda")
        .await
        .run()
        .await
        .unwrap()
        .wait()
        .await;
}
