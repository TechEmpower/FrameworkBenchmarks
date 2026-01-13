pub(crate) mod db;
pub(crate) mod middleware;
pub(crate) mod route;
pub(crate) mod server;
pub(crate) mod utils;

pub(crate) use db::*;
pub(crate) use server::*;
pub(crate) use utils::*;

pub(crate) use std::fmt;
use std::{u64, usize};

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

    let mut request_config: RequestConfig = RequestConfig::default();
    request_config
        .set_buffer_size(KB_4)
        .set_http_read_timeout_ms(u64::MAX)
        .set_max_body_size(usize::MAX)
        .set_max_header_count(usize::MAX)
        .set_max_header_key_length(usize::MAX)
        .set_max_header_line_length(usize::MAX)
        .set_max_header_value_length(usize::MAX)
        .set_max_path_length(usize::MAX)
        .set_max_query_length(usize::MAX)
        .set_max_request_line_length(usize::MAX)
        .set_max_ws_frame_size(usize::MAX)
        .set_max_ws_frames(usize::MAX)
        .set_ws_read_timeout_ms(u64::MAX);

    let config: ServerConfig = ServerConfig::new().await;
    config.request_config(request_config).await;
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
