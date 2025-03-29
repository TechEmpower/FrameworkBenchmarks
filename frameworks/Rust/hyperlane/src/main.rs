pub(crate) mod constant;
pub(crate) mod db;
pub(crate) mod lazy;
pub(crate) mod request_middleware;
pub(crate) mod response_middleware;
pub(crate) mod route;
pub(crate) mod server;
pub(crate) mod r#type;
pub(crate) mod utils;

pub(crate) use constant::*;
pub(crate) use db::*;
pub(crate) use hyperlane::{
    futures::{executor::block_on, future::join_all},
    once_cell::sync::Lazy,
    serde::*,
    serde_json::{Value, json},
    tokio::{spawn, sync::Semaphore, task::JoinHandle},
    *,
};
pub(crate) use lazy::*;
pub(crate) use rand::{Rng, SeedableRng, rng, rngs::SmallRng};
pub(crate) use request_middleware::*;
pub(crate) use response_middleware::*;
pub(crate) use route::*;
pub(crate) use server::*;
pub(crate) use sqlx::{
    postgres::{PgPoolOptions, PgRow},
    *,
};
pub(crate) use std::{fmt, sync::Arc};
pub(crate) use r#type::*;
pub(crate) use utils::*;

#[tokio::main]
async fn main() {
    init_db().await;
    run_server().await;
}
