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
    tokio::{
        runtime::{Builder, Runtime},
        spawn,
        task::JoinHandle,
    },
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

fn main() {
    run_server();
}
