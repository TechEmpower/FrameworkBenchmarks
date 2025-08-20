#![allow(dead_code)]
#![allow(unused_imports)]
pub(crate) mod r#const;
pub(crate) mod db;
pub(crate) mod lazy;
pub(crate) mod request_middleware;
pub(crate) mod route;
pub(crate) mod server;
pub(crate) mod r#type;
pub(crate) mod utils;

pub(crate) use r#const::*;
pub(crate) use db::*;
pub(crate) use r#type::*;
pub(crate) use utils::*;

pub(crate) use std::{fmt, hint::black_box, sync::Arc};

pub(crate) use futures::{executor::block_on, future::join_all};
pub(crate) use hyperlane::{
    tokio::{
        runtime::{Builder, Runtime},
        spawn,
        sync::{AcquireError, OwnedSemaphorePermit, Semaphore},
        task::JoinHandle,
    },
    *,
};
pub(crate) use hyperlane_time::*;
pub(crate) use lazy::*;
pub(crate) use once_cell::sync::Lazy;
pub(crate) use rand::{Rng, SeedableRng, rng, rngs::SmallRng};
pub(crate) use serde::*;
pub(crate) use serde_json::{Value, json};
pub(crate) use server::*;
pub(crate) use sqlx::{
    postgres::{PgPoolOptions, PgRow},
    *,
};

fn main() {
    run_server();
}
