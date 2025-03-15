pub(crate) mod constant;
pub(crate) mod db;
pub(crate) mod lazy;
pub(crate) mod request_middleware;
pub(crate) mod response_middleware;
pub(crate) mod route;
pub(crate) mod server;
pub(crate) mod r#type;
pub(crate) mod utils;

pub(crate) use bb8::{Pool, PooledConnection};
pub(crate) use bb8_postgres::PostgresConnectionManager;
pub(crate) use chrono::{DateTime, Utc};
pub(crate) use constant::*;
pub(crate) use db::*;
pub(crate) use html_escape::encode_text;
pub(crate) use hyperlane::{
    once_cell::sync::Lazy,
    serde::*,
    serde_json::json,
    tokio::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard},
    *,
};
pub(crate) use lazy::*;
pub(crate) use r#type::*;
pub(crate) use rand::Rng;
pub(crate) use request_middleware::*;
pub(crate) use response_middleware::*;
pub(crate) use route::*;
pub(crate) use server::*;
pub(crate) use std::{
    borrow::Cow,
    collections::HashMap,
    error::Error,
    fmt::{self, Write},
    io,
    sync::Arc,
    time::Duration,
    time::SystemTime,
};
pub(crate) use tokio_postgres::{Config, NoTls, Row, Statement};
pub(crate) use utils::*;

#[tokio::main]
async fn main() {
    init_db().await;
    run_server().await;
}
