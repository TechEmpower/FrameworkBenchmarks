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
pub(crate) use html_escape::decode_html_entities;
pub(crate) use hyperlane::{
    once_cell::sync::Lazy,
    serde::*,
    serde_json::json,
    tokio::sync::{RwLock, RwLockWriteGuard},
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
    error::Error,
    fmt::{self, Write},
    io,
    sync::Arc,
    time::SystemTime,
};
pub(crate) use tokio_postgres::{types::ToSql, Config, NoTls, Row, Statement};
pub(crate) use utils::*;

#[tokio::main]
async fn main() {
    println_warning!("start connect db");
    init_db().await;
    println_success!("connect db finish");
    println_warning!("start init server");
    run_server().await;
}
