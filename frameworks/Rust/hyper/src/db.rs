use std::sync::LazyLock;

use deadpool_postgres::{Config, CreatePoolError, Pool};
use tokio_postgres::NoTls;
use tracing::info;

pub static POOL: LazyLock<Pool> =
    LazyLock::new(|| connect().expect("failed to create database connection pool"));

fn connect() -> Result<Pool, CreatePoolError> {
    info!("Creating database connection pool");
    let config = Config {
        host: Some("tfb-database".to_string()),
        port: Some(5432),
        dbname: Some("hello_world".to_string()),
        user: Some("benchmarkdbuser".to_string()),
        password: Some("benchmarkdbpass".to_string()),
        ..Default::default()
    };
    config.create_pool(None, NoTls)
}
