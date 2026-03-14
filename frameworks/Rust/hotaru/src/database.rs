use crate::models::{Fortune, World};
use deadpool_postgres::{GenericClient, Manager, Pool};
use moka::sync::Cache;
use std::env;
use std::sync::Arc;
use tokio_postgres::NoTls;

pub const SQL_SELECT_WORLD: &str = "SELECT id, randomNumber FROM World WHERE id = $1";
pub const SQL_SELECT_FORTUNES: &str = "SELECT id, message FROM Fortune";
pub const SQL_UPDATE_WORLD: &str = "UPDATE World SET randomNumber = $1 WHERE id = $2";
pub const SQL_SELECT_CACHED_WORLD: &str = "SELECT id, randomNumber FROM World WHERE id = $1";
pub const SQL_SELECT_ALL_CACHED: &str = "SELECT id, randomNumber FROM World";

pub type DbPool = Pool;
pub type WorldCache = Arc<Cache<i32, Arc<World>>>;

pub fn create_pool() -> DbPool {
    let db_url = env::var("DATABASE_URL")
        .unwrap_or_else(|_| "postgres://benchmarkdbuser:benchmarkdbpass@localhost/hello_world".to_string());
    let pool_size = env::var("DB_POOL_SIZE")
        .ok()
        .and_then(|value| value.parse::<usize>().ok())
        .unwrap_or(56);

    let pg_config: tokio_postgres::Config = db_url
        .parse()
        .expect("Invalid DATABASE_URL");

    let manager = Manager::new(pg_config, NoTls);
    Pool::builder(manager)
        .max_size(pool_size)
        .build()
        .expect("Failed to build database pool")
}

pub fn create_cache() -> WorldCache {
    Arc::new(Cache::new(10_000))
}

pub async fn warm_cache(pool: &DbPool, cache: &WorldCache) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let client = pool.get().await?;
    let stmt = client.prepare_cached(SQL_SELECT_ALL_CACHED).await?;
    let rows = client.query(&stmt, &[]).await?;

    for row in rows {
        let world = World {
            id: row.get(0),
            random_number: row.get(1),
        };
        cache.insert(world.id, Arc::new(world));
    }

    Ok(())
}

pub async fn fetch_world_by_id<C>(client: &C, id: i32) -> Result<World, tokio_postgres::Error>
where
    C: GenericClient + Sync,
{
    let stmt = client.prepare_cached(SQL_SELECT_WORLD).await?;
    let row = client.query_one(&stmt, &[&id]).await?;
    Ok(World {
        id: row.get(0),
        random_number: row.get(1),
    })
}

pub async fn fetch_cached_world_by_id<C>(client: &C, id: i32) -> Result<World, tokio_postgres::Error>
where
    C: GenericClient + Sync,
{
    let stmt = client.prepare_cached(SQL_SELECT_CACHED_WORLD).await?;
    let row = client.query_one(&stmt, &[&id]).await?;
    Ok(World {
        id: row.get(0),
        random_number: row.get(1),
    })
}

pub async fn fetch_all_fortunes<C>(client: &C) -> Result<Vec<Fortune>, tokio_postgres::Error>
where
    C: GenericClient + Sync,
{
    let stmt = client.prepare_cached(SQL_SELECT_FORTUNES).await?;
    let rows = client.query(&stmt, &[]).await?;
    let mut fortunes = Vec::with_capacity(rows.len());
    for row in rows {
        fortunes.push(Fortune {
            id: row.get(0),
            message: row.get(1),
        });
    }
    Ok(fortunes)
}

pub async fn update_world<C>(client: &C, world: &World) -> Result<(), tokio_postgres::Error>
where
    C: GenericClient + Sync,
{
    let stmt = client.prepare_cached(SQL_UPDATE_WORLD).await?;
    client
        .execute(&stmt, &[&world.random_number, &world.id])
        .await?;
    Ok(())
}
