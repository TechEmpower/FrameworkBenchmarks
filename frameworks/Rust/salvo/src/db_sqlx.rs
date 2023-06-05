use sqlx::{
    pool::PoolConnection,
    postgres::{PgArguments, PgPoolOptions},
    Arguments, PgPool, Postgres, Error
};

use crate::{Fortune, World};

pub async fn create_pool(database_url: String, max_pool_size: u32, min_pool_size: u32) -> PgPool {
    PgPoolOptions::new()
        .max_connections(max_pool_size)
        .min_connections(min_pool_size)
        .connect(&database_url)
        .await
        .unwrap()
}

pub async fn fetch_world(mut conn: PoolConnection<Postgres>, number: i32) -> Result<World, Error> {
    let mut args = PgArguments::default();
    args.add(number);

    let world: World = sqlx::query_as_with("SELECT id, randomnumber FROM World WHERE id = $1", args)
        .fetch_one(&mut conn)
        .await
        .expect("error loading world");
    Ok(world)
}

pub async fn fetch_fortunes(mut conn: PoolConnection<Postgres>) -> Result<Vec<Fortune>, Error> {
    let fortunes: Vec<Fortune> = sqlx::query_as("SELECT * FROM Fortune")
        .fetch_all(&mut conn)
        .await
        .expect("error loading Fortunes");
    Ok(fortunes)
}
