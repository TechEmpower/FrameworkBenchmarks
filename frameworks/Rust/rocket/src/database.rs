use rocket_db_pools::{sqlx, Database};

#[derive(Database)]
#[database("hello_world")]
#[cfg(not(test))]
pub struct HelloWorld(sqlx::PgPool);

#[derive(Database)]
#[database("hello_world")]
#[cfg(test)]
pub struct HelloWorld(sqlx::SqlitePool);