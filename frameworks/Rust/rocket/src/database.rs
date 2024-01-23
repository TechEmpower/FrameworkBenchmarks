use rocket_db_pools::{sqlx, Database};

#[derive(Database)]
#[database("hello_world")]
pub struct HelloWorld(sqlx::PgPool);
