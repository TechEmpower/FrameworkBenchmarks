use rocket_sync_db_pools::database;

#[database("hello_world")]
pub struct Db(diesel::PgConnection);
