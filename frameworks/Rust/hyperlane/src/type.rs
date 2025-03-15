use crate::*;

pub type DbPoolConnection = bb8::Pool<PostgresConnectionManager<NoTls>>;
pub type DbConnection<'a> = PooledConnection<'a, PostgresConnectionManager<NoTls>>;
pub type Queries = usize;
pub type DynToSqlSync = dyn ToSql + Sync;

#[allow(bad_style)]
#[derive(Serialize)]
pub struct QueryRow {
    id: i32,
    randomNumber: i32,
}

impl QueryRow {
    pub fn new(id: i32, random_number: i32) -> Self {
        Self {
            id,
            randomNumber: random_number,
        }
    }
}

#[derive(Serialize)]
pub struct Fortunes {
    id: i32,
    message: String,
}

impl Fortunes {
    pub fn new(id: i32, message: String) -> Self {
        Self { id, message }
    }
}
