use crate::*;

pub type DbPoolConnection = bb8::Pool<PostgresConnectionManager<NoTls>>;
pub type DbConnection<'a> = PooledConnection<'a, PostgresConnectionManager<NoTls>>;
pub type Queries = usize;

#[allow(bad_style)]
#[derive(Serialize)]
pub struct QueryRow {
    id: i32,
    randomNumber: i32,
}

impl QueryRow {
    pub fn new(id: i32, random_number: i32) -> Self {
        Self {
            id: id,
            randomNumber: random_number,
        }
    }
}
