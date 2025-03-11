use crate::*;

pub type DbPoolConnection = bb8::Pool<PostgresConnectionManager<NoTls>>;
pub type DbConnection<'a> = PooledConnection<'a, PostgresConnectionManager<NoTls>>;
pub type QueryRow = (i32, i32);
pub type Queries = usize;
