use super::*;

pub static DB: Lazy<DbPoolConnection> = Lazy::new(|| block_on(async { connection_db().await }));
pub static CACHE: Lazy<Vec<QueryRow>> = Lazy::new(|| block_on(async { init_cache().await }));
