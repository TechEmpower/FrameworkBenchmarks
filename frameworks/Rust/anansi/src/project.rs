use anansi::project::prelude::*;
use crate::app_migrations;

#[cfg(feature = "raw")]
use super::hello::middleware::Stmt;

#[cfg(feature = "raw")]
use crate::impl_pg;

#[cfg(feature = "raw")]
app_cache!(local);

#[cfg(not(feature = "raw"))]
app_cache!(redis);

database!(postgres);

#[cfg(feature = "raw")]
raw_middleware!();

#[cfg(feature = "raw")]
anansi::setup!();

#[cfg(feature = "raw")]
impl_pg!();

#[cfg(not(feature = "raw"))]
middleware!();

#[derive(Clone, Debug)]
pub struct AppData {
    pub pool: Pool,
    #[cfg(feature = "raw")]
    pub stmt: Stmt,
}

#[cfg(feature = "raw")]
impl AppData {
    pub async fn new() -> Self {
        let pool = anansi::server::get_db::<AppData>(app_migrations).await;
        let stmt = Stmt::new(&pool).await.unwrap();
        Self {pool, stmt}
    }
}

#[cfg(not(feature = "raw"))]
impl AppData {
    pub async fn new() -> Self {
        let pool = anansi::server::get_db::<AppData>(app_migrations).await;
        Self {pool}
    }
}

impl anansi::db::AsDb for AppData {
    type SqlDb = Pool;
    fn as_db(&self) -> &Pool {
        &self.pool
    }
    fn as_db_mut(&mut self) -> &mut Pool {
        &mut self.pool
    }
}
