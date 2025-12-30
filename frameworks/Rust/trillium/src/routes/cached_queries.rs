use crate::db::{
    cached_world::{Entity as CachedWorlds, Model as CachedWorld},
    Db, DbConnExt,
};
use futures_lite::StreamExt;
use moka::future::Cache;
use sea_orm::{DatabaseConnection, DbErr, EntityTrait};
use std::{iter, sync::Arc};
use trillium::{Conn, Handler, Info, Status};
use trillium_api::ApiConnExt;
use trillium_router::RouterConnExt;
use unicycle::FuturesUnordered;

pub fn handler() -> CachedWorldHandler {
    CachedWorldHandler {
        cache: Cache::new(10_000),
    }
}

#[derive(Debug, Clone)]
pub struct CachedWorldHandler {
    cache: Cache<i32, CachedWorld>,
}

impl CachedWorldHandler {
    #[inline(always)]
    fn count_param(conn: &Conn) -> usize {
        conn.param("count")
            .and_then(|s| s.parse().ok())
            .unwrap_or(1)
            .min(500)
            .max(1)
    }

    #[inline(always)]
    async fn fetch_cached(
        &self,
        db: &DatabaseConnection,
        id: i32,
    ) -> Result<CachedWorld, Arc<DbErr>> {
        self.cache
            .try_get_with(id, async {
                CachedWorlds::find_by_id(id)
                    .one(db)
                    .await?
                    .ok_or_else(|| DbErr::RecordNotFound(String::from("not found")))
            })
            .await
    }
}

#[trillium::async_trait]
impl Handler for CachedWorldHandler {
    async fn init(&mut self, _info: &mut Info) {
        if self.cache.entry_count() == 0 {
            let db = Db::connection().await;
            let mut stream = CachedWorlds::find().stream(&db).await.unwrap();
            while let Some(Ok(world)) = stream.next().await {
                self.cache.insert(world.id, world).await
            }
            self.cache.run_pending_tasks().await;
        }
    }

    async fn run(&self, conn: Conn) -> Conn {
        let count = Self::count_param(&conn);
        let db = conn.db();
        let worlds: Result<Vec<_>, _> =
            iter::repeat_with(|| self.fetch_cached(db, fastrand::i32(1..=10_000)))
                .take(count)
                .collect::<FuturesUnordered<_>>()
                .try_collect()
                .await;

        match worlds {
            Ok(worlds) => conn.with_json(&worlds),
            Err(_) => conn.with_status(Status::InternalServerError),
        }
    }
}
