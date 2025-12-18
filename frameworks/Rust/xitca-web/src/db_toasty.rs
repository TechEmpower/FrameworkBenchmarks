use std::sync::{Arc, Mutex};

use futures_util::future::TryJoinAll;
use toasty::Db;

use crate::{
    ser::{Fortune, Fortunes, World},
    util::{DB_URL, HandleResult, Rand},
};

#[derive(Clone)]
pub struct Pool(Arc<_Pool>);

impl core::ops::Deref for Pool {
    type Target = _Pool;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct _Pool {
    db: Db,
    rng: Mutex<Rand>,
}

impl Pool {
    pub async fn create() -> HandleResult<Self> {
        let drv = xitca_postgres_toasty::PostgreSQL::new(DB_URL)?;

        let db = Db::builder()
            .register::<World>()
            .register::<Fortune>()
            .build(drv)
            .await?;

        Ok(Self(Arc::new(_Pool {
            db,
            rng: Default::default(),
        })))
    }

    pub async fn db(&self) -> HandleResult<World> {
        let id = self.rng.lock().unwrap().gen_id();
        World::get_by_id(&self.db, id).await.map_err(Into::into)
    }

    pub async fn queries(&self, num: u16) -> HandleResult<Vec<World>> {
        let get = self
            .rng
            .lock()
            .unwrap()
            .gen_multi()
            .take(num as _)
            .map(|id| World::get_by_id(&self.db, id))
            .collect::<TryJoinAll<_>>();

        get.await.map_err(Into::into)
    }

    pub async fn updates(&self, num: u16) -> HandleResult<Vec<World>> {
        let mut worlds = self.queries(num).await?;

        // TODO: revisit when toasty supports batch update or raw sql
        let update = worlds
            .iter_mut()
            .zip(self.rng.lock().unwrap().gen_multi())
            .map(|(world, rand)| world.update().randomnumber(rand).exec(&self.db))
            .collect::<TryJoinAll<_>>();

        update.await?;

        Ok(worlds)
    }

    pub async fn fortunes(&self) -> HandleResult<Fortunes> {
        let fortunes = Fortune::all().all(&self.db).await?.collect().await?;
        Ok(Fortunes::new(fortunes))
    }
}
