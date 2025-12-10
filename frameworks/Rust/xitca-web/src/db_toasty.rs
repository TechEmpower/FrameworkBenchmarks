use futures_util::future::try_join_all;
use toasty::Db;

use crate::{
    ser::{Fortune, Fortunes, World},
    util::{DB_URL, HandleResult, Rand},
};

pub struct Pool {
    db: Db,
    rng: core::cell::RefCell<Rand>,
}

impl Pool {
    pub async fn create() -> HandleResult<Self> {
        let conn = xitca_postgres_toasty::PostgreSQL::connect(DB_URL).await?;

        let db = Db::builder()
            .register::<World>()
            .register::<Fortune>()
            .build(conn)
            .await?;

        Ok(Self {
            db,
            rng: Default::default(),
        })
    }

    pub async fn get_world(&self) -> HandleResult<World> {
        let id = self.rng.borrow_mut().gen_id();
        World::get_by_id(&self.db, id).await.map_err(Into::into)
    }

    pub async fn get_worlds(&self, num: u16) -> HandleResult<Vec<World>> {
        try_join_all(core::iter::repeat_with(|| self.get_world()).take(num as _)).await
    }

    pub async fn update(&self, num: u16) -> HandleResult<Vec<World>> {
        let mut worlds = self.get_worlds(num).await?;

        try_join_all({
            let mut rng = self.rng.borrow_mut();
            worlds
                .iter_mut()
                .map(move |world| world.update().randomnumber(rng.gen_id()).exec(&self.db))
        })
        .await?;

        Ok(worlds)
    }

    pub async fn tell_fortune(&self) -> HandleResult<Fortunes> {
        let fortunes = Fortune::all().all(&self.db).await?.collect().await?;
        Ok(Fortunes::new(fortunes))
    }
}
