use xitca_postgres::{Execute, pool::Pool};

use super::{
    ser::{Fortunes, World},
    util::{DB_URL, HandleResult, Rand},
};

use crate::db::{self, FORTUNE_STMT, UPDATE_STMT, WORLD_STMT};

pub struct Client {
    pool: Pool,
    rng: core::cell::RefCell<Rand>,
}

pub async fn create() -> HandleResult<Client> {
    Ok(Client {
        pool: Pool::builder(DB_URL).capacity(1).build()?,
        rng: Default::default(),
    })
}

impl Client {
    pub async fn db(&self) -> HandleResult<World> {
        let mut conn = self.pool.get().await?;
        let stmt = WORLD_STMT.execute(&mut conn).await?;
        db::db(conn, &self.rng, &stmt).await
    }

    pub async fn queries(&self, num: u16) -> HandleResult<Vec<World>> {
        let mut conn = self.pool.get().await?;
        let stmt = WORLD_STMT.execute(&mut conn).await?;
        db::queries(conn, &self.rng, &stmt, num).await
    }

    pub async fn updates(&self, num: u16) -> HandleResult<Vec<World>> {
        let mut conn = self.pool.get().await?;
        let world_stmt = WORLD_STMT.execute(&mut conn).await?;
        let update_stmt = UPDATE_STMT.execute(&mut conn).await?;
        db::updates(conn, &self.rng, &world_stmt, &update_stmt, num).await
    }

    pub async fn fortunes(&self) -> HandleResult<Fortunes> {
        let mut conn = self.pool.get().await?;
        let stmt = FORTUNE_STMT.execute(&mut conn).await?;
        db::fortunes(conn, &stmt).await
    }
}
