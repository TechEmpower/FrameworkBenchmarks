use xitca_postgres::{Execute, pool::Pool};

use super::{
    db::Exec,
    ser::{Fortunes, World},
    util::{DB_URL, HandleResult},
};

pub struct Client {
    pool: Pool,
    exec: Exec,
}

impl Client {
    pub async fn create() -> HandleResult<Self> {
        Ok(Self {
            pool: Pool::builder(DB_URL).capacity(1).build()?,
            exec: Default::default(),
        })
    }

    pub async fn db(&self) -> HandleResult<World> {
        let mut conn = self.pool.get().await?;
        let stmt = Exec::WORLD_STMT.execute(&mut conn).await?;
        self.exec.db(conn, &stmt).await
    }

    pub async fn queries(&self, num: u16) -> HandleResult<Vec<World>> {
        let mut conn = self.pool.get().await?;
        let stmt = Exec::WORLD_STMT.execute(&mut conn).await?;
        self.exec.queries(conn, &stmt, num).await
    }

    pub async fn updates(&self, num: u16) -> HandleResult<Vec<World>> {
        let mut conn = self.pool.get().await?;
        let world_stmt = Exec::WORLD_STMT.execute(&mut conn).await?;
        let update_stmt = Exec::UPDATE_STMT.execute(&mut conn).await?;
        self.exec.updates(conn, &world_stmt, &update_stmt, num).await
    }

    pub async fn fortunes(&self) -> HandleResult<Fortunes> {
        let mut conn = self.pool.get().await?;
        let stmt = Exec::FORTUNE_STMT.execute(&mut conn).await?;
        Exec::fortunes(conn, &stmt).await
    }
}
