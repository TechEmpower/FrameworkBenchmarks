#[path = "./db_util.rs"]
mod db_util;

use xitca_postgres::{Execute, iter::AsyncLendingIterator, pool::Pool};

use super::{
    ser::{Fortune, Fortunes, World},
    util::{DB_URL, HandleResult, Rand},
};

use db_util::{FORTUNE_STMT, UPDATE_STMT, WORLD_STMT, not_found};

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
    pub async fn get_world(&self) -> HandleResult<World> {
        let mut conn = self.pool.get().await?;
        let stmt = WORLD_STMT.execute(&mut conn).await?;
        let id = self.rng.borrow_mut().gen_id();
        let mut res = stmt.bind([id]).query(&conn.consume()).await?;
        let row = res.try_next().await?.ok_or_else(not_found)?;
        Ok(World::new(row.get(0), row.get(1)))
    }

    pub async fn get_worlds(&self, num: u16) -> HandleResult<Vec<World>> {
        let mut conn = self.pool.get().await?;
        let stmt = WORLD_STMT.execute(&mut conn).await?;

        let get = self
            .rng
            .borrow_mut()
            .gen_multi()
            .take(num as _)
            .map(|id| stmt.bind([id]).query(&conn))
            .collect::<Vec<_>>();

        drop(conn);

        let mut worlds = Vec::with_capacity(num as _);

        for get in get {
            let mut res = get.await?;
            let row = res.try_next().await?.ok_or_else(not_found)?;
            worlds.push(World::new(row.get(0), row.get(1)));
        }

        Ok(worlds)
    }

    pub async fn update(&self, num: u16) -> HandleResult<Vec<World>> {
        let mut conn = self.pool.get().await?;
        let world_stmt = WORLD_STMT.execute(&mut conn).await?;
        let update_stmt = UPDATE_STMT.execute(&mut conn).await?;

        let (get, update, worlds) = {
            let mut rng = self.rng.borrow_mut();
            let mut ids = rng.gen_multi().take(num as _).collect::<Vec<_>>();
            ids.sort();

            let (get, rngs, worlds) = ids
                .iter()
                .cloned()
                .zip(rng.gen_multi())
                .map(|(id, rand)| {
                    let get = world_stmt.bind([id]).query(&conn);
                    (get, rand, World::new(id, rand))
                })
                .collect::<(Vec<_>, Vec<_>, Vec<_>)>();

            let update = update_stmt.bind([&ids, &rngs]).query(&conn.consume());

            (get, update, worlds)
        };

        for fut in get {
            let _rand = fut.await?.try_next().await?.ok_or_else(not_found)?.get::<i32>(1);
        }

        update.await?;

        Ok(worlds)
    }

    pub async fn tell_fortune(&self) -> HandleResult<Fortunes> {
        let mut fortunes = Vec::with_capacity(16);

        let mut conn = self.pool.get().await?;
        let stmt = FORTUNE_STMT.execute(&mut conn).await?;
        let mut res = stmt.query(&conn.consume()).await?;

        while let Some(row) = res.try_next().await? {
            fortunes.push(Fortune::new(row.get(0), row.get::<String>(1)));
        }

        Ok(Fortunes::new(fortunes))
    }
}
