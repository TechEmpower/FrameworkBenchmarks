#[path = "./db_util.rs"]
mod db_util;

use core::cell::RefCell;

use xitca_io::bytes::BytesMut;
use xitca_postgres::{Execute, iter::AsyncLendingIterator, pipeline::Pipeline, pool::Pool};

use super::{
    ser::{Fortune, Fortunes, World},
    util::{DB_URL, HandleResult, Rand},
};

use db_util::{FORTUNE_STMT, UPDATE_STMT, WORLD_STMT, not_found};

pub struct Client {
    pool: Pool,
    shared: RefCell<(Rand, BytesMut)>,
}

pub async fn create() -> HandleResult<Client> {
    Ok(Client {
        pool: Pool::builder(DB_URL).capacity(1).build()?,
        shared: Default::default(),
    })
}

impl Client {
    pub async fn get_world(&self) -> HandleResult<World> {
        let mut conn = self.pool.get().await?;
        let stmt = WORLD_STMT.execute(&mut conn).await?;
        let id = self.shared.borrow_mut().0.gen_id();
        let mut res = stmt.bind([id]).query(&conn.consume()).await?;
        let row = res.try_next().await?.ok_or_else(not_found)?;
        Ok(World::new(row.get(0), row.get(1)))
    }

    pub async fn get_worlds(&self, num: u16) -> HandleResult<Vec<World>> {
        let mut conn = self.pool.get().await?;
        let stmt = WORLD_STMT.execute(&mut conn).await?;

        let mut res = {
            let (ref mut rng, ref mut buf) = *self.shared.borrow_mut();
            let mut pipe = Pipeline::with_capacity_from_buf(num as _, buf);
            rng.gen_multi()
                .take(num as _)
                .try_for_each(|id| stmt.bind([id]).query(&mut pipe))?;
            pipe.query(&conn.consume())?
        };

        let mut worlds = Vec::with_capacity(num as _);

        while let Some(mut item) = res.try_next().await? {
            let row = item.try_next().await?.ok_or_else(not_found)?;
            worlds.push(World::new(row.get(0), row.get(1)));
        }

        Ok(worlds)
    }

    pub async fn update(&self, num: u16) -> HandleResult<Vec<World>> {
        let mut conn = self.pool.get().await?;
        let world_stmt = WORLD_STMT.execute(&mut conn).await?;
        let update_stmt = UPDATE_STMT.execute(&mut conn).await?;

        let (mut res, worlds) = {
            let (ref mut rng, ref mut buf) = *self.shared.borrow_mut();
            let mut pipe = Pipeline::with_capacity_from_buf((num + 1) as _, buf);

            let mut ids = rng.gen_multi().take(num as _).collect::<Vec<_>>();
            ids.sort();

            let (rngs, worlds) = ids
                .iter()
                .cloned()
                .zip(rng.gen_multi())
                .map(|(id, rand)| {
                    world_stmt.bind([id]).query(&mut pipe)?;
                    HandleResult::Ok((rand, World::new(id, rand)))
                })
                .collect::<HandleResult<(Vec<_>, Vec<_>)>>()?;
            update_stmt.bind([&ids, &rngs]).query(&mut pipe)?;
            (pipe.query(&conn.consume())?, worlds)
        };

        while let Some(mut item) = res.try_next().await? {
            while let Some(row) = item.try_next().await? {
                let _rand = row.get::<i32>(1);
            }
        }

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
