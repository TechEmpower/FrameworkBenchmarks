//! this module is unrealistic. related issue:
//! https://github.com/TechEmpower/FrameworkBenchmarks/issues/8790

#[path = "./db_util.rs"]
mod db_util;

use std::cell::RefCell;

use xitca_postgres::{Execute, iter::AsyncLendingIterator, pipeline::Pipeline, statement::Statement};

use super::{
    ser::{Fortune, Fortunes, World},
    util::{DB_URL, HandleResult},
};

use db_util::{FORTUNE_STMT, Shared, UPDATE_STMT, WORLD_STMT, not_found};

pub struct Client {
    cli: xitca_postgres::Client,
    shared: RefCell<Shared>,
    fortune: Statement,
    world: Statement,
    update: Statement,
}

pub async fn create() -> HandleResult<Client> {
    let (cli, mut drv) = xitca_postgres::Postgres::new(DB_URL).connect().await?;

    tokio::task::spawn(async move {
        while drv.try_next().await?.is_some() {}
        HandleResult::Ok(())
    });

    let world = WORLD_STMT.execute(&cli).await?.leak();
    let fortune = FORTUNE_STMT.execute(&cli).await?.leak();
    let update = UPDATE_STMT.execute(&cli).await?.leak();

    Ok(Client {
        cli,
        shared: Default::default(),
        world,
        fortune,
        update,
    })
}

impl Client {
    pub async fn get_world(&self) -> HandleResult<World> {
        let id = self.shared.borrow_mut().0.gen_id();
        let mut res = self.world.bind([id]).query(&self.cli).await?;
        let row = res.try_next().await?.ok_or_else(not_found)?;
        Ok(World::new(row.get(0), row.get(1)))
    }

    pub async fn get_worlds(&self, num: u16) -> HandleResult<Vec<World>> {
        let mut res = {
            let (ref mut rng, ref mut buf) = *self.shared.borrow_mut();
            let mut pipe = Pipeline::with_capacity_from_buf(num as _, buf);
            rng.gen_multi()
                .take(num as _)
                .try_for_each(|id| self.world.bind([id]).query(&mut pipe))?;
            pipe.query(&self.cli)?
        };

        let mut worlds = Vec::with_capacity(num as _);

        while let Some(mut item) = res.try_next().await? {
            while let Some(row) = item.try_next().await? {
                worlds.push(World::new(row.get(0), row.get(1)));
            }
        }

        Ok(worlds)
    }

    pub async fn update(&self, num: u16) -> HandleResult<Vec<World>> {
        let len = num as usize;

        let (mut res, worlds) = {
            let (ref mut rng, ref mut buf) = *self.shared.borrow_mut();
            let mut pipe = Pipeline::with_capacity_from_buf(len + 1, buf);

            let mut ids = rng.gen_multi().take(num as _).collect::<Vec<_>>();
            ids.sort();

            let (rngs, worlds) = ids
                .iter()
                .cloned()
                .zip(rng.gen_multi())
                .map(|(id, rand)| {
                    self.world.bind([id]).query(&mut pipe)?;
                    HandleResult::Ok((rand, World::new(id, rand)))
                })
                .collect::<HandleResult<(Vec<_>, Vec<_>)>>()?;
            self.update.bind([&ids, &rngs]).query(&mut pipe)?;

            (pipe.query(&self.cli)?, worlds)
        };

        while let Some(mut item) = res.try_next().await? {
            while let Some(row) = item.try_next().await? {
                let _rand = row.get::<i32>(1);
            }
        }

        Ok(worlds)
    }

    pub async fn tell_fortune(&self) -> HandleResult<Fortunes> {
        let mut items = Vec::with_capacity(16);

        let mut res = self.fortune.query(&self.cli).await?;

        while let Some(row) = res.try_next().await? {
            items.push(Fortune::new(row.get(0), row.get::<String>(1)));
        }

        Ok(Fortunes::new(items))
    }
}
