//! this module is unrealistic. related issue:
//! https://github.com/TechEmpower/FrameworkBenchmarks/issues/8790

#[path = "./db_util.rs"]
mod db_util;

use xitca_postgres::{Execute, iter::AsyncLendingIterator, statement::Statement};

use super::{
    ser::{Fortune, Fortunes, World},
    util::{DB_URL, HandleResult, Rand},
};

use db_util::{FORTUNE_STMT, UPDATE_STMT, WORLD_STMT, not_found};

pub struct Client {
    cli: xitca_postgres::Client,
    rng: core::cell::RefCell<Rand>,
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
        rng: Default::default(),
        world,
        fortune,
        update,
    })
}

impl Client {
    pub async fn get_world(&self) -> HandleResult<World> {
        let id = self.rng.borrow_mut().gen_id();
        let mut res = self.world.bind([id]).query(&self.cli).await?;
        let row = res.try_next().await?.ok_or_else(not_found)?;
        Ok(World::new(row.get(0), row.get(1)))
    }

    pub async fn get_worlds(&self, num: u16) -> HandleResult<Vec<World>> {
        let get = self
            .rng
            .borrow_mut()
            .gen_multi()
            .take(num as _)
            .map(|id| self.world.bind([id]).query(&self.cli))
            .collect::<Vec<_>>();

        let mut worlds = Vec::with_capacity(num as _);

        for query in get {
            let mut res = query.await?;
            let row = res.try_next().await?.ok_or_else(not_found)?;
            worlds.push(World::new(row.get(0), row.get(1)));
        }

        Ok(worlds)
    }

    pub async fn update(&self, num: u16) -> HandleResult<Vec<World>> {
        let (get, update, worlds) = {
            let mut rng = self.rng.borrow_mut();
            let mut ids = rng.gen_multi().take(num as _).collect::<Vec<_>>();
            ids.sort();

            let (get, rngs, worlds) = ids
                .iter()
                .cloned()
                .zip(rng.gen_multi())
                .map(|(id, rand)| {
                    let get = self.world.bind([id]).query(&self.cli);
                    (get, rand, World::new(id, rand))
                })
                .collect::<(Vec<_>, Vec<_>, Vec<_>)>();

            let update = self.update.bind([&ids, &rngs]).query(&self.cli);

            (get, update, worlds)
        };

        for fut in get {
            let _rand = fut.await?.try_next().await?.ok_or_else(not_found)?.get::<i32>(1);
        }

        update.await?;

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
