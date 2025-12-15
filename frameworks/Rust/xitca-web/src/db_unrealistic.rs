//! this module is unrealistic. related issue:
//! https://github.com/TechEmpower/FrameworkBenchmarks/issues/8790

use core::future::Future;

use xitca_postgres::{Execute, iter::AsyncLendingIterator, statement::Statement};

use super::{
    ser::{Fortunes, World},
    util::{DB_URL, HandleResult, Rand},
};

use crate::db::{self, FORTUNE_STMT, UPDATE_STMT, WORLD_STMT};

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
    #[inline]
    pub fn db(&self) -> impl Future<Output = HandleResult<World>> {
        db::db(&self.cli, &self.rng, &self.world)
    }

    #[inline]
    pub fn queries(&self, num: u16) -> impl Future<Output = HandleResult<Vec<World>>> {
        db::queries(&self.cli, &self.rng, &self.world, num)
    }

    #[inline]
    pub fn updates(&self, num: u16) -> impl Future<Output = HandleResult<Vec<World>>> {
        db::updates(&self.cli, &self.rng, &self.world, &self.update, num)
    }

    #[inline]
    pub fn fortunes(&self) -> impl Future<Output = HandleResult<Fortunes>> {
        db::fortunes(&self.cli, &self.fortune)
    }
}
