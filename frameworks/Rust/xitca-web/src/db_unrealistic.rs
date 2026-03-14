//! this module is unrealistic. related issue:
//! https://github.com/TechEmpower/FrameworkBenchmarks/issues/8790

use core::future::Future;

use xitca_postgres::{Execute, iter::AsyncLendingIterator, statement::Statement};

use super::{
    db::Exec,
    ser::{Fortunes, World},
    util::{DB_URL, HandleResult},
};

pub struct Client {
    cli: xitca_postgres::Client,
    exec: Exec,
    fortune: Statement,
    world: Statement,
    update: Statement,
}

impl Client {
    pub async fn create() -> HandleResult<Self> {
        let (cli, drv) = xitca_postgres::Postgres::new(DB_URL).connect().await?;

        let mut drv = drv.try_into_tcp().expect("raw tcp is used for database connection");

        tokio::task::spawn(async move {
            while drv.try_next().await?.is_some() {}
            HandleResult::Ok(())
        });

        let world = Exec::WORLD_STMT.execute(&cli).await?.leak();
        let fortune = Exec::FORTUNE_STMT.execute(&cli).await?.leak();
        let update = Exec::UPDATE_STMT.execute(&cli).await?.leak();

        Ok(Self {
            cli,
            exec: Default::default(),
            world,
            fortune,
            update,
        })
    }

    #[inline]
    pub fn db(&self) -> impl Future<Output = HandleResult<World>> {
        self.exec.db(&self.cli, &self.world)
    }

    #[inline]
    pub fn queries(&self, num: u16) -> impl Future<Output = HandleResult<Vec<World>>> {
        self.exec.queries(&self.cli, &self.world, num)
    }

    #[inline]
    pub fn updates(&self, num: u16) -> impl Future<Output = HandleResult<Vec<World>>> {
        self.exec.updates(&self.cli, &self.world, &self.update, num)
    }

    #[inline]
    pub fn fortunes(&self) -> impl Future<Output = HandleResult<Fortunes>> {
        Exec::fortunes(&self.cli, &self.fortune)
    }
}
