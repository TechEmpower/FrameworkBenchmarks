use xitca_postgres::{
    Execute,
    dev::ClientBorrow,
    iter::AsyncLendingIterator,
    statement::{Statement, StatementNamed},
    types::Type,
};

use crate::{
    ser::{Fortune, Fortunes, World},
    util::{Error, HandleResult, Rand},
};

#[cold]
#[inline(never)]
fn not_found() -> Error {
    "request World does not exist".into()
}

#[derive(Default)]
pub struct Exec {
    rng: core::cell::RefCell<Rand>,
}

impl Exec {
    pub const FORTUNE_STMT: StatementNamed<'_> = Statement::named("SELECT id,message FROM fortune", &[]);
    pub const WORLD_STMT: StatementNamed<'_> =
        Statement::named("SELECT id,randomNumber FROM world WHERE id=$1", &[Type::INT4]);
    pub const UPDATE_STMT: StatementNamed<'_> = Statement::named(
        "UPDATE world SET randomNumber=w.r FROM (SELECT unnest($1) as i,unnest($2) as r) w WHERE world.id=w.i",
        &[Type::INT4_ARRAY, Type::INT4_ARRAY],
    );

    pub(crate) async fn db<C>(&self, conn: C, stmt: &Statement) -> HandleResult<World>
    where
        C: ClientBorrow,
    {
        let id = self.rng.borrow_mut().gen_id();
        let mut res = stmt.bind([id]).query(conn.borrow_cli_ref()).await?;
        drop(conn);
        let row = res.try_next().await?.ok_or_else(not_found)?;
        Ok(World::new(row.get(0), row.get(1)))
    }

    pub(crate) async fn queries<C>(&self, conn: C, stmt: &Statement, num: u16) -> HandleResult<Vec<World>>
    where
        C: ClientBorrow,
    {
        let get = self
            .rng
            .borrow_mut()
            .gen_multi()
            .take(num as _)
            .map(|id| stmt.bind([id]).query(conn.borrow_cli_ref()))
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

    pub(crate) async fn updates<C>(
        &self,
        conn: C,
        world_stmt: &Statement,
        update_stmt: &Statement,
        num: u16,
    ) -> HandleResult<Vec<World>>
    where
        C: ClientBorrow,
    {
        let (worlds, get, update) = {
            let mut rng = self.rng.borrow_mut();
            let mut ids = rng.gen_multi().take(num as _).collect::<Vec<_>>();
            ids.sort();

            let (get, rngs, worlds) = ids
                .iter()
                .cloned()
                .zip(rng.gen_multi())
                .map(|(id, rand)| {
                    let get = world_stmt.bind([id]).query(conn.borrow_cli_ref());
                    (get, rand, World::new(id, rand))
                })
                .collect::<(Vec<_>, Vec<_>, Vec<_>)>();

            let update = update_stmt.bind([&ids, &rngs]).execute(conn.borrow_cli_ref());

            drop(conn);

            (worlds, get, update)
        };

        for get in get {
            let _rand = get.await?.try_next().await?.ok_or_else(not_found)?.get::<i32>(1);
        }

        update.await?;

        Ok(worlds)
    }

    pub(crate) async fn fortunes<C>(conn: C, stmt: &Statement) -> HandleResult<Fortunes>
    where
        C: ClientBorrow,
    {
        let mut res = stmt.query(conn.borrow_cli_ref()).await?;

        drop(conn);

        let mut fortunes = Vec::with_capacity(16);

        while let Some(row) = res.try_next().await? {
            fortunes.push(Fortune::new(row.get(0), row.get_zc(1)));
        }

        Ok(Fortunes::new(fortunes))
    }
}
