use diesel::prelude::*;
use diesel_async::{AsyncConnection, RunQueryDsl};
use futures_util::future::TryJoinAll;
use xitca_postgres_diesel::AsyncPgConnection;

use crate::{
    ser::{Fortunes, World},
    util::{DB_URL, HandleResult, Rand},
};

pub struct Pool {
    pool: AsyncPgConnection,
    rng: core::cell::RefCell<Rand>,
}

impl Pool {
    pub async fn create() -> HandleResult<Self> {
        let pool = AsyncPgConnection::establish(DB_URL).await?;

        Ok(Self {
            pool,
            rng: Default::default(),
        })
    }

    pub async fn db(&self) -> HandleResult<World> {
        use schema::world::dsl::{id, world};

        let w_id = self.rng.borrow_mut().gen_id();
        let w = world.filter(id.eq(w_id)).first(&mut &self.pool).await?;
        Ok(w)
    }

    pub async fn queries(&self, num: u16) -> HandleResult<Vec<World>> {
        use schema::world::dsl::{id, world};

        let get = self
            .rng
            .borrow_mut()
            .gen_multi()
            .take(num as _)
            .map(|w_id| world.filter(id.eq(w_id)).first(&mut &self.pool))
            .collect::<TryJoinAll<_>>();

        get.await.map_err(Into::into)
    }

    pub async fn updates(&self, num: u16) -> HandleResult<Vec<World>> {
        let mut worlds = self.queries(num).await?;

        let params = worlds
            .iter_mut()
            .zip(self.rng.borrow_mut().gen_multi())
            .map(|(world, rand)| {
                world.randomnumber = rand;
                (world.id, rand)
            })
            .collect();

        let sql = update_query_from_ids(params);
        diesel::sql_query(sql).execute(&mut &self.pool).await?;

        Ok(worlds)
    }

    pub async fn fortunes(&self) -> HandleResult<Fortunes> {
        let fortunes = schema::fortune::dsl::fortune.load(&mut &self.pool).await?;
        Ok(Fortunes::new(fortunes))
    }
}

mod schema {
    diesel::table! {
        world (id) {
            id -> Integer,
            randomnumber -> Integer,
        }
    }

    diesel::table! {
        fortune (id) {
            id -> Integer,
            message -> Text,
        }
    }
}

// diesel does not support high level bulk update api. use raw sql to bypass the limitation.
// relate discussion: https://github.com/diesel-rs/diesel/discussions/2879
fn update_query_from_ids(mut rngs: Vec<(i32, i32)>) -> String {
    rngs.sort_by(|(a, _), (b, _)| a.cmp(b));

    const PREFIX: &str = "UPDATE world SET randomNumber=w.r FROM (VALUES ";
    const SUFFIX: &str = ") AS w (i,r) WHERE world.id=w.i";

    let mut query = String::from(PREFIX);

    use core::fmt::Write;
    rngs.iter().for_each(|(w_id, num)| {
        write!(query, "({}::int,{}::int),", w_id, num).unwrap();
    });

    if query.ends_with(',') {
        query.pop();
    }

    query.push_str(SUFFIX);

    query
}
