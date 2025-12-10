use diesel::prelude::*;
use diesel_async::{AsyncConnection, RunQueryDsl};
use futures_util::future::{TryFutureExt, TryJoinAll, try_join};
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

    pub async fn get_world(&self) -> HandleResult<World> {
        {
            use schema::world::dsl::*;

            let w_id = self.rng.borrow_mut().gen_id();
            world.filter(id.eq(w_id)).first(&mut &self.pool).map_err(Into::into)
        }
        .await
    }

    pub async fn get_worlds(&self, num: u16) -> HandleResult<Vec<World>> {
        {
            use schema::world::dsl::*;

            self.rng
                .borrow_mut()
                .gen_multi()
                .take(num as _)
                .map(|w_id| world.filter(id.eq(w_id)).first(&mut &self.pool).map_err(Into::into))
                .collect::<TryJoinAll<_>>()
        }
        .await
    }

    pub async fn update(&self, num: u16) -> HandleResult<Vec<World>> {
        {
            use schema::world::dsl::*;

            let mut rng = self.rng.borrow_mut();
            let mut params = Vec::with_capacity(num as _);

            let get = rng
                .clone()
                .gen_multi()
                .take(num as _)
                .zip(rng.gen_multi())
                .map(|(w_id, rng)| {
                    let get = world.filter(id.eq(w_id)).first::<World>(&mut &self.pool);

                    params.push((w_id, rng));

                    async move {
                        let mut w = get.await?;
                        w.randomnumber = rng;
                        HandleResult::Ok(w)
                    }
                })
                .collect::<TryJoinAll<_>>();

            let sql = update_query_from_ids(params);
            let update = diesel::sql_query(sql).execute(&mut &self.pool).map_err(Into::into);

            try_join(get, update)
        }
        .await
        .map(|(worlds, _)| worlds)
    }

    pub async fn tell_fortune(&self) -> HandleResult<Fortunes> {
        {
            use schema::fortune::dsl::*;

            fortune.load(&mut &self.pool).map_err(Into::into)
        }
        .await
        .map(Fortunes::new)
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
