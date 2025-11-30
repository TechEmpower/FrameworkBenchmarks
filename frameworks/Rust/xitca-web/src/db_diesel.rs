#[path = "./db_util.rs"]
mod db_util;

use core::cell::RefCell;

use std::io;

use diesel::prelude::*;
use diesel_async::{
    RunQueryDsl,
    pooled_connection::{AsyncDieselConnectionManager, bb8},
};
use futures_util::future::{TryFutureExt, try_join, try_join_all};
use xitca_postgres_diesel::AsyncPgConnection;

use crate::{
    ser::{Fortune, Fortunes, World},
    util::{DB_URL, HandleResult, Rand},
};

use db_util::update_query_from_ids;

pub struct Pool {
    pool: bb8::Pool<AsyncPgConnection>,
    rng: RefCell<Rand>,
}

pub async fn create() -> io::Result<Pool> {
    bb8::Pool::builder()
        .max_size(1)
        .min_idle(Some(1))
        .test_on_check_out(false)
        .build(AsyncDieselConnectionManager::new(DB_URL))
        .await
        .map_err(io::Error::other)
        .map(|pool| Pool {
            pool,
            rng: RefCell::new(Rand::default()),
        })
}

impl Pool {
    pub async fn get_world(&self) -> HandleResult<World> {
        {
            use crate::schema::world::dsl::*;

            let w_id = self.rng.borrow_mut().gen_id();
            let mut conn = self.pool.get().await?;
            world.filter(id.eq(w_id)).first(&mut conn).map_err(Into::into)
        }
        .await
    }

    pub async fn get_worlds(&self, num: u16) -> HandleResult<Vec<World>> {
        try_join_all({
            use crate::schema::world::dsl::*;

            let mut conn = self.pool.get().await?;
            let mut rng = self.rng.borrow_mut();

            core::iter::repeat_with(|| {
                let w_id = rng.gen_id();
                world.filter(id.eq(w_id)).first(&mut conn).map_err(Into::into)
            })
            .take(num as _)
            .collect::<Vec<_>>()
        })
        .await
    }

    pub async fn update(&self, num: u16) -> HandleResult<Vec<World>> {
        let (get, update) = {
            use crate::schema::world::dsl::*;

            let mut conn = self.pool.get().await?;
            let mut rng = self.rng.borrow_mut();

            let (rngs, get) = core::iter::repeat_with(|| {
                let w_id = rng.gen_id();
                let rng = rng.gen_id();

                let get = world.filter(id.eq(w_id)).first::<World>(&mut conn);

                ((w_id, rng), async move {
                    let mut w = get.await?;
                    w.randomnumber = rng;
                    HandleResult::Ok(w)
                })
            })
            .take(num as _)
            .collect::<(Vec<_>, Vec<_>)>();

            let update = diesel::sql_query(update_query_from_ids(rngs))
                .execute(&mut conn)
                .map_err(Into::into);

            (try_join_all(get), update)
        };

        try_join(get, update).await.map(|(worlds, _)| worlds)
    }

    pub async fn tell_fortune(&self) -> HandleResult<Fortunes> {
        {
            use crate::schema::fortune::dsl::*;

            let mut conn = self.pool.get().await?;
            fortune.load(&mut conn).map_err(Into::into)
        }
        .await
        .map(Fortunes::new)
    }
}
