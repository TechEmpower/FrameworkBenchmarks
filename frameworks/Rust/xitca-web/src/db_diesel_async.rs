#[path = "./db_util.rs"]
mod db_util;

use std::{io, sync::Mutex};

use diesel::prelude::*;
use diesel_async::{
    pooled_connection::{bb8, AsyncDieselConnectionManager},
    RunQueryDsl,
};
use futures_util::{
    future::join,
    stream::{FuturesUnordered, TryStreamExt},
};
use xitca_postgres_diesel::AsyncPgConnection;

use crate::{
    ser::{Fortune, Fortunes, World},
    util::{HandleResult, Rand, DB_URL},
};

use db_util::{not_found, update_query_from_ids};

pub struct Pool {
    pool: bb8::Pool<AsyncPgConnection>,
    rng: Mutex<Rand>,
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
            rng: Mutex::new(Rand::default()),
        })
}

impl Pool {
    pub async fn get_world(&self) -> HandleResult<World> {
        use crate::schema::world::dsl::*;
        {
            let w_id = self.rng.lock().unwrap().gen_id();
            let mut conn = self.pool.get().await?;
            world.filter(id.eq(w_id)).load(&mut conn)
        }
        .await?
        .pop()
        .ok_or_else(not_found)
    }

    pub async fn get_worlds(&self, num: u16) -> HandleResult<Vec<World>> {
        use crate::schema::world::dsl::*;
        {
            let mut conn = self.pool.get().await?;
            let mut rng = self.rng.lock().unwrap();
            core::iter::repeat_with(|| {
                let w_id = rng.gen_id();
                let fut = world.filter(id.eq(w_id)).load(&mut conn);
                async { fut.await?.pop().ok_or_else(not_found) }
            })
            .take(num as _)
            .collect::<FuturesUnordered<_>>()
        }
        .try_collect()
        .await
    }

    pub async fn update(&self, num: u16) -> HandleResult<Vec<World>> {
        use crate::schema::world::dsl::*;

        let (select_res, update_res) = {
            let mut conn = self.pool.get().await?;
            let mut rng = self.rng.lock().unwrap();

            let (select, mut rngs) = core::iter::repeat_with(|| {
                let w_id = rng.gen_id();
                let num = rng.gen_id();

                let fut = world.filter(id.eq(w_id)).load::<World>(&mut conn);
                let select = async move {
                    let mut w = fut.await?.pop().ok_or_else(not_found)?;
                    w.randomnumber = num;
                    HandleResult::Ok(w)
                };

                (select, (w_id, num))
            })
            .take(num as _)
            .collect::<(FuturesUnordered<_>, Vec<_>)>();

            rngs.sort_by(|(a, _), (b, _)| a.cmp(b));

            let update = diesel::sql_query(update_query_from_ids(&rngs)).execute(&mut conn);

            join(select.try_collect::<Vec<_>>(), update)
        }
        .await;

        update_res?;
        select_res
    }

    pub async fn tell_fortune(&self) -> HandleResult<Fortunes> {
        use crate::schema::fortune::dsl::*;

        let mut items = {
            let mut conn = self.pool.get().await?;
            fortune.load(&mut conn)
        }
        .await?;

        items.push(Fortune::new(0, "Additional fortune added at request time."));
        items.sort_by(|it, next| it.message.cmp(&next.message));

        Ok(Fortunes::new(items))
    }
}
