#[path = "./db_util.rs"]
mod db_util;

use core::cell::RefCell;

use std::io;

use diesel::prelude::*;
use diesel_async::{
    RunQueryDsl,
    pooled_connection::{AsyncDieselConnectionManager, bb8},
};
use xitca_postgres_diesel::AsyncPgConnection;

use crate::{
    ser::{Fortune, Fortunes, World},
    util::{DB_URL, HandleResult, Rand},
};

use db_util::{not_found, update_query_from_ids};

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
        use crate::schema::world::dsl::*;
        {
            let w_id = self.rng.borrow_mut().gen_id();
            let mut conn = self.pool.get().await?;
            world.filter(id.eq(w_id)).load(&mut conn)
        }
        .await?
        .pop()
        .ok_or_else(not_found)
    }

    pub async fn get_worlds(&self, num: u16) -> HandleResult<Vec<World>> {
        let res = {
            use crate::schema::world::dsl::*;

            let mut conn = self.pool.get().await?;
            let mut rng = self.rng.borrow_mut();
            core::iter::repeat_with(|| {
                let w_id = rng.gen_id();
                world.filter(id.eq(w_id)).load(&mut conn)
            })
            .take(num as _)
            .collect::<Vec<_>>()
        };

        let mut worlds = Vec::with_capacity(num as _);

        for fut in res {
            let world = fut.await?.pop().ok_or_else(not_found)?;
            worlds.push(world);
        }

        Ok(worlds)
    }

    pub async fn update(&self, num: u16) -> HandleResult<Vec<World>> {
        let (get, update) = {
            use crate::schema::world::dsl::*;

            let mut conn = self.pool.get().await?;
            let mut rng = self.rng.borrow_mut();

            let (rngs, get) = core::iter::repeat_with(|| {
                let w_id = rng.gen_id();
                let num = rng.gen_id();

                let fut = world.filter(id.eq(w_id)).load::<World>(&mut conn);

                ((w_id, num), async move {
                    let mut w = fut.await?.pop().ok_or_else(not_found)?;
                    w.randomnumber = num;
                    HandleResult::Ok(w)
                })
            })
            .take(num as _)
            .collect::<(Vec<_>, Vec<_>)>();

            (get, diesel::sql_query(update_query_from_ids(rngs)).execute(&mut conn))
        };

        let mut worlds = Vec::with_capacity(num as _);

        for fut in get {
            let world = fut.await?;
            worlds.push(world);
        }

        update.await?;

        Ok(worlds)
    }

    pub async fn tell_fortune(&self) -> HandleResult<Fortunes> {
        let mut fortunes = {
            use crate::schema::fortune::dsl::*;

            let mut conn = self.pool.get().await?;
            fortune.load(&mut conn)
        }
        .await?;

        fortunes.push(Fortune::new(0, "Additional fortune added at request time."));
        fortunes.sort_by(|a, b| a.message.cmp(&b.message));

        Ok(Fortunes::new(fortunes))
    }
}
