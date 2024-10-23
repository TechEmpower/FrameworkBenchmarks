#[path = "./db_util.rs"]
mod db_util;

use std::{
    io,
    sync::{Arc, Mutex},
};

use diesel::{prelude::*, r2d2};

use crate::{
    ser::{Fortune, Fortunes, World},
    util::{HandleResult, Rand, DB_URL},
};

use db_util::{not_found, update_query_from_ids};

pub type Pool = Arc<_Pool>;

pub struct _Pool {
    pool: r2d2::Pool<r2d2::ConnectionManager<PgConnection>>,
    rng: Mutex<Rand>,
}

pub fn create() -> io::Result<Arc<_Pool>> {
    r2d2::Builder::new()
        .max_size(100)
        .min_idle(Some(100))
        .test_on_check_out(false)
        .idle_timeout(None)
        .max_lifetime(None)
        .build(r2d2::ConnectionManager::new(DB_URL))
        .map_err(io::Error::other)
        .map(|pool| {
            Arc::new(_Pool {
                pool,
                rng: Mutex::new(Rand::default()),
            })
        })
}

impl _Pool {
    pub fn get_world(&self) -> HandleResult<World> {
        use crate::schema::world::dsl::*;

        let w_id = self.rng.lock().unwrap().gen_id();
        let mut conn = self.pool.get()?;
        world.filter(id.eq(w_id)).load(&mut conn)?.pop().ok_or_else(not_found)
    }

    pub fn get_worlds(&self, num: u16) -> HandleResult<Vec<World>> {
        use crate::schema::world::dsl::*;

        let mut conn = self.pool.get()?;
        core::iter::repeat_with(|| {
            let w_id = self.rng.lock().unwrap().gen_id();
            world.filter(id.eq(w_id)).load(&mut conn)?.pop().ok_or_else(not_found)
        })
        .take(num as _)
        .collect()
    }

    pub fn update(&self, num: u16) -> HandleResult<Vec<World>> {
        use crate::schema::world::dsl::*;

        let mut rngs = {
            let mut rng = self.rng.lock().unwrap();
            (0..num).map(|_| (rng.gen_id(), rng.gen_id())).collect::<Vec<_>>()
        };

        rngs.sort_by(|(a, _), (b, _)| a.cmp(b));

        let update_sql = update_query_from_ids(&rngs);

        let mut conn = self.pool.get()?;

        let worlds = rngs
            .into_iter()
            .map(|(w_id, num)| {
                let mut w: World = world.filter(id.eq(w_id)).load(&mut conn)?.pop().ok_or_else(not_found)?;
                w.randomnumber = num;
                Ok(w)
            })
            .collect::<HandleResult<Vec<_>>>()?;

        diesel::sql_query(update_sql).execute(&mut conn)?;

        Ok(worlds)
    }

    pub fn tell_fortune(&self) -> HandleResult<Fortunes> {
        use crate::schema::fortune::dsl::*;

        let mut items = {
            let mut conn = self.pool.get()?;
            fortune.load(&mut conn)?
        };

        items.push(Fortune::new(0, "Additional fortune added at request time."));
        items.sort_by(|it, next| it.message.cmp(&next.message));

        Ok(Fortunes::new(items))
    }
}
