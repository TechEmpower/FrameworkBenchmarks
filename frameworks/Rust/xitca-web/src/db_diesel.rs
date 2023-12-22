use std::sync::{Arc, Mutex};

use diesel::{prelude::*, r2d2};

use crate::{
    ser::{Fortune, Fortunes, World},
    util::{Error, HandleResult, Rand, DB_URL},
};

pub type DieselPool = Arc<Pool>;

pub struct Pool {
    pool: r2d2::Pool<r2d2::ConnectionManager<PgConnection>>,
    rng: Mutex<Rand>,
}

pub fn create() -> std::io::Result<Arc<Pool>> {
    r2d2::Builder::new()
        .max_size(256)
        .min_idle(Some(256))
        .test_on_check_out(false)
        .idle_timeout(None)
        .max_lifetime(None)
        .build(r2d2::ConnectionManager::new(DB_URL))
        .map_err(std::io::Error::other)
        .map(|pool| {
            Arc::new(Pool {
                pool,
                rng: Mutex::new(Rand::default()),
            })
        })
}

#[cold]
#[inline(never)]
fn not_found() -> Error {
    format!("world not found").into()
}

impl Pool {
    pub fn get_world(&self) -> HandleResult<World> {
        use crate::schema::world::dsl::*;

        let w_id = self.rng.lock().unwrap().gen_id();

        let mut conn = self.pool.get()?;
        world
            .filter(id.eq(w_id))
            .load(&mut conn)?
            .pop()
            .ok_or_else(not_found)
    }

    pub fn get_worlds(&self, num: u16) -> HandleResult<Vec<World>> {
        use crate::schema::world::dsl::*;

        let mut conn = self.pool.get()?;

        (0..num)
            .map(|_| {
                let w_id = self.rng.lock().unwrap().gen_id();
                world
                    .filter(id.eq(w_id))
                    .load::<World>(&mut conn)?
                    .pop()
                    .ok_or_else(not_found)
            })
            .collect()
    }

    pub fn update(&self, num: u16) -> HandleResult<Vec<World>> {
        use crate::schema::world::dsl::*;

        let mut conn = self.pool.get()?;

        let mut worlds = (0..num)
            .map(|_| {
                let mut rng = self.rng.lock().unwrap();
                let w_id = rng.gen_id();
                let r_id = rng.gen_id();
                drop(rng);
                world
                    .filter(id.eq(w_id))
                    .load::<World>(&mut conn)?
                    .pop()
                    .map(|mut w| {
                        w.randomnumber = r_id;
                        w
                    })
                    .ok_or_else(not_found)
            })
            .collect::<HandleResult<Vec<_>>>()?;

        worlds.sort_by_key(|w| w.id);

        conn.transaction::<_, diesel::result::Error, _>(|conn| {
            for w in &worlds {
                diesel::update(world)
                    .filter(id.eq(w.id))
                    .set(randomnumber.eq(w.randomnumber))
                    .execute(conn)?;
            }
            Ok(())
        })?;

        Ok(worlds)
    }

    pub fn tell_fortune(&self) -> HandleResult<Fortunes> {
        use crate::schema::fortune::dsl::*;

        let mut conn = self.pool.get()?;

        let mut items = fortune.load::<Fortune>(&mut conn)?;

        items.push(Fortune::new(0, "Additional fortune added at request time."));
        items.sort_by(|it, next| it.message.cmp(&next.message));

        Ok(Fortunes::new(items))
    }
}
