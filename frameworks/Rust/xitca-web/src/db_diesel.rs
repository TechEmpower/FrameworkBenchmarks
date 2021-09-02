use std::{error::Error, io};

use diesel::{prelude::*, r2d2};
use rand::{rngs::SmallRng, Rng, SeedableRng};
use tokio::task::spawn_blocking;

use super::ser::{Fortune, Fortunes, World};

type DbResult<T> = Result<T, Box<dyn Error + Send + Sync + 'static>>;

#[derive(Clone)]
pub struct DieselPool {
    pool: r2d2::Pool<r2d2::ConnectionManager<PgConnection>>,
    rng: SmallRng,
}

pub fn connect(config: &str) -> io::Result<DieselPool> {
    let manager = r2d2::ConnectionManager::new(config);
    let pool = r2d2::Builder::new()
        .max_size(5)
        .min_idle(Some(5))
        .test_on_check_out(false)
        .idle_timeout(None)
        .max_lifetime(None)
        .build(manager)
        .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;

    Ok(DieselPool {
        pool,
        rng: SmallRng::from_entropy(),
    })
}

impl DieselPool {
    pub async fn get_world(&self) -> DbResult<World> {
        let mut this = self.clone();

        spawn_blocking(move || {
            use crate::schema::world::dsl::*;

            let conn = this.pool.get()?;

            let random_id = this.rng.gen_range(1..10_001);
            let w = world
                .filter(id.eq(random_id))
                .load::<World>(&conn)?
                .pop()
                .unwrap();

            Ok(w)
        })
        .await?
    }

    pub async fn get_worlds(&self, num: u16) -> DbResult<Vec<World>> {
        let mut this = self.clone();

        spawn_blocking(move || {
            use crate::schema::world::dsl::*;

            let conn = this.pool.get()?;

            (0..num)
                .map(|_| {
                    let w_id = this.rng.gen_range(1..10_001);
                    let w = world
                        .filter(id.eq(w_id))
                        .load::<World>(&conn)?
                        .pop()
                        .unwrap();
                    Ok(w)
                })
                .collect()
        })
        .await?
    }

    pub async fn update(&self, num: u16) -> DbResult<Vec<World>> {
        let mut this = self.clone();

        spawn_blocking(move || {
            use crate::schema::world::dsl::*;

            let conn = this.pool.get()?;

            let mut worlds = (0..num)
                .map(|_| {
                    let w_id: i32 = this.rng.gen_range(1..10_001);
                    let mut w = world
                        .filter(id.eq(w_id))
                        .load::<World>(&conn)?
                        .pop()
                        .unwrap();
                    w.randomnumber = this.rng.gen_range(1..10_001);
                    Ok(w)
                })
                .collect::<DbResult<Vec<_>>>()?;

            worlds.sort_by_key(|w| w.id);

            conn.transaction::<_, diesel::result::Error, _>(|| {
                for w in &worlds {
                    diesel::update(world)
                        .filter(id.eq(w.id))
                        .set(randomnumber.eq(w.randomnumber))
                        .execute(&conn)?;
                }
                Ok(())
            })?;

            Ok(worlds)
        })
        .await?
    }

    pub async fn tell_fortune(&self) -> DbResult<Fortunes> {
        let this = self.clone();

        spawn_blocking(move || {
            use crate::schema::fortune::dsl::*;

            let conn = this.pool.get()?;

            let mut items = fortune.load::<Fortune>(&conn)?;

            items.push(Fortune::new(0, "Additional fortune added at request time."));
            items.sort_by(|it, next| it.message.cmp(&next.message));

            Ok(Fortunes::new(items))
        })
        .await?
    }
}
