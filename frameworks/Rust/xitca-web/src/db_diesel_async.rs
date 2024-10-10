use std::{
    io,
    sync::{Arc, Mutex},
};

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
    util::{bulk_update_gen, Error, HandleResult, Rand, DB_URL},
};

pub type Pool = Arc<_Pool>;

pub struct _Pool {
    pool: bb8::Pool<AsyncPgConnection>,
    rng: Mutex<Rand>,
}

pub async fn create() -> io::Result<Arc<_Pool>> {
    bb8::Pool::builder()
        .max_size(1)
        .min_idle(Some(1))
        .test_on_check_out(false)
        .build(AsyncDieselConnectionManager::new(DB_URL))
        .await
        .map_err(io::Error::other)
        .map(|pool| {
            Arc::new(_Pool {
                pool,
                rng: Mutex::new(Rand::default()),
            })
        })
}

#[cold]
#[inline(never)]
fn not_found() -> Error {
    "world not found".into()
}

impl _Pool {
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
            (0..num)
                .map(|_| {
                    let w_id = rng.gen_id();
                    let fut = world.filter(id.eq(w_id)).load::<World>(&mut conn);
                    async { fut.await?.pop().ok_or_else(not_found) }
                })
                .collect::<FuturesUnordered<_>>()
        }
        .try_collect()
        .await
    }

    pub async fn update(&self, num: u16) -> HandleResult<Vec<World>> {
        use crate::schema::world::dsl::*;

        let mut rngs = Vec::with_capacity(num as _);

        let (select_res, update_res) = {
            let mut conn = self.pool.get().await?;

            let mut rng = self.rng.lock().unwrap();

            let select = (0..num)
                .map(|_| {
                    let w_id = rng.gen_id();
                    let num = rng.gen_id();

                    rngs.push((w_id, num));

                    let fut = world.filter(id.eq(w_id)).load::<World>(&mut conn);

                    async move {
                        fut.await?
                            .pop()
                            .map(|mut w| {
                                w.randomnumber = num;
                                w
                            })
                            .ok_or_else(not_found)
                    }
                })
                .collect::<FuturesUnordered<_>>();

            rngs.sort_by(|(a, _), (b, _)| a.cmp(b));

            let update = diesel::sql_query(update_query(&rngs)).execute(&mut conn);

            join(select.try_collect::<Vec<_>>(), update)
        }
        .await;

        update_res?;
        let mut worlds = select_res?;

        worlds.sort_by_key(|w| w.id);

        Ok(worlds)
    }

    pub async fn tell_fortune(&self) -> HandleResult<Fortunes> {
        use crate::schema::fortune::dsl::*;

        let mut items = {
            let mut conn = self.pool.get().await?;
            fortune.load::<Fortune>(&mut conn)
        }
        .await?;

        items.push(Fortune::new(0, "Additional fortune added at request time."));
        items.sort_by(|it, next| it.message.cmp(&next.message));

        Ok(Fortunes::new(items))
    }
}

// diesel does not support high level bulk update api. use raw sql to bypass the limitation.
// relate discussion: https://github.com/diesel-rs/diesel/discussions/2879
fn update_query(ids: &[(i32, i32)]) -> String {
    bulk_update_gen(|query| {
        use std::fmt::Write;
        ids.iter().for_each(|(w_id, num)| {
            write!(query, "({}::int,{}::int),", w_id, num).unwrap();
        });
    })
}
