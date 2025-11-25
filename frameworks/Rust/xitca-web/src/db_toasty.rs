use core::cell::RefCell;

use std::io;

use toasty::Db;
use xitca_postgres_toasty::PostgreSQL;

use crate::{
    ser::{Fortune, Fortunes, World},
    util::{DB_URL, HandleResult, Rand},
};

// this is not a realistic connection pool.
pub struct Pool {
    db: Db,
    rng: RefCell<Rand>,
}

pub async fn create() -> io::Result<Pool> {
    let conn = PostgreSQL::connect(DB_URL).await.unwrap();

    let db = Db::builder()
        .register::<World>()
        .register::<Fortune>()
        .build(conn)
        .await
        .unwrap();

    Ok(Pool {
        db,
        rng: Default::default(),
    })
}

impl Pool {
    pub async fn get_world(&self) -> HandleResult<World> {
        let id = self.rng.borrow_mut().gen_id();
        World::get_by_id(&self.db, id).await.map_err(Into::into)
    }

    pub async fn get_worlds(&self, num: u16) -> HandleResult<Vec<World>> {
        let res = {
            let mut rng = self.rng.borrow_mut();
            core::iter::repeat_with(|| {
                let id = rng.gen_id();
                World::get_by_id(&self.db, id)
            })
            .take(num as _)
            .collect::<Vec<_>>()
        };

        let mut worlds = Vec::with_capacity(num as _);

        for fut in res {
            let world = fut.await?;
            worlds.push(world);
        }

        Ok(worlds)
    }

    pub async fn update(&self, num: u16) -> HandleResult<Vec<World>> {
        let res = {
            let mut rng = self.rng.borrow_mut();

            core::iter::repeat_with(|| {
                let id = rng.gen_id();
                let rng = rng.gen_id();

                let fut = World::get_by_id(&self.db, id);
                async move {
                    let mut world = fut.await?;
                    world.randomnumber = rng;
                    HandleResult::Ok(world)
                }
            })
            .take(num as _)
            .collect::<Vec<_>>()
        };

        let mut worlds = Vec::with_capacity(num as _);

        for fut in res {
            let mut world = fut.await?;
            let rng = world.randomnumber;
            world.update().randomnumber(rng).exec(&self.db).await?;
            worlds.push(world);
        }

        Ok(worlds)
    }

    pub async fn tell_fortune(&self) -> HandleResult<Fortunes> {
        let mut fortunes = Fortune::all().all(&self.db).await?.collect::<Vec<_>>().await?;
        Ok(Fortunes::new(fortunes))
    }
}
