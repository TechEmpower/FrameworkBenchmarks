//! Db executor actor
use actix::prelude::*;
use diesel;
use diesel::prelude::*;
use diesel::result::Error;
use rand::{thread_rng, Rng, ThreadRng};
use std::io;

use models;

pub struct DbExecutor {
    conn: PgConnection,
    rng: ThreadRng,
}

unsafe impl Send for DbExecutor {}

impl Actor for DbExecutor {
    type Context = SyncContext<Self>;
}

impl DbExecutor {
    pub fn new(db_url: &str) -> DbExecutor {
        DbExecutor {
            conn: PgConnection::establish(db_url)
                .expect(&format!("Error connecting to {}", db_url)),
            rng: thread_rng(),
        }
    }
}

pub struct RandomWorld;

impl Message for RandomWorld {
    type Result = io::Result<models::World>;
}

impl Handler<RandomWorld> for DbExecutor {
    type Result = io::Result<models::World>;

    fn handle(&mut self, _: RandomWorld, _: &mut Self::Context) -> Self::Result {
        use schema::world::dsl::*;

        let random_id = self.rng.gen_range(1, 10_000);
        match world
            .filter(id.eq(random_id))
            .load::<models::World>(&self.conn)
        {
            Ok(mut items) => Ok(items.pop().unwrap()),
            Err(_) => Err(io::Error::new(io::ErrorKind::Other, "Database error")),
        }
    }
}

pub struct RandomWorlds(pub u16);

impl Message for RandomWorlds {
    type Result = io::Result<Vec<models::World>>;
}

impl Handler<RandomWorlds> for DbExecutor {
    type Result = io::Result<Vec<models::World>>;

    fn handle(&mut self, msg: RandomWorlds, _: &mut Self::Context) -> Self::Result {
        use schema::world::dsl::*;

        let mut worlds = Vec::with_capacity(msg.0 as usize);
        for _ in 0..msg.0 {
            let w_id = self.rng.gen_range(1, 10_000);
            let w = match world.filter(id.eq(w_id)).load::<models::World>(&self.conn) {
                Ok(mut items) => items.pop().unwrap(),
                Err(_) => {
                    return Err(io::Error::new(io::ErrorKind::Other, "Database error"))
                }
            };
            worlds.push(w)
        }
        Ok(worlds)
    }
}

pub struct UpdateWorld(pub usize);

impl Message for UpdateWorld {
    type Result = io::Result<Vec<models::World>>;
}

impl Handler<UpdateWorld> for DbExecutor {
    type Result = io::Result<Vec<models::World>>;

    fn handle(&mut self, msg: UpdateWorld, _: &mut Self::Context) -> Self::Result {
        use schema::world::dsl::*;

        let mut worlds = Vec::with_capacity(msg.0);
        for _ in 0..msg.0 {
            let w_id = self.rng.gen_range::<i32>(1, 10_000);
            let mut w = match world.filter(id.eq(w_id)).load::<models::World>(&self.conn)
            {
                Ok(mut items) => items.pop().unwrap(),
                Err(_) => {
                    return Err(io::Error::new(io::ErrorKind::Other, "Database error"))
                }
            };
            w.randomnumber = self.rng.gen_range(1, 10_000);
            worlds.push(w);
        }
        worlds.sort_by_key(|w| w.id);

        let _ = self.conn.transaction::<(), Error, _>(|| {
            for w in &worlds {
                let _ = diesel::update(world)
                    .filter(id.eq(w.id))
                    .set(randomnumber.eq(w.randomnumber))
                    .execute(&self.conn);
            }
            Ok(())
        });

        Ok(worlds)
    }
}

pub struct TellFortune;

impl Message for TellFortune {
    type Result = io::Result<Vec<models::Fortune>>;
}

impl Handler<TellFortune> for DbExecutor {
    type Result = io::Result<Vec<models::Fortune>>;

    fn handle(&mut self, _: TellFortune, _: &mut Self::Context) -> Self::Result {
        use schema::fortune::dsl::*;

        match fortune.load::<models::Fortune>(&self.conn) {
            Ok(mut items) => {
                items.push(models::Fortune {
                    id: 0,
                    message: "Additional fortune added at request time.".to_string(),
                });
                items.sort_by(|it, next| it.message.cmp(&next.message));
                Ok(items)
            }
            Err(e) => Err(io::Error::new(io::ErrorKind::Other, e)),
        }
    }
}
