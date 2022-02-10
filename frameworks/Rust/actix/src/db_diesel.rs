//! Diesel DB methods.

use std::io;

use actix::prelude::*;
use diesel::{prelude::*, result::Error};
use rand::{rngs::SmallRng, Rng, SeedableRng};

use crate::models;

pub struct DbExecutor {
    conn: PgConnection,
    rng: SmallRng,
}

impl DbExecutor {
    pub fn new(db_url: &str) -> DbExecutor {
        DbExecutor {
            conn: PgConnection::establish(db_url)
                .unwrap_or_else(|_| panic!("Error connecting to {}", db_url)),
            rng: SmallRng::from_entropy(),
        }
    }
}

impl Actor for DbExecutor {
    type Context = SyncContext<Self>;
}

pub struct RandomWorld;

impl Message for RandomWorld {
    type Result = io::Result<models::World>;
}

impl Handler<RandomWorld> for DbExecutor {
    type Result = io::Result<models::World>;

    fn handle(&mut self, _: RandomWorld, _: &mut Self::Context) -> Self::Result {
        use crate::schema::world::dsl::*;

        let random_id = self.rng.gen_range(1..10_001);
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
        use crate::schema::world::dsl::*;

        let mut worlds = Vec::with_capacity(msg.0 as usize);
        for _ in 0..msg.0 {
            let w_id = self.rng.gen_range(1..10_001);
            let w = match world.filter(id.eq(w_id)).load::<models::World>(&self.conn) {
                Ok(mut items) => items.pop().unwrap(),
                Err(_) => {
                    return Err(io::Error::new(io::ErrorKind::Other, "Database error"));
                }
            };
            worlds.push(w)
        }
        Ok(worlds)
    }
}

pub struct UpdateWorld(pub u16);

impl Message for UpdateWorld {
    type Result = io::Result<Vec<models::World>>;
}

impl Handler<UpdateWorld> for DbExecutor {
    type Result = io::Result<Vec<models::World>>;

    fn handle(&mut self, msg: UpdateWorld, _: &mut Self::Context) -> Self::Result {
        use crate::schema::world::dsl::*;

        let mut worlds = Vec::with_capacity(msg.0 as usize);
        for _ in 0..msg.0 {
            let w_id: i32 = self.rng.gen_range(1..10_001);
            let mut w = match world.filter(id.eq(w_id)).load::<models::World>(&self.conn) {
                Ok(mut items) => items.pop().unwrap(),
                Err(_) => {
                    return Err(io::Error::new(io::ErrorKind::Other, "Database error"));
                }
            };
            w.randomnumber = self.rng.gen_range(1..10_001);
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
        use crate::schema::fortune::dsl::*;

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
