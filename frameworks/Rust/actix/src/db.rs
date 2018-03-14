//! Db executor actor
use std::io;
use rand::{thread_rng, Rng, ThreadRng};
use actix::prelude::*;
use diesel;
use diesel::prelude::*;

use models;


pub struct DbExecutor{
    conn: PgConnection,
    rng: ThreadRng
}

unsafe impl Send for DbExecutor {}

impl Actor for DbExecutor {
    type Context = SyncContext<Self>;
}

impl DbExecutor {
    pub fn new(db_url: &str) -> DbExecutor {
        DbExecutor{
            conn: PgConnection::establish(db_url)
                .expect(&format!("Error connecting to {}", db_url)),
            rng: thread_rng()}
    }
}

pub struct RandomWorld;

impl Message for RandomWorld {
    type Result = io::Result<models::World>;
}

impl Handler<RandomWorld> for DbExecutor {
    type Result = io::Result<models::World>;

    fn handle(&mut self, _: RandomWorld, _: &mut Self::Context) -> Self::Result {
        use schema::World::dsl::*;

        let random_id = self.rng.gen_range(1, 10_000);
        match World.filter(id.eq(random_id)).load::<models::World>(&self.conn) {
            Ok(mut items) =>
                Ok(items.pop().unwrap()),
            Err(_) =>
                Err(io::Error::new(io::ErrorKind::Other, "Database error")),
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
        use schema::World::dsl::*;

        let mut worlds = Vec::with_capacity(msg.0 as usize);
        for _ in 0..msg.0 {
            let w_id = self.rng.gen_range(1, 10_000);
            let world = match World.filter(id.eq(w_id)).load::<models::World>(&self.conn) {
                Ok(mut items) => items.pop().unwrap(),
                Err(_) => return Err(io::Error::new(io::ErrorKind::Other, "Database error")),
            };
            worlds.push(world)
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
        use schema::World::dsl::*;

        let mut worlds = Vec::with_capacity(msg.0);
        for _ in 0..msg.0 {
            let w_id = self.rng.gen_range::<i32>(1, 10_000);
            let mut world = match World.filter(id.eq(w_id)).load::<models::World>(&self.conn) {
                Ok(mut items) => items.pop().unwrap(),
                Err(_) => return Err(io::Error::new(io::ErrorKind::Other, "Database error")),
            };

            world.randomnumber = self.rng.gen_range(1, 10_000);
            let _ = diesel::update(World)
                .filter(id.eq(world.id))
                .set(randomnumber.eq(world.randomnumber))
                .execute(&self.conn);

            worlds.push(world);
        }
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
        use schema::Fortune::dsl::*;

        match Fortune.load::<models::Fortune>(&self.conn) {
            Ok(mut items) => {
                items.push(models::Fortune{
                    id: 0,
                    message: "Additional fortune added at request time.".to_string()});
                items.sort_by(|it, next| it.message.cmp(&next.message));
                Ok(items)
            }
            Err(e) =>
                Err(io::Error::new(io::ErrorKind::Other, e))
        }
    }
}
