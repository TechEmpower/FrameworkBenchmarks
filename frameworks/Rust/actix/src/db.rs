//! Db executor actor
use std::io;
use rand::{self, Rng};
use actix::prelude::*;
use diesel;
use diesel::prelude::*;

use models;


pub struct DbExecutor(PgConnection);

impl Actor for DbExecutor {
    type Context = SyncContext<Self>;
}

impl DbExecutor {
    pub fn new(db_url: &str) -> DbExecutor {
        DbExecutor(PgConnection::establish(db_url)
                   .expect(&format!("Error connecting to {}", db_url)))
    }
}

pub struct RandomWorld;

impl ResponseType for RandomWorld {
    type Item = models::World;
    type Error = io::Error;
}

impl Handler<RandomWorld> for DbExecutor {
    fn handle(&mut self, _: RandomWorld, _: &mut Self::Context) -> Response<Self, RandomWorld>
    {
        use schema::World::dsl::*;

        let random_id = rand::thread_rng().gen_range(1, 10_000);
        match World.filter(id.eq(random_id)).load::<models::World>(&self.0)
        {
            Ok(mut items) =>
                Self::reply(items.pop().unwrap()),
            Err(_) =>
                Self::reply_error(
                    io::Error::new(io::ErrorKind::Other, "Database error")),
        }
    }
}

pub struct UpdateWorld(pub Vec<models::World>);

impl ResponseType for UpdateWorld {
    type Item = ();
    type Error = io::Error;
}

impl Handler<UpdateWorld> for DbExecutor {
    fn handle(&mut self, msg: UpdateWorld, _: &mut Self::Context) -> Response<Self, UpdateWorld>
    {
        use schema::World::dsl::*;

        for world in msg.0 {
            let _ = diesel::update(World)
                .filter(id.eq(world.id))
                .set(randomnumber.eq(world.randomnumber))
                .execute(&self.0);
        }
        Self::empty()
    }
}

pub struct TellFortune;

impl ResponseType for TellFortune {
    type Item = Vec<models::Fortune>;
    type Error = io::Error;
}

impl Handler<TellFortune> for DbExecutor {
    fn handle(&mut self, _: TellFortune, _: &mut Self::Context) -> Response<Self, TellFortune>
    {
        use schema::Fortune::dsl::*;

        match Fortune.load::<models::Fortune>(&self.0) {
            Ok(mut items) => {
                items.push(models::Fortune{
                    id: 0,
                    message: "Additional fortune added at request time.".to_string()});
                items.sort_by(|it, next| it.message.cmp(&next.message));
                Self::reply(items)
            }
            Err(_) =>
                Self::reply_error(
                    io::Error::new(io::ErrorKind::Other, "Databse error"))
        }
    }
}
