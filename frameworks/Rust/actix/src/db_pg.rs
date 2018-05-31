use actix::prelude::*;
use postgres::{Connection, TlsMode};
use rand::{thread_rng, Rng, ThreadRng};
use std::io;

use models::{Fortune, World};

/// Postgres interface
pub struct PgConnection {
    conn: Connection,
    rng: ThreadRng,
}

impl Actor for PgConnection {
    type Context = SyncContext<Self>;
}

impl PgConnection {
    pub fn new(db_url: &str) -> PgConnection {
        let conn = Connection::connect(db_url, TlsMode::None)
            .expect(&format!("Error connecting to {}", db_url));
        PgConnection {
            conn,
            rng: thread_rng(),
        }
    }
}

unsafe impl Send for PgConnection {}

pub struct RandomWorld;

impl Message for RandomWorld {
    type Result = io::Result<World>;
}

impl Handler<RandomWorld> for PgConnection {
    type Result = io::Result<World>;

    fn handle(&mut self, _: RandomWorld, _: &mut Self::Context) -> Self::Result {
        let random_world = self
            .conn
            .prepare_cached("SELECT id, randomnumber FROM world WHERE id=$1")
            .unwrap();

        let random_id = self.rng.gen_range::<i32>(1, 10_000);
        let rows = &random_world.query(&[&random_id]).unwrap();
        let row = rows.get(0);
        Ok(World {
            id: row.get(0),
            randomnumber: row.get(1),
        })
    }
}

pub struct RandomWorlds(pub u16);

impl Message for RandomWorlds {
    type Result = io::Result<Vec<World>>;
}

impl Handler<RandomWorlds> for PgConnection {
    type Result = io::Result<Vec<World>>;

    fn handle(&mut self, msg: RandomWorlds, _: &mut Self::Context) -> Self::Result {
        let random_world = self
            .conn
            .prepare_cached("SELECT id, randomnumber FROM world WHERE id=$1")
            .unwrap();

        let mut worlds = Vec::with_capacity(msg.0 as usize);
        for _ in 0..msg.0 {
            let w_id: i32 = self.rng.gen_range(1, 10_000);
            let rows = &random_world.query(&[&w_id]).unwrap();
            let row = rows.get(0);
            worlds.push(World {
                id: row.get(0),
                randomnumber: row.get(1),
            });
        }
        Ok(worlds)
    }
}

pub struct UpdateWorld(pub u16);

impl Message for UpdateWorld {
    type Result = io::Result<Vec<World>>;
}

impl Handler<UpdateWorld> for PgConnection {
    type Result = io::Result<Vec<World>>;

    fn handle(&mut self, msg: UpdateWorld, _: &mut Self::Context) -> Self::Result {
        let get_world = self
            .conn
            .prepare_cached("SELECT id FROM world WHERE id=$1")
            .unwrap();
        let mut update = String::with_capacity(120 + 6 * msg.0 as usize);
        update
            .push_str("UPDATE world SET randomnumber = temp.randomnumber FROM (VALUES ");

        let mut worlds = Vec::with_capacity(msg.0 as usize);
        for _ in 0..msg.0 {
            let random_id = self.rng.gen_range::<i32>(1, 10_000);
            let rows = &get_world.query(&[&random_id]).unwrap();
            let w = World {
                id: rows.get(0).get(0),
                randomnumber: self.rng.gen_range(1, 10_000),
            };
            update.push_str(&format!("({}, {}),", w.id, w.randomnumber));
            worlds.push(w);
        }
        worlds.sort_by_key(|w| w.id);

        update.pop();
        update
            .push_str(" ORDER BY 1) AS temp(id, randomnumber) WHERE temp.id = world.id");
        self.conn.execute(&update, &[]).unwrap();

        Ok(worlds)
    }
}

pub struct TellFortune;

impl Message for TellFortune {
    type Result = io::Result<Vec<Fortune>>;
}

impl Handler<TellFortune> for PgConnection {
    type Result = io::Result<Vec<Fortune>>;

    fn handle(&mut self, _: TellFortune, _: &mut Self::Context) -> Self::Result {
        let fortune = self
            .conn
            .prepare_cached("SELECT id, message FROM fortune")
            .unwrap();

        let mut items = Vec::with_capacity(16);
        items.push(Fortune {
            id: 0,
            message: "Additional fortune added at request time.".to_string(),
        });

        for row in &fortune.query(&[])? {
            items.push(Fortune {
                id: row.get(0),
                message: row.get(1),
            });
        }
        items.sort_by(|it, next| it.message.cmp(&next.message));
        Ok(items)
    }
}
