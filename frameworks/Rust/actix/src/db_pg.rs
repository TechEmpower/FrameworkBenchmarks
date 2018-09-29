use std::io;

use actix::fut;
use actix::prelude::*;
use futures::{stream, Future, Stream};
use rand::{thread_rng, Rng, ThreadRng};
use tokio_postgres::{connect, Client, Statement, TlsMode};

use models::{Fortune, World};

/// Postgres interface
pub struct PgConnection {
    cl: Option<Client>,
    fortune: Option<Statement>,
    world: Option<Statement>,
    update: Option<Statement>,
    rng: ThreadRng,
}

impl Actor for PgConnection {
    type Context = Context<Self>;
}

impl PgConnection {
    pub fn connect(db_url: &str) -> Addr<PgConnection> {
        let hs = connect(db_url.parse().unwrap(), TlsMode::None);

        PgConnection::create(move |ctx| {
            let act = PgConnection {
                cl: None,
                fortune: None,
                world: None,
                update: None,
                rng: thread_rng(),
            };

            hs.map_err(|_| panic!("can not connect to postgresql"))
                .into_actor(&act)
                .and_then(|(cl, conn), act, ctx| {
                    ctx.wait(
                        cl.prepare("SELECT id, message FROM fortune")
                            .map_err(|_| ())
                            .into_actor(act)
                            .and_then(|st, act, _| {
                                act.fortune = Some(st);
                                fut::ok(())
                            }),
                    );
                    ctx.wait(
                        cl.prepare("SELECT id, randomnumber FROM world WHERE id=$1")
                            .map_err(|_| ())
                            .into_actor(act)
                            .and_then(|st, act, _| {
                                act.world = Some(st);
                                fut::ok(())
                            }),
                    );
                    ctx.wait(
                        cl.prepare("SELECT id FROM world WHERE id=$1")
                            .map_err(|_| ())
                            .into_actor(act)
                            .and_then(|st, act, _| {
                                act.update = Some(st);
                                fut::ok(())
                            }),
                    );

                    act.cl = Some(cl);
                    Arbiter::spawn(conn.map_err(|e| panic!("{}", e)));
                    fut::ok(())
                }).wait(ctx);

            act
        })
    }
}

pub struct RandomWorld;

impl Message for RandomWorld {
    type Result = io::Result<World>;
}

impl Handler<RandomWorld> for PgConnection {
    type Result = ResponseFuture<World, io::Error>;

    fn handle(&mut self, _: RandomWorld, _: &mut Self::Context) -> Self::Result {
        let random_id = self.rng.gen_range::<i32>(1, 10_001);

        Box::new(
            self.cl
                .as_mut()
                .unwrap()
                .query(self.world.as_ref().unwrap(), &[&random_id])
                .into_future()
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e.0))
                .and_then(|(row, _)| {
                    let row = row.unwrap();
                    Ok(World {
                        id: row.get(0),
                        randomnumber: row.get(1),
                    })
                }),
        )
    }
}

pub struct RandomWorlds(pub u16);

impl Message for RandomWorlds {
    type Result = io::Result<Vec<World>>;
}

impl Handler<RandomWorlds> for PgConnection {
    type Result = ResponseFuture<Vec<World>, io::Error>;

    fn handle(&mut self, msg: RandomWorlds, _: &mut Self::Context) -> Self::Result {
        let mut worlds = Vec::with_capacity(msg.0 as usize);
        for _ in 0..msg.0 {
            let w_id: i32 = self.rng.gen_range(1, 10_001);
            worlds.push(
                self.cl
                    .as_mut()
                    .unwrap()
                    .query(self.world.as_ref().unwrap(), &[&w_id])
                    .into_future()
                    .map_err(|e| io::Error::new(io::ErrorKind::Other, e.0))
                    .and_then(|(row, _)| {
                        let row = row.unwrap();
                        Ok(World {
                            id: row.get(0),
                            randomnumber: row.get(1),
                        })
                    }),
            );
        }

        Box::new(stream::futures_unordered(worlds).collect())
    }
}

pub struct UpdateWorld(pub u16);

impl Message for UpdateWorld {
    type Result = io::Result<Vec<World>>;
}

impl Handler<UpdateWorld> for PgConnection {
    type Result = ResponseActFuture<Self, Vec<World>, io::Error>;

    fn handle(&mut self, msg: UpdateWorld, _: &mut Self::Context) -> Self::Result {
        let mut worlds = Vec::with_capacity(msg.0 as usize);
        for _ in 0..msg.0 {
            let id: i32 = self.rng.gen_range(1, 10_001);
            let w_id: i32 = self.rng.gen_range(1, 10_001);
            worlds.push(
                self.cl
                    .as_mut()
                    .unwrap()
                    .query(self.update.as_ref().unwrap(), &[&w_id])
                    .into_future()
                    .map_err(|e| io::Error::new(io::ErrorKind::Other, e.0))
                    .and_then(move |(row, _)| {
                        let row = row.unwrap();
                        Ok(World {
                            id: row.get(0),
                            randomnumber: id,
                        })
                    }),
            );
        }

        Box::new(
            stream::futures_unordered(worlds)
                .collect()
                .into_actor(self)
                .and_then(move |mut worlds, act, _| {
                    let mut update = String::with_capacity(120 + 6 * msg.0 as usize);
                    update
                        .push_str("UPDATE world SET randomnumber = temp.randomnumber FROM (VALUES ");

                    for w in &worlds {
                        update.push_str(&format!("({}, {}),", w.id, w.randomnumber));
                    }
                    worlds.sort_by_key(|w| w.id);

                    update.pop();
                    update
                        .push_str(" ORDER BY 1) AS temp(id, randomnumber) WHERE temp.id = world.id");

                    act.cl
                        .as_mut()
                        .unwrap()
                        .batch_execute(&update)
                        .map_err(|e| io::Error::new(io::ErrorKind::Other, e))
                        .into_actor(act)
                        .and_then(|_, _, _| fut::ok(worlds))
                }),
        )
    }
}

pub struct TellFortune;

impl Message for TellFortune {
    type Result = io::Result<Vec<Fortune>>;
}

impl Handler<TellFortune> for PgConnection {
    type Result = ResponseFuture<Vec<Fortune>, io::Error>;

    fn handle(&mut self, _: TellFortune, _: &mut Self::Context) -> Self::Result {
        let mut items = Vec::with_capacity(16);
        items.push(Fortune {
            id: 0,
            message: "Additional fortune added at request time.".to_string(),
        });

        Box::new(
            self.cl
                .as_mut()
                .unwrap()
                .query(self.fortune.as_ref().unwrap(), &[])
                .fold(items, move |mut items, row| {
                    items.push(Fortune {
                        id: row.get(0),
                        message: row.get(1),
                    });
                    Ok::<_, io::Error>(items)
                }).map_err(|e| io::Error::new(io::ErrorKind::Other, e))
                .and_then(|mut items| {
                    items.sort_by(|it, next| it.message.cmp(&next.message));
                    Ok(items)
                }),
        )
    }
}
