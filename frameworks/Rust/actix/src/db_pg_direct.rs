use std::io;

use actix::prelude::*;
use futures::{stream, Future, Stream};
use rand::{thread_rng, Rng};
use tokio_postgres::{connect, Client, Statement, TlsMode};

use models::{Fortune, World};

/// Postgres interface
pub struct PgConnection {
    cl: Client,
    fortune: Statement,
    world: Statement,
    update: Statement,
}

impl PgConnection {
    pub fn connect(db_url: &str) -> impl Future<Item = PgConnection, Error = ()> {
        let hs = connect(db_url.parse().unwrap(), TlsMode::None);

        hs.map_err(|_| panic!("can not connect to postgresql"))
            .and_then(|(cl, conn)| {
                Arbiter::spawn(conn.map_err(|e| panic!("{}", e)));
                cl.prepare("SELECT id, message FROM fortune")
                    .map_err(|_| ())
                    .and_then(move |fortune| {
                        cl.prepare("SELECT id, randomnumber FROM world WHERE id=$1")
                            .map_err(|_| ())
                            .and_then(move |world| {
                                let st = (fortune, world);
                                cl.prepare("SELECT id FROM world WHERE id=$1")
                                    .map_err(|_| ())
                                    .and_then(|update| {
                                        Ok(PgConnection {
                                            cl,
                                            fortune: st.0,
                                            world: st.1,
                                            update,
                                        })
                                    })
                            })
                    })
            })
    }
}

impl PgConnection {
    pub fn get_world(&self) -> impl Future<Item = World, Error = io::Error> {
        let random_id = thread_rng().gen_range::<i32>(1, 10_001);

        self.cl
            .query(&self.world, &[&random_id])
            .into_future()
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.0))
            .and_then(|(row, _)| {
                let row = row.unwrap();
                Ok(World {
                    id: row.get(0),
                    randomnumber: row.get(1),
                })
            })
    }

    pub fn get_worlds(
        &self, num: usize,
    ) -> impl Future<Item = Vec<World>, Error = io::Error> {
        let mut worlds = Vec::with_capacity(num);
        for _ in 0..num {
            let w_id: i32 = thread_rng().gen_range(1, 10_001);
            worlds.push(
                self.cl
                    .query(&self.world, &[&w_id])
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

        stream::futures_unordered(worlds).collect()
    }

    pub fn update(
        &self, num: usize,
    ) -> impl Future<Item = Vec<World>, Error = io::Error> {
        let mut worlds = Vec::with_capacity(num);
        for _ in 0..num {
            let id: i32 = thread_rng().gen_range(1, 10_001);
            let w_id: i32 = thread_rng().gen_range(1, 10_001);
            worlds.push(
                self.cl
                    .query(&self.update, &[&w_id])
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

        let cl = self.cl.clone();
        stream::futures_unordered(worlds)
            .collect()
            .and_then(move |mut worlds| {
                let mut update = String::with_capacity(120 + 6 * num as usize);
                update.push_str(
                    "UPDATE world SET randomnumber = temp.randomnumber FROM (VALUES ",
                );

                for w in &worlds {
                    update.push_str(&format!("({}, {}),", w.id, w.randomnumber));
                }
                worlds.sort_by_key(|w| w.id);

                update.pop();
                update.push_str(
                    " ORDER BY 1) AS temp(id, randomnumber) WHERE temp.id = world.id",
                );

                cl.batch_execute(&update)
                    .map_err(|e| io::Error::new(io::ErrorKind::Other, e))
                    .and_then(|_| Ok(worlds))
            })
    }

    pub fn tell_fortune(&self) -> impl Future<Item = Vec<Fortune>, Error = io::Error> {
        let mut items = Vec::new();
        items.push(Fortune {
            id: 0,
            message: "Additional fortune added at request time.".to_string(),
        });

        self.cl
            .query(&self.fortune, &[])
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
            })
    }
}
