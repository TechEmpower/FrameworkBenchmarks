use std::fmt::Write;
use std::io;

use actix_http::Error;
use bytes::{Bytes, BytesMut};
use futures::future::join_all;
use futures::{stream, Future, Stream};
use rand::{thread_rng, Rng, ThreadRng};
use tokio_postgres::{connect, Client, NoTls, Statement};

use crate::models::World;
use crate::utils::{Fortune, Writer};

/// Postgres interface
pub struct PgConnection {
    cl: Client,
    fortune: Statement,
    world: Statement,
    rng: ThreadRng,
}

impl PgConnection {
    pub fn connect(db_url: &str) -> impl Future<Item = PgConnection, Error = ()> {
        let hs = connect(db_url, NoTls);

        hs.map_err(|_| panic!("can not connect to postgresql"))
            .and_then(|(mut cl, conn)| {
                actix_rt::spawn(conn.map_err(|e| panic!("{}", e)));

                join_all(vec![
                    cl.prepare("SELECT id, message FROM fortune"),
                    cl.prepare("SELECT id, randomnumber FROM world WHERE id=$1"),
                ])
                .map_err(|_| ())
                .map(move |mut st| {
                    let world = st.pop().unwrap();
                    let fortune = st.pop().unwrap();
                    PgConnection {
                        cl,
                        fortune,
                        world,
                        rng: thread_rng(),
                    }
                })
            })
    }
}

impl PgConnection {
    pub fn get_world(&mut self) -> impl Future<Item = Bytes, Error = Error> {
        let random_id = self.rng.gen_range::<i32>(1, 10_001);

        self.cl
            .query(&self.world, &[&random_id])
            .into_future()
            .map_err(|e| {
                Error::from(io::Error::new(io::ErrorKind::Other, format!("{:?}", e.0)))
            })
            .map(|(row, _)| {
                let row = row.unwrap();
                let mut body = BytesMut::with_capacity(33);
                serde_json::to_writer(
                    Writer(&mut body),
                    &World {
                        id: row.get(0),
                        randomnumber: row.get(1),
                    },
                )
                .unwrap();
                body.freeze()
            })
    }

    pub fn get_worlds(
        &mut self,
        num: usize,
    ) -> impl Future<Item = Vec<World>, Error = io::Error> {
        let mut worlds = Vec::with_capacity(num);
        for _ in 0..num {
            let w_id: i32 = self.rng.gen_range(1, 10_001);
            worlds.push(
                self.cl
                    .query(&self.world, &[&w_id])
                    .into_future()
                    .map_err(|e| {
                        io::Error::new(io::ErrorKind::Other, format!("{:?}", e.0))
                    })
                    .map(|(row, _)| {
                        let row = row.unwrap();
                        World {
                            id: row.get(0),
                            randomnumber: row.get(1),
                        }
                    }),
            );
        }

        stream::futures_unordered(worlds).collect()
    }

    pub fn update(
        &mut self,
        num: usize,
    ) -> impl Future<Item = Vec<World>, Error = io::Error> {
        let mut worlds = Vec::with_capacity(num);
        for _ in 0..num {
            let id: i32 = self.rng.gen_range(1, 10_001);
            let w_id: i32 = self.rng.gen_range(1, 10_001);
            worlds.push(
                self.cl
                    .query(&self.world, &[&w_id])
                    .into_future()
                    .map_err(|e| {
                        io::Error::new(io::ErrorKind::Other, format!("{:?}", e.0))
                    })
                    .map(move |(row, _)| {
                        let row = row.unwrap();
                        let mut world = World {
                            id: row.get(0),
                            randomnumber: row.get(1),
                        };
                        world.randomnumber = id;
                        world
                    }),
            );
        }

        let mut cl = self.cl.clone();
        let mut rng = self.rng.clone();
        stream::futures_unordered(worlds)
            .collect()
            .and_then(move |worlds| {
                let mut update = String::with_capacity(120 + 6 * num as usize);
                update.push_str(
                    "UPDATE world SET randomnumber = temp.randomnumber FROM (VALUES ",
                );

                for w in &worlds {
                    let _ = write!(&mut update, "({}, {}),", w.id, w.randomnumber);
                }
                update.pop();
                update.push_str(
                    " ORDER BY 1) AS temp(id, randomnumber) WHERE temp.id = world.id",
                );

                cl.simple_query(&update)
                    .collect()
                    .map_err(|e| {
                        io::Error::new(io::ErrorKind::Other, format!("{:?}", e))
                    })
                    .map(|_| worlds)
            })
    }

    pub fn tell_fortune(
        &mut self,
    ) -> impl Future<Item = Vec<Fortune>, Error = io::Error> {
        let items = vec![Fortune {
            id: 0,
            message: "Additional fortune added at request time.".to_string(),
        }];

        self.cl
            .query(&self.fortune, &[])
            .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("{:?}", e)))
            .fold(items, move |mut items, row| {
                items.push(Fortune {
                    id: row.get(0),
                    message: row.get(1),
                });
                Ok::<_, io::Error>(items)
            })
            .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("{:?}", e)))
            .map(|mut items| {
                items.sort_by(|it, next| it.message.cmp(&next.message));
                items
            })
    }
}
