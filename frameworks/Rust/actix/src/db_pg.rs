use std::fmt::Write;
use std::io;

use actix::prelude::*;
use bytes::{Bytes, BytesMut};
use futures::stream::futures_unordered::FuturesUnordered;
use futures::{FutureExt, StreamExt, TryStreamExt};
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

impl Actor for PgConnection {
    type Context = Context<Self>;
}

impl PgConnection {
    pub async fn connect(db_url: &str) -> Result<Addr<PgConnection>, io::Error> {
        let (cl, conn) = connect(db_url, NoTls)
            .await
            .expect("can not connect to postgresql");
        actix_rt::spawn(conn.map(|res| panic!("{:?}", res)));

        let fortune = cl.prepare("SELECT id, message FROM fortune").await.unwrap();
        let world = cl
            .prepare("SELECT id, randomnumber FROM world WHERE id=$1")
            .await
            .unwrap();

        Ok(PgConnection::create(move |_| PgConnection {
            cl,
            fortune,
            world,
            rng: thread_rng(),
        }))
    }
}

pub struct RandomWorld;

impl Message for RandomWorld {
    type Result = io::Result<Bytes>;
}

impl Handler<RandomWorld> for PgConnection {
    type Result = ResponseFuture<Result<Bytes, io::Error>>;

    fn handle(&mut self, _: RandomWorld, _: &mut Self::Context) -> Self::Result {
        let random_id = self.rng.gen_range::<i32>(1, 10_001);
        let fut = self.cl.query_one(&self.world, &[&random_id]);

        Box::pin(async move {
            let row = fut
                .await
                .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("{:?}", e)))?;

            let mut body = BytesMut::with_capacity(33);
            serde_json::to_writer(
                Writer(&mut body),
                &World {
                    id: row.get(0),
                    randomnumber: row.get(1),
                },
            )
            .unwrap();

            Ok(body.freeze())
        })
    }
}

pub struct RandomWorlds(pub u16);

impl Message for RandomWorlds {
    type Result = io::Result<Vec<World>>;
}

impl Handler<RandomWorlds> for PgConnection {
    type Result = ResponseFuture<Result<Vec<World>, io::Error>>;

    fn handle(&mut self, msg: RandomWorlds, _: &mut Self::Context) -> Self::Result {
        let worlds = FuturesUnordered::new();
        for _ in 0..msg.0 {
            let w_id: i32 = self.rng.gen_range(1, 10_001);
            worlds.push(
                self.cl
                    .query_one(&self.world, &[&w_id])
                    .map(|res| match res {
                        Err(e) => {
                            Err(io::Error::new(io::ErrorKind::Other, format!("{:?}", e)))
                        }
                        Ok(row) => Ok(World {
                            id: row.get(0),
                            randomnumber: row.get(1),
                        }),
                    }),
            );
        }

        Box::pin(worlds.try_collect())
    }
}

pub struct UpdateWorld(pub u16);

impl Message for UpdateWorld {
    type Result = io::Result<Vec<World>>;
}

impl Handler<UpdateWorld> for PgConnection {
    type Result = ResponseFuture<Result<Vec<World>, io::Error>>;

    fn handle(&mut self, msg: UpdateWorld, _: &mut Self::Context) -> Self::Result {
        let worlds = FuturesUnordered::new();
        for _ in 0..msg.0 {
            let id: i32 = self.rng.gen_range(1, 10_001);
            let w_id: i32 = self.rng.gen_range(1, 10_001);
            worlds.push(self.cl.query_one(&self.world, &[&w_id]).map(
                move |res| match res {
                    Err(e) => {
                        Err(io::Error::new(io::ErrorKind::Other, format!("{:?}", e)))
                    }
                    Ok(row) => {
                        let mut world = World {
                            id: row.get(0),
                            randomnumber: row.get(1),
                        };
                        world.randomnumber = id;
                        Ok(world)
                    }
                },
            ));
        }

        let cl = self.cl.clone();
        Box::pin(async move {
            let worlds: Vec<World> = worlds.try_collect().await?;

            let mut update = String::with_capacity(120 + 6 * msg.0 as usize);
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
                .await
                .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("{:?}", e)))?;

            Ok(worlds)
        })
    }
}

pub struct TellFortune;

impl Message for TellFortune {
    type Result = io::Result<Vec<Fortune>>;
}

impl Handler<TellFortune> for PgConnection {
    type Result = ResponseFuture<Result<Vec<Fortune>, io::Error>>;

    fn handle(&mut self, _: TellFortune, _: &mut Self::Context) -> Self::Result {
        let mut items = vec![Fortune {
            id: 0,
            message: "Additional fortune added at request time.".to_string(),
        }];
        let fut = self.cl.query_raw(&self.fortune, &[]);

        Box::pin(async move {
            let mut stream = fut
                .await
                .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("{:?}", e)))?;

            while let Some(row) = stream.next().await {
                let row = row.map_err(|e| {
                    io::Error::new(io::ErrorKind::Other, format!("{:?}", e))
                })?;
                items.push(Fortune {
                    id: row.get(0),
                    message: row.get(1),
                });
            }

            items.sort_by(|it, next| it.message.cmp(&next.message));
            Ok(items)
        })
    }
}
