use std::fmt::Write;
use std::io;

use actix_http::Error;
use bytes::{Bytes, BytesMut};
use futures::stream::futures_unordered::FuturesUnordered;
use futures::{Future, FutureExt, StreamExt, TryStreamExt};
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
    pub async fn connect(db_url: &str) -> PgConnection {
        let (cl, conn) = connect(db_url, NoTls)
            .await
            .expect("can not connect to postgresql");

        actix_rt::spawn(conn.map(|res| panic!("{:?}", res)));

        let world = cl.prepare("SELECT id, message FROM fortune").await.unwrap();
        let fortune = cl
            .prepare("SELECT id, randomnumber FROM world WHERE id=$1")
            .await
            .unwrap();

        PgConnection {
            cl,
            fortune,
            world,
            rng: thread_rng(),
        }
    }
}

impl PgConnection {
    pub fn get_world(&mut self) -> impl Future<Output = Result<Bytes, Error>> {
        let random_id = self.rng.gen_range::<i32>(1, 10_001);
        let fut = self.cl.query_one(&self.world, &[&random_id]);

        async move {
            let row = fut.await.map_err(|e| {
                Error::from(io::Error::new(io::ErrorKind::Other, format!("{:?}", e)))
            })?;

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
        }
    }

    pub fn get_worlds(
        &mut self,
        num: usize,
    ) -> impl Future<Output = Result<Vec<World>, io::Error>> {
        let worlds = FuturesUnordered::new();
        for _ in 0..num {
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

        worlds.try_collect()
    }

    pub fn update(
        &mut self,
        num: usize,
    ) -> impl Future<Output = Result<Vec<World>, io::Error>> {
        let worlds = FuturesUnordered::new();
        for _ in 0..num {
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
        async move {
            let worlds: Vec<World> = worlds.try_collect().await?;

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
                .await
                .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("{:?}", e)))?;

            Ok(worlds)
        }
    }

    pub fn tell_fortune(
        &mut self,
    ) -> impl Future<Output = Result<Vec<Fortune>, io::Error>> {
        let mut items = vec![Fortune {
            id: 0,
            message: "Additional fortune added at request time.".to_string(),
        }];

        let fut = self.cl.query_raw(&self.fortune, &[]);

        async move {
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
        }
    }
}
