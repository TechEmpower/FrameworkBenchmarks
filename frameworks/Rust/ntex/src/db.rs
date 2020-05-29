use std::cell::RefCell;
use std::fmt::Write;
use std::io;

use bytes::{Bytes, BytesMut};
use futures::stream::futures_unordered::FuturesUnordered;
use futures::{Future, FutureExt, StreamExt, TryStreamExt};
use fxhash::FxHashMap;
use ntex::web::Error;
use random_fast_rng::{FastRng, Random};
use simd_json_derive::Serialize;
use tokio_postgres::types::ToSql;
use tokio_postgres::{connect, Client, NoTls, Statement};

use crate::utils::{Fortune, Writer};

#[derive(Serialize, Debug)]
pub struct World {
    pub id: i32,
    pub randomnumber: i32,
}

/// Postgres interface
pub struct PgConnection {
    cl: Client,
    fortune: Statement,
    world: Statement,
    rng: RefCell<FastRng>,
    updates: FxHashMap<u16, Statement>,
}

impl PgConnection {
    pub async fn connect(db_url: &str) -> PgConnection {
        let (cl, conn) = connect(db_url, NoTls)
            .await
            .expect("can not connect to postgresql");
        ntex::rt::spawn(conn.map(|_| ()));

        let fortune = cl.prepare("SELECT * FROM fortune").await.unwrap();
        let mut updates = FxHashMap::default();
        for num in 1..=500u16 {
            let mut pl: u16 = 1;
            let mut q = String::new();
            q.push_str("UPDATE world SET randomnumber = CASE id ");
            for _ in 1..=num {
                let _ = write!(&mut q, "when ${} then ${} ", pl, pl + 1);
                pl += 2;
            }
            q.push_str("ELSE randomnumber END WHERE id IN (");
            for _ in 1..=num {
                let _ = write!(&mut q, "${},", pl);
                pl += 1;
            }
            q.pop();
            q.push(')');
            updates.insert(num, cl.prepare(&q).await.unwrap());
        }
        let world = cl.prepare("SELECT * FROM world WHERE id=$1").await.unwrap();

        PgConnection {
            cl,
            fortune,
            world,
            updates,
            rng: RefCell::new(FastRng::new()),
        }
    }
}

impl PgConnection {
    pub fn get_world(&self) -> impl Future<Output = Result<Bytes, Error>> {
        let random_id = (self.rng.borrow_mut().get_u32() % 10_000 + 1) as i32;
        let fut = self.cl.query_one(&self.world, &[&random_id]);

        async move {
            let row = fut.await.map_err(|e| {
                Error::from(io::Error::new(io::ErrorKind::Other, format!("{:?}", e)))
            })?;

            let mut body = BytesMut::with_capacity(40);
            World {
                id: row.get(0),
                randomnumber: row.get(1),
            }
            .json_write(&mut Writer(&mut body))
            .unwrap();

            Ok(body.freeze())
        }
    }

    pub fn get_worlds(
        &self,
        num: usize,
    ) -> impl Future<Output = Result<Vec<World>, io::Error>> {
        let worlds = FuturesUnordered::new();
        let mut rng = self.rng.borrow_mut();
        for _ in 0..num {
            let w_id = (rng.get_u32() % 10_000 + 1) as i32;
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
        &self,
        num: u16,
    ) -> impl Future<Output = Result<Vec<World>, io::Error>> {
        let worlds = FuturesUnordered::new();
        let mut rng = self.rng.borrow_mut();
        for _ in 0..num {
            let id = (rng.get_u32() % 10_000 + 1) as i32;
            let w_id = (rng.get_u32() % 10_000 + 1) as i32;
            worlds.push(self.cl.query_one(&self.world, &[&w_id]).map(
                move |res| match res {
                    Err(e) => {
                        Err(io::Error::new(io::ErrorKind::Other, format!("{:?}", e)))
                    }
                    Ok(row) => {
                        let world = World {
                            id: row.get(0),
                            randomnumber: id,
                        };
                        Ok(world)
                    }
                },
            ));
        }

        let cl = self.cl.clone();
        let st = self.updates.get(&num).unwrap().clone();
        async move {
            let worlds: Vec<World> = worlds.try_collect().await?;

            let mut params: Vec<&dyn ToSql> = Vec::with_capacity(num as usize * 3);
            for w in &worlds {
                params.push(&w.id);
                params.push(&w.randomnumber);
            }
            for w in &worlds {
                params.push(&w.id);
            }

            cl.query(&st, &params)
                .await
                .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("{:?}", e)))?;

            Ok(worlds)
        }
    }

    pub fn tell_fortune(&self) -> impl Future<Output = Result<Vec<Fortune>, io::Error>> {
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
