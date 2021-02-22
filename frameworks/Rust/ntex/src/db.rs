use std::{borrow::Cow, cell::RefCell, fmt::Write as FmtWrite};

use bytes::{Bytes, BytesMut};
use futures::{stream::futures_unordered::FuturesUnordered, Future, FutureExt, StreamExt};
use random_fast_rng::{FastRng, Random};
use smallvec::SmallVec;
use tokio_postgres::types::ToSql;
use tokio_postgres::{connect, Client, NoTls, Statement};
use yarte::{ywrite_html, Serialize};

#[derive(Copy, Clone, Serialize, Debug)]
pub struct World {
    pub id: i32,
    pub randomnumber: i32,
}

#[derive(serde::Serialize, Debug)]
pub struct Fortune {
    pub id: i32,
    pub message: Cow<'static, str>,
}

/// Postgres interface
pub struct PgConnection {
    cl: Client,
    fortune: Statement,
    world: Statement,
    rng: RefCell<FastRng>,
    updates: Vec<Statement>,
}

impl PgConnection {
    pub async fn connect(db_url: &str) -> PgConnection {
        let (cl, conn) = connect(db_url, NoTls)
            .await
            .expect("can not connect to postgresql");
        ntex::rt::spawn(conn.map(|_| ()));

        let fortune = cl.prepare("SELECT * FROM fortune").await.unwrap();
        let mut updates = Vec::new();
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
            updates.push(cl.prepare(&q).await.unwrap());
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
    pub fn get_world(&self) -> impl Future<Output = Bytes> {
        let random_id = (self.rng.borrow_mut().get_u32() % 10_000 + 1) as i32;
        self.cl.query_one(&self.world, &[&random_id]).map(|row| {
            let row = row.unwrap();
            World {
                id: row.get(0),
                randomnumber: row.get(1),
            }
            .to_bytes::<BytesMut>(40)
        })
    }

    pub fn get_worlds(&self, num: u16) -> impl Future<Output = Vec<World>> {
        let worlds = FuturesUnordered::new();
        let mut rng = self.rng.borrow_mut();
        for _ in 0..num {
            let w_id = (rng.get_u32() % 10_000 + 1) as i32;
            worlds.push(
                self.cl
                    .query_one(&self.world, &[&w_id])
                    .map(|res| match res {
                        Err(_) => panic!(),
                        Ok(row) => World {
                            id: row.get(0),
                            randomnumber: row.get(1),
                        },
                    }),
            );
        }

        worlds.collect()
    }

    pub fn update(&self, num: u16) -> impl Future<Output = Vec<World>> {
        let worlds = FuturesUnordered::new();
        let mut rng = self.rng.borrow_mut();
        for _ in 0..num {
            let id = (rng.get_u32() % 10_000 + 1) as i32;
            let w_id = (rng.get_u32() % 10_000 + 1) as i32;
            worlds.push(
                self.cl
                    .query_one(&self.world, &[&w_id])
                    .map(move |res| match res {
                        Err(_) => panic!(),
                        Ok(row) => World {
                            id: row.get(0),
                            randomnumber: id,
                        },
                    }),
            );
        }

        let cl = self.cl.clone();
        let st = self.updates[(num as usize) - 1].clone();
        async move {
            let worlds: Vec<World> = worlds.collect().await;

            let mut params: Vec<&dyn ToSql> = Vec::with_capacity(num as usize * 3);
            for w in &worlds {
                params.push(&w.id);
                params.push(&w.randomnumber);
            }
            for w in &worlds {
                params.push(&w.id);
            }
            let _ = cl
                .query(&st, &params)
                .await
                .map_err(|e| log::error!("{:?}", e));

            worlds
        }
    }

    pub fn tell_fortune(&self) -> impl Future<Output = Bytes> {
        let fut = self.cl.query_raw(&self.fortune, &[]);

        async move {
            let mut stream = fut.await.unwrap();
            let mut fortunes: SmallVec<[_; 32]> = smallvec::smallvec![Fortune {
                id: 0,
                message: Cow::Borrowed("Additional fortune added at request time."),
            }];

            while let Some(row) = stream.next().await {
                let row = row.unwrap();
                fortunes.push(Fortune {
                    id: row.get(0),
                    message: Cow::Owned(row.get(1)),
                });
            }

            fortunes.sort_by(|it, next| it.message.cmp(&next.message));

            let mut buf = Vec::with_capacity(2048);
            ywrite_html!(buf, "{{> fortune }}");

            Bytes::from(buf)
        }
    }
}
