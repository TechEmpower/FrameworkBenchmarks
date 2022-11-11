use std::{borrow::Cow, fmt::Write as FmtWrite};

use futures::{Future, FutureExt};
use nanorand::{Rng, WyRand};
use ntex::util::{Bytes, BytesMut};
use smallvec::SmallVec;
use tokio_postgres::types::ToSql;
use tokio_postgres::{connect, Client, Statement};
use yarte::{ywrite_html, Serialize};

#[derive(Copy, Clone, Serialize, Debug)]
pub struct World {
    pub id: i32,
    pub randomnumber: i32,
}

#[derive(Serialize, Debug)]
pub struct Fortune {
    pub id: i32,
    pub message: Cow<'static, str>,
}

/// Postgres interface
pub struct PgConnection {
    cl: Client,
    fortune: Statement,
    world: Statement,
    rng: WyRand,
    updates: Vec<Statement>,
}

impl PgConnection {
    pub async fn connect(db_url: &str) -> PgConnection {
        let (cl, conn) = connect(db_url)
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
            rng: WyRand::new(),
        }
    }
}

impl PgConnection {
    pub fn get_world(&self) -> impl Future<Output = Bytes> {
        let random_id = (self.rng.clone().generate::<u32>() % 10_000 + 1) as i32;
        self.cl.query(&self.world, &[&random_id]).map(|rows| {
            let rows = rows.unwrap();
            let mut body = BytesMut::with_capacity(64);
            World {
                id: rows[0].get(0),
                randomnumber: rows[0].get(1),
            }
            .to_bytes_mut(&mut body);
            body.freeze()
        })
    }

    pub fn get_worlds(&self, num: usize) -> impl Future<Output = Bytes> {
        let mut futs = Vec::with_capacity(num);
        let mut rng = self.rng.clone();
        for _ in 0..num {
            let w_id = (rng.generate::<u32>() % 10_000 + 1) as i32;
            futs.push(self.cl.query(&self.world, &[&w_id]));
        }

        async move {
            let mut worlds = BytesMut::with_capacity(48 * num);
            for fut in futs {
                let rows = fut.await.unwrap();
                World {
                    id: rows[0].get(0),
                    randomnumber: rows[0].get(1),
                }
                .to_bytes_mut(&mut worlds)
            }
            worlds.freeze()
        }
    }

    pub fn update(&self, num: usize) -> impl Future<Output = Bytes> {
        let mut futs = Vec::with_capacity(num);
        let mut rng = self.rng.clone();
        for _ in 0..num {
            let w_id = (rng.generate::<u32>() % 10_000 + 1) as i32;
            futs.push(self.cl.query(&self.world, &[&w_id]));
        }

        let cl = self.cl.clone();
        let st = self.updates[num - 1].clone();
        let base = num * 2;
        async move {
            let mut worlds = BytesMut::with_capacity(48 * num);
            let mut params_data: Vec<i32> = Vec::with_capacity(num * 3);
            unsafe {
                params_data.set_len(num * 3);
            }
            for (idx, fut) in futs.into_iter().enumerate() {
                let q = fut.await.unwrap();
                let id = (rng.generate::<u32>() % 10_000 + 1) as i32;
                let wid = q[0].get(0);
                let randomnumber = id;

                params_data[idx * 2] = wid;
                params_data[idx * 2 + 1] = randomnumber;
                params_data[base + idx] = wid;
                World {
                    id: wid,
                    randomnumber,
                }
                .to_bytes_mut(&mut worlds);
            }

            ntex::rt::spawn(async move {
                let mut params: Vec<&dyn ToSql> = Vec::with_capacity(num as usize * 3);
                for i in params_data.iter() {
                    params.push(i);
                }
                let _ = cl
                    .query(&st, &params)
                    .await
                    .map_err(|e| log::error!("{:?}", e));
            });

            worlds.freeze()
        }
    }

    pub fn tell_fortune(&self) -> impl Future<Output = Bytes> {
        let fut = self.cl.query_raw(&self.fortune, &[]);

        async move {
            let rows = fut.await.unwrap();
            let mut fortunes: SmallVec<[_; 32]> = smallvec::smallvec![Fortune {
                id: 0,
                message: Cow::Borrowed("Additional fortune added at request time."),
            }];

            for row in rows {
                fortunes.push(Fortune {
                    id: row.get(0),
                    message: Cow::Owned(row.get(1)),
                });
            }

            fortunes.sort_by(|it, next| it.message.cmp(&next.message));

            let mut buf = BytesMut::with_capacity(2048);
            ywrite_html!(buf, "{{> fortune }}");

            buf.freeze()
        }
    }
}
