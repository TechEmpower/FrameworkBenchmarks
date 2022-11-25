#![allow(clippy::uninit_vec)]
use std::{borrow::Cow, cell::RefCell, fmt::Write as FmtWrite, rc::Rc};

use futures::{Future, FutureExt};
use nanorand::{Rng, WyRand};
use ntex::util::{BufMut, Bytes, BytesMut};
use smallvec::SmallVec;
use tokio_postgres::types::ToSql;
use tokio_postgres::{connect, Client, Statement};
use yarte::{ywrite_html, Serialize};

use super::utils;

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
    buf: Rc<RefCell<BytesMut>>,
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
            buf: Rc::new(RefCell::new(BytesMut::with_capacity(65535))),
        }
    }
}

impl PgConnection {
    pub fn get_world(&self) -> impl Future<Output = Bytes> {
        let buf = self.buf.clone();
        let random_id = (self.rng.clone().generate::<u32>() % 10_000 + 1) as i32;
        self.cl
            .query_one(&self.world, &[&random_id])
            .map(move |row| {
                let row = row.unwrap();
                let mut body = buf.borrow_mut();
                utils::reserve(&mut body);
                World {
                    id: row.get(0),
                    randomnumber: row.get(1),
                }
                .to_bytes_mut(&mut *body);
                body.split().freeze()
            })
    }

    pub fn get_worlds(&self, num: usize) -> impl Future<Output = Bytes> {
        let buf = self.buf.clone();
        let mut rng = self.rng.clone();
        let mut queries = SmallVec::<[_; 32]>::new();
        (0..num).for_each(|_| {
            let w_id = (rng.generate::<u32>() % 10_000 + 1) as i32;
            queries.push(self.cl.query_one(&self.world, &[&w_id]));
        });

        async move {
            let mut worlds = SmallVec::<[_; 32]>::new();
            for fut in queries {
                let row = fut.await.unwrap();
                worlds.push(World {
                    id: row.get(0),
                    randomnumber: row.get(1),
                })
            }

            let mut body = buf.borrow_mut();
            utils::reserve(&mut body);

            body.put_u8(b'[');
            worlds.iter().for_each(|w| {
                w.to_bytes_mut(&mut *body);
                body.put_u8(b',');
            });
            let idx = body.len() - 1;
            body[idx] = b']';
            body.split().freeze()
        }
    }

    pub fn update(&self, num: usize) -> impl Future<Output = Bytes> {
        let buf = self.buf.clone();
        let mut rng = self.rng.clone();
        let mut queries = SmallVec::<[_; 32]>::new();
        (0..num).for_each(|_| {
            let w_id = (rng.generate::<u32>() % 10_000 + 1) as i32;
            queries.push(self.cl.query_one(&self.world, &[&w_id]));
        });

        let cl = self.cl.clone();
        let st = self.updates[num - 1].clone();
        let base = num * 2;
        async move {
            let mut worlds = SmallVec::<[_; 32]>::new();
            let mut params_data: Vec<i32> = Vec::with_capacity(num * 3);
            unsafe {
                params_data.set_len(num * 3);
            }
            for (idx, fut) in queries.into_iter().enumerate() {
                let q = fut.await.unwrap();
                let id = (rng.generate::<u32>() % 10_000 + 1) as i32;
                let wid = q.get(0);
                let randomnumber = id;

                params_data[idx * 2] = wid;
                params_data[idx * 2 + 1] = randomnumber;
                params_data[base + idx] = wid;
                worlds.push(World {
                    id: wid,
                    randomnumber,
                });
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

            let mut body = buf.borrow_mut();
            utils::reserve(&mut body);
            body.put_u8(b'[');
            worlds.iter().for_each(|w| {
                w.to_bytes_mut(&mut *body);
                body.put_u8(b',');
            });
            let idx = body.len() - 1;
            body[idx] = b']';
            body.split().freeze()
        }
    }

    pub fn tell_fortune(&self) -> impl Future<Output = Bytes> {
        let buf = self.buf.clone();
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

            let mut body = std::mem::replace(&mut *buf.borrow_mut(), BytesMut::new());
            utils::reserve(&mut body);
            ywrite_html!(body, "{{> fortune }}");

            let result = body.split().freeze();
            let _ = std::mem::replace(&mut *buf.borrow_mut(), body);
            result
        }
    }
}
