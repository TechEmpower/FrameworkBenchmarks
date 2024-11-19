#![allow(clippy::uninit_vec)]
use std::{borrow::Cow, cell::RefCell, fmt::Write as FmtWrite};

use nanorand::{Rng, WyRand};
use ntex::util::{BufMut, Bytes, BytesMut};
use smallvec::SmallVec;
use tokio_postgres::types::ToSql;
use tokio_postgres::{connect, Client, Statement};
use yarte::TemplateBytesTrait;

use super::utils;

#[derive(Copy, Clone, Debug, sonic_rs::Serialize)]
pub struct World {
    pub id: i32,
    pub randomnumber: i32,
}

#[derive(Debug, sonic_rs::Serialize)]
pub struct Fortune {
    pub id: i32,
    pub message: Cow<'static, str>,
}

#[derive(yarte::TemplateBytes)]
#[template(path = "fortune.hbs")]
pub struct FortunesTemplate<'a> {
    pub fortunes: &'a Vec<Fortune>,
}

/// Postgres interface
pub struct PgConnection {
    cl: Client,
    fortune: Statement,
    world: Statement,
    rng: WyRand,
    updates: Vec<Statement>,
    buf: RefCell<BytesMut>,
    fbuf: RefCell<Vec<Fortune>>,
}

impl PgConnection {
    pub async fn connect(db_url: &str) -> PgConnection {
        let (cl, conn) = connect(db_url)
            .await
            .expect("can not connect to postgresql");
        ntex::rt::spawn(async move {
            let _ = conn.await;
        });

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
            buf: RefCell::new(BytesMut::with_capacity(10 * 1024 * 1024)),
            fbuf: RefCell::new(Vec::with_capacity(64)),
        }
    }
}

impl PgConnection {
    pub async fn get_world(&self) -> Bytes {
        let random_id = (self.rng.clone().generate::<u32>() % 10_000 + 1) as i32;

        let row = self.cl.query_one(&self.world, &[&random_id]).await.unwrap();

        let mut body = self.buf.borrow_mut();
        utils::reserve(&mut body, 1024);
        sonic_rs::to_writer(
            utils::BytesWriter(&mut body),
            &World {
                id: row.get(0),
                randomnumber: row.get(1),
            },
        )
        .unwrap();
        body.split().freeze()
    }

    pub async fn get_worlds(&self, num: usize) -> Bytes {
        let mut rng = self.rng.clone();
        let mut queries = SmallVec::<[_; 32]>::new();
        (0..num).for_each(|_| {
            let w_id = (rng.generate::<u32>() % 10_000 + 1) as i32;
            queries.push(self.cl.query_one(&self.world, &[&w_id]));
        });

        let mut worlds = SmallVec::<[_; 32]>::new();
        for fut in queries {
            let row = fut.await.unwrap();
            worlds.push(World {
                id: row.get(0),
                randomnumber: row.get(1),
            })
        }

        let mut body = self.buf.borrow_mut();
        utils::reserve(&mut body, 2 * 1024);
        body.put_u8(b'[');
        worlds.iter().for_each(|w| {
            sonic_rs::to_writer(utils::BytesWriter(&mut body), w).unwrap();
            body.put_u8(b',');
        });
        let idx = body.len() - 1;
        body[idx] = b']';
        body.split().freeze()
    }

    pub async fn update(&self, num: usize) -> Bytes {
        let mut rng = nanorand::tls_rng();
        let mut queries = SmallVec::<[_; 32]>::new();
        (0..num).for_each(|_| {
            let w_id = (rng.generate::<u32>() % 10_000 + 1) as i32;
            queries.push(self.cl.query_one(&self.world, &[&w_id]));
        });

        let mut worlds = SmallVec::<[_; 32]>::new();
        for fut in queries.into_iter() {
            let row = fut.await.unwrap();
            worlds.push(World {
                id: row.get(0),
                randomnumber: (rng.generate::<u32>() % 10_000 + 1) as i32,
            });
        }

        let mut params: Vec<&dyn ToSql> = Vec::with_capacity(num * 3);
        for w in &worlds {
            params.push(&w.id);
            params.push(&w.randomnumber);
        }
        for w in &worlds {
            params.push(&w.id);
        }
        let _ = self.cl.query(&self.updates[num - 1], &params).await;

        let mut body = self.buf.borrow_mut();
        utils::reserve(&mut body, 2 * 1024);
        body.put_u8(b'[');
        worlds.iter().for_each(|w| {
            sonic_rs::to_writer(utils::BytesWriter(&mut body), w).unwrap();
            body.put_u8(b',');
        });
        let idx = body.len() - 1;
        body[idx] = b']';
        body.split().freeze()
    }

    pub async fn tell_fortune(&self) -> Bytes {
        let rows = self.cl.query_raw(&self.fortune, &[]).await.unwrap();

        let mut fortunes = self.fbuf.borrow_mut();
        fortunes.push(Fortune {
            id: 0,
            message: Cow::Borrowed("Additional fortune added at request time."),
        });
        fortunes.extend(rows.iter().map(|row| Fortune {
            id: row.get(0),
            message: Cow::Owned(row.get(1)),
        }));
        fortunes.sort_by(|it, next| it.message.cmp(&next.message));

        let mut body = std::mem::replace(&mut *self.buf.borrow_mut(), BytesMut::new());
        utils::reserve(&mut body, 4 * 1024);

        FortunesTemplate {
            fortunes: &*fortunes,
        }
        .write_call(&mut body);
        fortunes.clear();

        let result = body.split().freeze();
        let _ = std::mem::replace(&mut *self.buf.borrow_mut(), body);
        result
    }
}
