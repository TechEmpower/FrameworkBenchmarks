use std::{borrow::Cow, cell::RefCell};

use nanorand::{Rng, WyRand};
use ntex::util::{Bytes, BytesMut};
use smallvec::SmallVec;
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
    updates: Statement,
    buf: RefCell<BytesMut>,
    fbuf: RefCell<Vec<Fortune>>,
}

impl PgConnection {
    pub async fn connect(db_url: &str) -> PgConnection {
        let (cl, conn) = connect(db_url, utils::db_config())
            .await
            .expect("can not connect to postgresql");
        ntex::rt::spawn(async move {
            let _ = conn.await;
        });

        let fortune = cl.prepare("SELECT * FROM fortune").await.unwrap();
        let updates = cl.prepare("UPDATE world w SET randomnumber = u.new_val FROM (SELECT unnest($1::int[]) as id, unnest($2::int[]) as new_val) u WHERE w.id = u.id").await.unwrap();
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
        let mut queries = Vec::with_capacity(num);
        (0..num).for_each(|_| {
            let w_id = (rng.generate::<u32>() % 10_000 + 1) as i32;
            queries.push(self.cl.query_one(&self.world, &[&w_id]));
        });

        let mut worlds = Vec::with_capacity(num);
        for fut in queries {
            let row = fut.await.unwrap();
            worlds.push(World {
                id: row.get(0),
                randomnumber: row.get(1),
            })
        }

        let mut body = self.buf.borrow_mut();
        utils::reserve(&mut body, 2 * 1024);
        sonic_rs::to_writer(utils::BytesWriter(&mut body), &worlds[..]).unwrap();
        body.split().freeze()
    }

    pub async fn update(&self, num: usize) -> Bytes {
        let mut rng = nanorand::tls_rng();
        let mut ids = Vec::with_capacity(num);
        let mut numbers = Vec::with_capacity(num);
        let mut worlds = SmallVec::<[_; 32]>::new();
        let mut queries = SmallVec::<[_; 32]>::new();

        (0..num).for_each(|_| {
            let w_id = (rng.generate::<u32>() % 10_000 + 1) as i32;
            ids.push(w_id);
            numbers.push((rng.generate::<u32>() % 10_000 + 1) as i32);
        });
        ids.sort();

        (0..num).for_each(|idx| {
            worlds.push(World {
                id: ids[idx],
                randomnumber: numbers[idx],
            });
            queries.push(self.cl.query_one(&self.world, &[&ids[idx]]));
        });
        let _ = self
            .cl
            .query(&self.updates, &[&ids, &numbers])
            .await
            .unwrap();

        let mut body = self.buf.borrow_mut();
        utils::reserve(&mut body, 2 * 1024);
        sonic_rs::to_writer(utils::BytesWriter(&mut body), &worlds[..]).unwrap();
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
            fortunes: &fortunes,
        }
        .write_call(&mut body);
        fortunes.clear();

        let result = body.split().freeze();
        let _ = std::mem::replace(&mut *self.buf.borrow_mut(), body);
        result
    }
}
