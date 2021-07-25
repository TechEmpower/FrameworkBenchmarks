use std::{cell::RefCell, error::Error, fmt::Write};

use ahash::AHashMap;
use futures_util::stream::{FuturesUnordered, TryStreamExt};
use rand::{rngs::SmallRng, Rng, SeedableRng};
use tokio::pin;
use tokio_postgres::{types::ToSql, NoTls, Statement};

use super::ser::{Fortune, Fortunes, World};

pub struct Client {
    client: tokio_postgres::Client,
    rng: RefCell<SmallRng>,
    fortune: Statement,
    world: Statement,
    updates: AHashMap<u16, Statement>,
}

pub async fn create(config: &str) -> Client {
    let (client, conn) = tokio_postgres::connect(config, NoTls).await.unwrap();

    tokio::task::spawn_local(async move {
        let _ = conn.await;
    });

    let fortune = client.prepare("SELECT * FROM fortune").await.unwrap();

    let mut updates = AHashMap::new();
    for num in 1..=500u16 {
        let mut pl = 1;
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

        let st = client.prepare(&q).await.unwrap();
        updates.insert(num, st);
    }
    let world = client
        .prepare("SELECT * FROM world WHERE id=$1")
        .await
        .unwrap();

    Client {
        client,
        rng: RefCell::new(SmallRng::from_entropy()),
        fortune,
        world,
        updates,
    }
}

type DbResult<T> = Result<T, Box<dyn Error>>;

impl Client {
    pub async fn get_world(&self) -> DbResult<World> {
        let random_id = (self.rng.borrow_mut().gen::<u32>() % 10_000 + 1) as i32;
        let row = self.client.query_one(&self.world, &[&random_id]).await?;

        Ok(World::new(row.get(0), row.get(1)))
    }

    pub async fn get_worlds(&self, num: u16) -> DbResult<Vec<World>> {
        let worlds = {
            let mut rng = self.rng.borrow_mut();
            (0..num)
                .map(|_| {
                    let w_id = (rng.gen::<u32>() % 10_000 + 1) as i32;
                    async move {
                        let row = self.client.query_one(&self.world, &[&w_id]).await?;
                        Ok(World::new(row.get(0), row.get(1)))
                    }
                })
                .collect::<FuturesUnordered<_>>()
        };

        worlds.try_collect().await
    }

    pub async fn update(&self, num: u16) -> DbResult<Vec<World>> {
        let worlds = {
            let mut rng = self.rng.borrow_mut();

            (0..num)
                .map(|_| {
                    let id = (rng.gen::<u32>() % 10_000 + 1) as i32;
                    let w_id = (rng.gen::<u32>() % 10_000 + 1) as i32;
                    async move {
                        let row = self.client.query_one(&self.world, &[&w_id]).await?;
                        let mut world = World::new(row.get(0), row.get(1));
                        world.randomnumber = id;
                        Ok::<_, Box<dyn Error>>(world)
                    }
                })
                .collect::<FuturesUnordered<_>>()
        };

        let worlds = worlds.try_collect::<Vec<_>>().await?;

        let mut params = Vec::<&(dyn ToSql + Sync)>::with_capacity(num as usize * 3);

        for w in &worlds {
            params.push(&w.id);
            params.push(&w.randomnumber);
        }
        for w in &worlds {
            params.push(&w.id);
        }

        let st = self.updates.get(&num).unwrap();

        let _ = self.client.query(st, params.as_slice()).await?;

        Ok(worlds)
    }

    pub async fn tell_fortune(&self) -> DbResult<Fortunes> {
        let mut items = Vec::with_capacity(32);

        items.push(Fortune::new(0, "Additional fortune added at request time."));

        let stream = self
            .client
            .query_raw::<_, _, &[i32; 0]>(&self.fortune, &[])
            .await?;

        pin!(stream);

        while let Some(row) = stream.try_next().await? {
            items.push(Fortune::new(row.get(0), row.get::<_, String>(1)));
        }

        items.sort_by(|it, next| it.message.cmp(&next.message));

        Ok(Fortunes::new(items))
    }
}
