use std::{cell::RefCell, collections::HashMap, error::Error, fmt::Write, future::Future};

use futures_util::stream::{FuturesUnordered, StreamExt, TryStreamExt};
use rand::{rngs::SmallRng, Rng, SeedableRng};
use xitca_postgres::{statement::Statement, Postgres, ToSql};
use xitca_unsafe_collection::no_hash::NoHashBuilder;

use super::ser::{Fortune, Fortunes, World};

pub struct Client {
    client: xitca_postgres::Client,
    rng: RefCell<SmallRng>,
    fortune: Statement,
    world: Statement,
    updates: HashMap<u16, Statement, NoHashBuilder>,
}

impl Drop for Client {
    fn drop(&mut self) {
        drop(self.fortune.clone().into_guarded(&self.client));
        drop(self.world.clone().into_guarded(&self.client));
        for (_, stmt) in std::mem::take(&mut self.updates) {
            drop(stmt.into_guarded(&self.client))
        }
    }
}

pub async fn create(config: &str) -> DbResult<Client> {
    let (client, bg_task) = Postgres::new(config.to_string()).connect().await?;

    tokio::task::spawn_local(bg_task);

    let fortune = client.prepare("SELECT * FROM fortune", &[]).await?.leak();

    let world = client
        .prepare("SELECT * FROM world WHERE id=$1", &[])
        .await?
        .leak();

    let mut updates = HashMap::default();

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

        let st = client.prepare(&q, &[]).await?.leak();
        updates.insert(num, st);
    }

    Ok(Client {
        client,
        rng: RefCell::new(SmallRng::from_entropy()),
        fortune,
        world,
        updates,
    })
}

type DbResult<T> = Result<T, Box<dyn Error>>;

impl Client {
    async fn query_one_world(&self, id: i32) -> DbResult<World> {
        let row = self
            .client
            .query_raw(&self.world, &[&id])
            .await?
            .next()
            .await
            .ok_or_else(|| format!("World {id} does not exist"))??;

        Ok(World::new(row.get(0), row.get(1)))
    }

    pub fn get_world(&self) -> impl Future<Output = DbResult<World>> + '_ {
        let id = (self.rng.borrow_mut().gen::<u32>() % 10_000 + 1) as i32;
        self.query_one_world(id)
    }

    pub fn get_worlds(&self, num: u16) -> impl Future<Output = DbResult<Vec<World>>> + '_ {
        let mut rng = self.rng.borrow_mut();
        (0..num)
            .map(|_| {
                let id = (rng.gen::<u32>() % 10_000 + 1) as i32;
                self.query_one_world(id)
            })
            .collect::<FuturesUnordered<_>>()
            .try_collect()
    }

    pub async fn update(&self, num: u16) -> DbResult<Vec<World>> {
        let worlds = {
            let mut rng = self.rng.borrow_mut();

            (0..num)
                .map(|_| {
                    let id = (rng.gen::<u32>() % 10_000 + 1) as i32;
                    let w_id = (rng.gen::<u32>() % 10_000 + 1) as i32;
                    async move {
                        let mut world = self.query_one_world(w_id).await?;
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

        let mut stream = self
            .client
            .query_raw::<&[i32; 0]>(&self.fortune, &[])
            .await?;

        while let Some(row) = stream.try_next().await? {
            items.push(Fortune::new(row.get(0), row.get::<String>(1)));
        }

        items.sort_by(|it, next| it.message.cmp(&next.message));

        Ok(Fortunes::new(items))
    }
}
