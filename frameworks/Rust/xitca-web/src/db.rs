use std::{collections::HashMap, fmt::Write};

use xitca_postgres::{
    pipeline::Pipeline, statement::Statement, AsyncLendingIterator, SharedClient,
};
use xitca_unsafe_collection::no_hash::NoHashBuilder;

use super::{
    ser::{Fortune, Fortunes, World},
    util::{HandleResult, Rand, DB_URL},
};

pub struct Client {
    client: SharedClient,
    #[cfg(not(feature = "pg-sync"))]
    rng: std::cell::RefCell<Rand>,
    #[cfg(feature = "pg-sync")]
    rng: std::sync::Mutex<Rand>,
    fortune: Statement,
    world: Statement,
    updates: HashMap<u16, Statement, NoHashBuilder>,
}

pub async fn create() -> HandleResult<Client> {
    let mut client = SharedClient::new(DB_URL.to_string()).await?;

    let fortune = client.prepare_cached("SELECT * FROM fortune", &[]).await?;

    let world = client
        .prepare_cached("SELECT * FROM world WHERE id=$1", &[])
        .await?;

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

        let st = client.prepare_cached(&q, &[]).await?;
        updates.insert(num, st);
    }

    Ok(Client {
        client,
        #[cfg(not(feature = "pg-sync"))]
        rng: std::cell::RefCell::new(Rand::default()),
        #[cfg(feature = "pg-sync")]
        rng: std::sync::Mutex::new(Rand::default()),
        fortune,
        world,
        updates,
    })
}

impl Client {
    #[cfg(not(feature = "pg-sync"))]
    fn borrow_rng_mut(&self) -> std::cell::RefMut<'_, Rand> {
        self.rng.borrow_mut()
    }

    #[cfg(feature = "pg-sync")]
    fn borrow_rng_mut(&self) -> std::sync::MutexGuard<'_, Rand> {
        self.rng.lock().unwrap()
    }

    pub async fn get_world(&self) -> HandleResult<World> {
        let id = self.borrow_rng_mut().gen_id();
        self.client
            .query_raw(&self.world, [id])
            .await?
            .try_next()
            .await?
            .map(|row| World::new(row.get_raw(0), row.get_raw(1)))
            .ok_or_else(|| "World does not exist".into())
    }

    pub async fn get_worlds(&self, num: u16) -> HandleResult<Vec<World>> {
        let mut pipe = Pipeline::new();

        {
            let mut rng = self.borrow_rng_mut();
            (0..num).try_for_each(|_| pipe.query_raw(&self.world, [rng.gen_id()]))?;
        }

        let mut worlds = Vec::new();
        worlds.reserve(num as usize);

        let mut res = self.client.pipeline(pipe).await?;
        while let Some(mut item) = res.try_next().await? {
            while let Some(row) = item.try_next().await? {
                worlds.push(World::new(row.get_raw(0), row.get_raw(1)))
            }
        }

        Ok(worlds)
    }

    pub async fn update(&self, num: u16) -> HandleResult<Vec<World>> {
        let len = num as usize;

        let mut params = Vec::new();
        params.reserve(len * 3);

        let mut pipe = Pipeline::new();

        {
            let mut rng = self.borrow_rng_mut();
            (0..num).try_for_each(|_| {
                let w_id = rng.gen_id();
                let r_id = rng.gen_id();
                params.extend([w_id, r_id]);
                pipe.query_raw(&self.world, [w_id])
            })?;
        }

        params.extend_from_within(..len);
        let st = self.updates.get(&num).unwrap();
        pipe.query_raw(st, &params)?;

        let mut worlds = Vec::new();
        worlds.reserve(len);
        let mut r_ids = params.into_iter().skip(1).step_by(2);

        let mut res = self.client.pipeline(pipe).await?;
        while let Some(mut item) = res.try_next().await? {
            while let Some(row) = item.try_next().await? {
                let r_id = r_ids.next().unwrap();
                worlds.push(World::new(row.get_raw(0), r_id))
            }
        }

        Ok(worlds)
    }

    pub async fn tell_fortune(&self) -> HandleResult<Fortunes> {
        let mut items = Vec::with_capacity(32);
        items.push(Fortune::new(0, "Additional fortune added at request time."));

        let mut stream = self.client.query_raw::<[i32; 0]>(&self.fortune, []).await?;
        while let Some(row) = stream.try_next().await? {
            items.push(Fortune::new(row.get_raw(0), row.get_raw::<String>(1)));
        }
        items.sort_by(|it, next| it.message.cmp(&next.message));

        Ok(Fortunes::new(items))
    }
}
