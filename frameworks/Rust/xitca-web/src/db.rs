use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::Write,
    future::{Future, IntoFuture},
};

use futures_util::future::{try_join, try_join_all, TryFutureExt};
use xitca_postgres::{statement::Statement, AsyncIterator, Postgres};
use xitca_unsafe_collection::no_hash::NoHashBuilder;

use super::{
    ser::{Fortune, Fortunes, World},
    util::{HandleResult, Rand},
};

pub struct Client {
    client: xitca_postgres::Client,
    rng: RefCell<Rand>,
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

pub async fn create(config: &str) -> HandleResult<Client> {
    let (client, driver) = Postgres::new(config.to_string()).connect().await?;

    tokio::task::spawn_local(driver.into_future());

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
        rng: RefCell::new(Rand::default()),
        fortune,
        world,
        updates,
    })
}

impl Client {
    async fn query_one_world(&self, id: i32) -> HandleResult<World> {
        self.client
            .query_raw(&self.world, [id])
            .await?
            .next()
            .await
            .ok_or_else(|| format!("World {id} does not exist"))?
            .map(|row| World::new(row.get(0), row.get(1)))
            .map_err(Into::into)
    }

    pub fn get_world(&self) -> impl Future<Output = HandleResult<World>> + '_ {
        let id = self.rng.borrow_mut().gen_id();
        self.query_one_world(id)
    }

    pub fn get_worlds(&self, num: u16) -> impl Future<Output = HandleResult<Vec<World>>> + '_ {
        let mut rng = self.rng.borrow_mut();
        let gets = (0..num).map(|_| self.query_one_world(rng.gen_id()));
        try_join_all(gets)
    }

    pub async fn update(&self, num: u16) -> HandleResult<Vec<World>> {
        let len = num as usize;

        let mut params = Vec::new();
        params.reserve(len * 3);

        let gets = {
            let mut rng = self.rng.borrow_mut();
            let gets = (0..num).map(|_| {
                let w_id = rng.gen_id();
                let r_id = rng.gen_id();
                params.push(w_id);
                params.push(r_id);
                self.query_one_world(w_id).map_ok(move |mut world| {
                    world.randomnumber = r_id;
                    world
                })
            });
            try_join_all(gets)
        };

        params.extend_from_within(..len);

        let st = self.updates.get(&num).unwrap();
        let update = self.client.execute_raw(st, &params).map_err(Into::into);

        try_join(gets, update).await.map(|(world, _)| world)
    }

    pub async fn tell_fortune(&self) -> HandleResult<Fortunes> {
        let mut items = Vec::with_capacity(32);

        items.push(Fortune::from_static(
            0,
            "Additional fortune added at request time.",
        ));

        let mut stream = self.client.query_raw::<[i32; 0]>(&self.fortune, []).await?;

        while let Some(row) = stream.next().await {
            let row = row?;
            items.push(Fortune::new(row.get(0), row.get(1)));
        }

        items.sort_by(|it, next| it.message.cmp(&next.message));

        Ok(Fortunes::new(items))
    }
}
