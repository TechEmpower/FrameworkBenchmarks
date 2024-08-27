use std::fmt::Write;

use xitca_io::bytes::BytesMut;
use xitca_postgres::{
    pipeline::Pipeline, statement::Statement, AsyncLendingIterator, SharedClient,
};

use super::{
    ser::{Fortune, Fortunes, World},
    util::{HandleResult, Rand, DB_URL},
};

pub struct Client {
    client: SharedClient,
    #[cfg(not(feature = "pg-sync"))]
    shared: std::cell::RefCell<Shared>,
    #[cfg(feature = "pg-sync")]
    shared: std::sync::Mutex<Shared>,
    fortune: Statement,
    world: Statement,
    updates: Box<[Statement]>,
}

type Shared = (Rand, BytesMut);

pub async fn create() -> HandleResult<Client> {
    let mut client = SharedClient::new(DB_URL.to_string()).await?;

    let fortune = client.prepare_cached("SELECT * FROM fortune", &[]).await?;

    let world = client
        .prepare_cached("SELECT * FROM world WHERE id=$1", &[])
        .await?;

    let mut updates = Vec::new();

    // a dummy statement as placeholder of 0 index.
    // avoid off by one calculation when using non zero u16 as slicing index.
    updates.push(Statement::default());

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
        updates.push(st);
    }

    let shared = (Rand::default(), BytesMut::new());

    Ok(Client {
        client,
        #[cfg(not(feature = "pg-sync"))]
        shared: std::cell::RefCell::new(shared),
        #[cfg(feature = "pg-sync")]
        shared: std::sync::Mutex::new(shared),
        fortune,
        world,
        updates: updates.into_boxed_slice(),
    })
}

impl Client {
    #[cfg(not(feature = "pg-sync"))]
    fn shared(&self) -> std::cell::RefMut<'_, Shared> {
        self.shared.borrow_mut()
    }

    #[cfg(feature = "pg-sync")]
    fn shared(&self) -> std::sync::MutexGuard<'_, Shared> {
        self.shared.lock().unwrap()
    }

    pub async fn get_world(&self) -> HandleResult<World> {
        let id = self.shared().0.gen_id();
        self.client
            .query_raw(&self.world, [id])
            .await?
            .try_next()
            .await?
            .map(|row| World::new(row.get_raw(0), row.get_raw(1)))
            .ok_or_else(|| "World does not exist".into())
    }

    pub async fn get_worlds(&self, num: u16) -> HandleResult<Vec<World>> {
        let len = num as usize;

        let mut res = {
            let (ref mut rng, ref mut buf) = *self.shared();

            let mut pipe = Pipeline::<_, false>::with_capacity_from_buf(len, buf);

            (0..num).try_for_each(|_| pipe.query_raw(&self.world, [rng.gen_id()]))?;

            self.client.pipeline(pipe)
        }
        .await?;

        let mut worlds = Vec::with_capacity(len);

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

        let mut res = {
            let (ref mut rng, ref mut buf) = *self.shared();

            let mut pipe = Pipeline::<_, false>::with_capacity_from_buf(len + 1, buf);

            (0..num).try_for_each(|_| {
                let w_id = rng.gen_id();
                let r_id = rng.gen_id();
                params.extend([w_id, r_id]);
                pipe.query_raw(&self.world, [w_id])
            })?;

            params.extend_from_within(..len);

            let st = self.updates.get(len).unwrap();
            pipe.query_raw(st, &params)?;

            self.client.pipeline(pipe)
        }
        .await?;

        let mut worlds = Vec::new();
        worlds.reserve(len);
        let mut r_ids = params.into_iter().skip(1).step_by(2);

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

        let mut res = self.client.query_raw::<[i32; 0]>(&self.fortune, []).await?;
        while let Some(row) = res.try_next().await? {
            items.push(Fortune::new(row.get_raw(0), row.get_raw::<String>(1)));
        }
        items.sort_by(|it, next| it.message.cmp(&next.message));

        Ok(Fortunes::new(items))
    }
}
