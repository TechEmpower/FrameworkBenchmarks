use std::fmt::Write;

use xitca_io::bytes::BytesMut;
use xitca_postgres::{pipeline::Pipeline, AsyncLendingIterator, Pool, Type};

use super::{
    ser::{Fortune, Fortunes, World},
    util::{HandleResult, Rand, DB_URL},
};

pub struct Client {
    pool: Pool,
    #[cfg(not(feature = "pg-sync"))]
    shared: std::cell::RefCell<Shared>,
    #[cfg(feature = "pg-sync")]
    shared: std::sync::Mutex<Shared>,
    updates: Box<[Box<str>]>,
}

type Shared = (Rand, BytesMut);

const FORTUNE_SQL: &str = "SELECT * FROM fortune";

const FORTUNE_SQL_TYPES: &[Type] = &[];

const WORLD_SQL: &str = "SELECT * FROM world WHERE id=$1";

const WORLD_SQL_TYPES: &[Type] = &[Type::INT4];

fn update_query(num: usize) -> Box<str> {
    const PREFIX: &str = "UPDATE world SET randomNumber = w.r FROM (VALUES ";
    const SUFFIX: &str = ") AS w (i,r) WHERE world.id = w.i";

    let (_, mut query) = (1..=num).fold((1, String::from(PREFIX)), |(idx, mut query), _| {
        write!(query, "(${}::int,${}::int),", idx, idx + 1).unwrap();
        (idx + 2, query)
    });

    query.pop();

    query.push_str(SUFFIX);

    query.into_boxed_str()
}

pub async fn create() -> HandleResult<Client> {
    let pool = Pool::builder(DB_URL).capacity(1).build()?;

    let shared = (Rand::default(), BytesMut::new());

    let updates = core::iter::once(Box::from(""))
        .chain((1..=500).map(update_query))
        .collect::<Box<[Box<str>]>>();

    {
        let mut conn = pool.get().await?;
        for update in updates.iter().skip(1) {
            conn.prepare(update, &[]).await?;
        }
    }

    Ok(Client {
        pool,
        #[cfg(not(feature = "pg-sync"))]
        shared: std::cell::RefCell::new(shared),
        #[cfg(feature = "pg-sync")]
        shared: std::sync::Mutex::new(shared),
        updates,
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
        let mut conn = self.pool.get().await?;
        let stmt = conn.prepare(WORLD_SQL, WORLD_SQL_TYPES).await?;
        let id = self.shared().0.gen_id();
        let mut res = conn.consume().query_raw(&stmt, [id])?;
        let row = res.try_next().await?.ok_or_else(|| "World does not exist")?;
        Ok(World::new(row.get_raw(0), row.get_raw(1)))
    }

    pub async fn get_worlds(&self, num: u16) -> HandleResult<Vec<World>> {
        let len = num as usize;

        let mut conn = self.pool.get().await?;
        let stmt = conn.prepare(WORLD_SQL, WORLD_SQL_TYPES).await?;

        let mut res = {
            let (ref mut rng, ref mut buf) = *self.shared();
            let mut pipe = Pipeline::with_capacity_from_buf(len, buf);
            (0..num).try_for_each(|_| pipe.query_raw(&stmt, [rng.gen_id()]))?;
            conn.consume().pipeline(pipe)?
        };

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

        let update = self.updates.get(len).ok_or_else(|| "num out of bound")?;

        let mut conn = self.pool.get().await?;
        let world_stmt = conn.prepare(WORLD_SQL, WORLD_SQL_TYPES).await?;
        let update_stmt = conn.prepare(&update, &[]).await?;

        let mut params = Vec::with_capacity(len);

        let mut res = {
            let (ref mut rng, ref mut buf) = *self.shared();
            let mut pipe = Pipeline::with_capacity_from_buf(len + 1, buf);
            (0..num).try_for_each(|_| {
                let w_id = rng.gen_id();
                let r_id = rng.gen_id();
                params.push([w_id, r_id]);
                pipe.query_raw(&world_stmt, [w_id])
            })?;
            pipe.query_raw(&update_stmt, sort_update_params(&params))?;
            conn.consume().pipeline(pipe)?
        };

        let mut worlds = Vec::with_capacity(len);

        let mut r_ids = params.into_iter();

        while let Some(mut item) = res.try_next().await? {
            while let Some(row) = item.try_next().await? {
                let r_id = r_ids.next().unwrap()[1];
                worlds.push(World::new(row.get_raw(0), r_id))
            }
        }

        Ok(worlds)
    }

    pub async fn tell_fortune(&self) -> HandleResult<Fortunes> {
        let mut items = Vec::with_capacity(32);
        items.push(Fortune::new(0, "Additional fortune added at request time."));

        let mut conn = self.pool.get().await?;
        let stmt = conn.prepare(FORTUNE_SQL, FORTUNE_SQL_TYPES).await?;
        let mut res = conn.consume().query_raw::<[i32; 0]>(&stmt, [])?;

        while let Some(row) = res.try_next().await? {
            items.push(Fortune::new(row.get_raw(0), row.get_raw::<String>(1)));
        }

        items.sort_by(|it, next| it.message.cmp(&next.message));

        Ok(Fortunes::new(items))
    }
}

fn sort_update_params(params: &Vec<[i32; 2]>) -> impl ExactSizeIterator<Item = i32> {
    let mut params = params.clone();
    params.sort_by(|a, b| a[0].cmp(&b[0]));

    struct ParamIter<I>(I);

    impl<I> Iterator for ParamIter<I>
    where
        I: Iterator,
    {
        type Item = I::Item;

        #[inline]
        fn next(&mut self) -> Option<Self::Item> {
            self.0.next()
        }

        #[inline]
        fn size_hint(&self) -> (usize, Option<usize>) {
            self.0.size_hint()
        }
    }

    // impl depends on compiler optimization to flat Vec<[T]> to Vec<T> when inferring
    // it's size hint. possible to cause runtime panic.
    impl<I> ExactSizeIterator for ParamIter<I> where I: Iterator {}

    ParamIter(params.into_iter().flatten())
}
