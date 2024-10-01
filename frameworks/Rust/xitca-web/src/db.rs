use std::cell::RefCell;

use xitca_io::bytes::BytesMut;
use xitca_postgres::{
    iter::AsyncLendingIterator,
    pipeline::Pipeline,
    pool::Pool,
    statement::{Statement, StatementNamed},
    types::Type,
    Execute, ExecuteMut,
};

use super::{
    ser::{Fortune, Fortunes, World},
    util::{bulk_update_gen, HandleResult, Rand, DB_URL},
};

pub struct Client {
    pool: Pool,
    shared: RefCell<Shared>,
    updates: Box<[Box<str>]>,
}

type Shared = (Rand, BytesMut);

const FORTUNE_STMT: StatementNamed = Statement::named("SELECT * FROM fortune", &[]);
const WORLD_STMT: StatementNamed = Statement::named("SELECT * FROM world WHERE id=$1", &[Type::INT4]);

fn update_query(num: usize) -> Box<str> {
    bulk_update_gen(|query| {
        use std::fmt::Write;
        (1..=num).fold((1, query), |(idx, query), _| {
            write!(query, "(${}::int,${}::int),", idx, idx + 1).unwrap();
            (idx + 2, query)
        });
    })
    .into_boxed_str()
}

pub async fn create() -> HandleResult<Client> {
    Ok(Client {
        pool: Pool::builder(DB_URL).capacity(1).build()?,
        shared: RefCell::new((Rand::default(), BytesMut::new())),
        updates: core::iter::once(Box::from(""))
            .chain((1..=500).map(update_query))
            .collect(),
    })
}

impl Client {
    pub async fn get_world(&self) -> HandleResult<World> {
        let mut conn = self.pool.get().await?;
        let stmt = WORLD_STMT.execute_mut(&mut conn).await?;
        let id = self.shared.borrow_mut().0.gen_id();
        let mut res = stmt.bind([id]).query(&conn.consume()).await?;
        let row = res.try_next().await?.ok_or("request World does not exist")?;
        Ok(World::new(row.get(0), row.get(1)))
    }

    pub async fn get_worlds(&self, num: u16) -> HandleResult<Vec<World>> {
        let len = num as usize;

        let mut conn = self.pool.get().await?;
        let stmt = WORLD_STMT.execute_mut(&mut conn).await?;

        let mut res = {
            let (ref mut rng, ref mut buf) = *self.shared.borrow_mut();
            let mut pipe = Pipeline::with_capacity_from_buf(len, buf);
            (0..num).try_for_each(|_| stmt.bind([rng.gen_id()]).query_mut(&mut pipe))?;
            pipe.query(&conn.consume())?
        };

        let mut worlds = Vec::with_capacity(len);

        while let Some(mut item) = res.try_next().await? {
            while let Some(row) = item.try_next().await? {
                worlds.push(World::new(row.get(0), row.get(1)))
            }
        }

        Ok(worlds)
    }

    pub async fn update(&self, num: u16) -> HandleResult<Vec<World>> {
        let len = num as usize;

        let update = self.updates.get(len).ok_or("request num is out of range")?;
        let mut conn = self.pool.get().await?;
        let world_stmt = WORLD_STMT.execute_mut(&mut conn).await?;
        let update_stmt = Statement::named(update, &[]).execute_mut(&mut conn).await?;

        let mut params = Vec::with_capacity(len);

        let mut res = {
            let (ref mut rng, ref mut buf) = *self.shared.borrow_mut();
            let mut pipe = Pipeline::with_capacity_from_buf(len + 1, buf);
            (0..num).try_for_each(|_| {
                let w_id = rng.gen_id();
                let r_id = rng.gen_id();
                params.push([w_id, r_id]);
                world_stmt.bind([w_id]).query_mut(&mut pipe)
            })?;
            update_stmt.bind(sort_update_params(&params)).query_mut(&mut pipe)?;
            pipe.query(&conn.consume())?
        };

        let mut worlds = Vec::with_capacity(len);

        let mut r_ids = params.into_iter();

        while let Some(mut item) = res.try_next().await? {
            while let Some(row) = item.try_next().await? {
                let r_id = r_ids.next().unwrap()[1];
                worlds.push(World::new(row.get(0), r_id))
            }
        }

        Ok(worlds)
    }

    pub async fn tell_fortune(&self) -> HandleResult<Fortunes> {
        let mut items = Vec::with_capacity(32);
        items.push(Fortune::new(0, "Additional fortune added at request time."));

        let mut conn = self.pool.get().await?;
        let stmt = FORTUNE_STMT.execute_mut(&mut conn).await?;
        let mut res = stmt.query(&conn.consume()).await?;

        while let Some(row) = res.try_next().await? {
            items.push(Fortune::new(row.get(0), row.get::<String>(1)));
        }

        items.sort_by(|it, next| it.message.cmp(&next.message));

        Ok(Fortunes::new(items))
    }
}

fn sort_update_params(params: &[[i32; 2]]) -> impl ExactSizeIterator<Item = i32> {
    let mut params = params.to_owned();
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
