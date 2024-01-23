use diesel::pg::PgConnection;
use diesel::prelude::*;
use diesel::r2d2::ConnectionManager;
use roa::http::StatusCode;
use roa_diesel::preload::*;
use roa_diesel::Pool;

use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};

use crate::models::Fortune;
use crate::{async_trait, throw, Context, Result, Service, StdResult, World};
use futures::stream::{FuturesUnordered, TryStreamExt};

#[derive(Clone)]
pub struct State {
    pool: Pool<PgConnection>,
    rng: SmallRng,
}

impl AsRef<Pool<PgConnection>> for State {
    #[inline]
    fn as_ref(&self) -> &Pool<PgConnection> {
        &self.pool
    }
}

impl State {
    pub async fn bind(pg_url: &str) -> StdResult<Self> {
        let pool = Pool::builder()
            .max_size(50)
            .build(ConnectionManager::<PgConnection>::new(pg_url))?;
        Ok(Self {
            pool,
            rng: SmallRng::from_entropy(),
        })
    }
}

#[async_trait(?Send)]
impl Service for Context<State> {
    #[inline]
    fn random_id(&mut self) -> i32 {
        self.rng.gen_range(0, 10_001)
    }

    #[inline]
    fn get_queries(&self) -> usize {
        use std::cmp::{max, min};
        let query = self.uri().query();
        let nums = query
            .and_then(|query| Some((query, query.find("q")?)))
            .and_then(|(query, pos)| query.split_at(pos + 2).1.parse().ok())
            .unwrap_or(1);
        min(500, max(1, nums))
    }

    #[inline]
    async fn query_world(&self, wid: i32) -> Result<World> {
        use crate::schema::world::dsl::*;
        let data = self.first(world.filter(id.eq(wid))).await?;
        match data {
            None => throw!(StatusCode::NOT_FOUND),
            Some(item) => Ok(item),
        }
    }

    #[inline]
    async fn fortunes(&self) -> Result<Vec<Fortune>> {
        use crate::schema::fortune::dsl::*;
        Ok(self.load_data(fortune).await?)
    }

    #[inline]
    async fn update_worlds(&mut self) -> Result<Vec<World>> {
        let worlds = FuturesUnordered::new();
        let random_ids: Vec<_> =
            (0..self.get_queries()).map(|_| self.random_id()).collect();
        for wid in random_ids {
            worlds.push(update_world(self, wid));
        }
        worlds.try_collect().await
    }
}

async fn update_world(ctx: &Context<State>, wid: i32) -> Result<World> {
    use crate::schema::world::dsl::*;
    let mut data = ctx.query_world(wid).await?;
    data.randomnumber = wid;
    ctx.execute(
        diesel::update(world)
            .filter(id.eq(wid))
            .set(randomnumber.eq(wid)),
    )
    .await?;
    Ok(data)
}
