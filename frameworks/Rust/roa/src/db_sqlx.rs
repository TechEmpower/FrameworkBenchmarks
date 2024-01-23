use crate::models::Fortune;
use crate::{async_trait, throw, Context, Result, Service, StdResult, World};
use futures::TryStreamExt;
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use roa::http::StatusCode;
use sqlx::{PgPool, Row};
use std::collections::HashMap;
use std::fmt::Write;
use std::sync::Arc;

#[derive(Clone)]
pub struct State {
    client: PgPool,
    updates: Arc<HashMap<usize, String>>,
    rng: SmallRng,
}

impl State {
    pub async fn bind(url: &str) -> StdResult<Self> {
        let client = PgPool::new(url).await?;
        let mut updates = HashMap::new();
        for num in 1..=500 {
            let mut pl = 1;
            let mut q = String::new();
            q.push_str("UPDATE world SET randomnumber = CASE id ");
            for _ in 1..=num {
                write!(&mut q, "when ${} then ${} ", pl, pl + 1)?;
                pl += 2;
            }
            q.push_str("ELSE randomnumber END WHERE id IN (");
            for _ in 1..=num {
                write!(&mut q, "${},", pl)?;
                pl += 1;
            }
            q.pop();
            q.push(')');
            updates.insert(num, q);
        }
        Ok(State {
            client,
            updates: Arc::new(updates),
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
        match sqlx::query("SELECT * FROM world WHERE id=$1")
            .bind(wid)
            .fetch_optional(&mut &self.client)
            .await?
        {
            None => throw!(StatusCode::NOT_FOUND),
            Some(row) => Ok(World {
                id: row.get(0),
                randomnumber: row.get(1),
            }),
        }
    }

    #[inline]
    async fn fortunes(&self) -> Result<Vec<Fortune>> {
        let fortunes: Vec<_> = sqlx::query("SELECT * FROM fortune")
            .fetch(&mut &self.client)
            .map_ok(|row| Fortune {
                id: row.get(0),
                message: row.get(1),
            })
            .try_collect()
            .await?;
        Ok(fortunes)
    }

    #[inline]
    async fn update_worlds(&mut self) -> Result<Vec<World>> {
        let mut worlds = self.query_worlds().await?;
        let mut query = sqlx::query(&self.updates[&worlds.len()]);
        for w in worlds.iter_mut() {
            w.randomnumber = w.id;
            query = query.bind(w.id).bind(w.randomnumber);
        }
        for w in &worlds {
            query = query.bind(w.id);
        }
        query.execute(&mut &self.client).await?;
        Ok(worlds)
    }
}
