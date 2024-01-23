use crate::models::Fortune;
use crate::{async_trait, throw, Context, Result, Service, StdResult, World};
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use roa::http::StatusCode;
use roa_pg::types::ToSql;
use roa_pg::{connect, Client, Statement};
use std::collections::HashMap;
use std::fmt::Write;
use std::sync::Arc;

#[derive(Clone)]
pub struct State {
    client: Arc<Client>,
    queries: Arc<Queries>,
    rng: SmallRng,
}

pub struct Queries {
    fortune: Statement,
    world: Statement,
    updates: HashMap<usize, Statement>,
}

impl State {
    pub async fn bind(url: &str) -> StdResult<Self> {
        let (client, conn) = connect(&url.parse()?).await?;

        async_std::task::spawn(conn);

        let fortune = client.prepare("SELECT * FROM fortune").await?;
        let world = client.prepare("SELECT * FROM world WHERE id=$1").await?;
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
            updates.insert(num, client.prepare(&q).await?);
        }
        Ok(State {
            client: Arc::new(client),
            queries: Arc::new(Queries {
                fortune,
                world,
                updates,
            }),
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
        match self.client.query_opt(&self.queries.world, &[&wid]).await? {
            None => throw!(StatusCode::NOT_FOUND),
            Some(row) => Ok(World {
                id: row.get(0),
                randomnumber: row.get(1),
            }),
        }
    }

    #[inline]
    async fn fortunes(&self) -> Result<Vec<Fortune>> {
        let fortunes = self
            .client
            .query(&self.queries.fortune, &[])
            .await?
            .iter()
            .map(|row| Fortune {
                id: row.get(0),
                message: row.get(1),
            })
            .collect();
        Ok(fortunes)
    }

    #[inline]
    async fn update_worlds(&mut self) -> Result<Vec<World>> {
        let mut worlds = self.query_worlds().await?;
        let nums = worlds.len();
        let mut params: Vec<&(dyn ToSql + Sync)> = Vec::with_capacity(nums * 3);
        for w in worlds.iter_mut() {
            w.randomnumber = w.id;
        }
        for w in &worlds {
            params.push(&w.id);
            params.push(&w.randomnumber);
        }
        for w in &worlds {
            params.push(&w.id);
        }
        let statement = &self.queries.updates[&nums];
        self.client.execute(statement, &params).await?;
        Ok(worlds)
    }
}
