use std::{collections::HashMap, fmt::Write, io, sync::Arc};

use futures_util::{StreamExt, TryFutureExt, TryStreamExt, stream::FuturesUnordered};
use rand::{Rng, SeedableRng, rng, rngs::SmallRng};
use tokio::pin;
use tokio_postgres::{Client, NoTls, Statement, connect, types::ToSql};
use vidi::{Error, IntoResponse, Response, StatusCode};

use crate::models::{Fortune, World};

/// Postgres Error
#[derive(Debug, thiserror::Error)]
pub enum PgError {
    #[error("connect to database was failed")]
    Connect,
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error(transparent)]
    Pg(#[from] tokio_postgres::Error),
}

impl From<PgError> for Error {
    fn from(e: PgError) -> Self {
        Error::Responder(Box::new(e.into_response()))
    }
}

impl IntoResponse for PgError {
    fn into_response(self) -> Response {
        (StatusCode::INTERNAL_SERVER_ERROR, self.to_string()).into_response()
    }
}

/// Postgres interface
pub struct PgConnection {
    client: Client,
    world: Statement,
    fortune: Statement,
    updates: HashMap<u16, Statement>,
}

impl PgConnection {
    pub async fn connect(db_url: &str) -> Arc<Self> {
        let (client, conn) = connect(db_url, NoTls)
            .await
            .expect("can not connect to postgresql");

        // Spawn connection
        tokio::spawn(async move {
            if let Err(error) = conn.await {
                eprintln!("Connection error: {error}");
            }
        });

        let fortune = client.prepare("SELECT * FROM fortune").await.unwrap();
        let mut updates = HashMap::new();

        for num in 1..=500u16 {
            let mut pl = 1;
            let mut q = String::new();

            q.push_str("UPDATE world SET randomnumber = CASE id ");

            for _ in 1..=num {
                let _ = write!(q, "when ${pl} then ${} ", pl + 1);
                pl += 2;
            }

            q.push_str("ELSE randomnumber END WHERE id IN (");

            for _ in 1..=num {
                let _ = write!(q, "${pl},");
                pl += 1;
            }

            q.pop();
            q.push(')');

            updates.insert(num, client.prepare(&q).await.unwrap());
        }

        let world = client
            .prepare("SELECT * FROM world WHERE id = $1")
            .await
            .unwrap();

        Arc::new(PgConnection {
            world,
            client,
            fortune,
            updates,
        })
    }
}

impl PgConnection {
    async fn query_one_world(&self, id: i32) -> Result<World, PgError> {
        let stream = self.client.query_raw(&self.world, &[&id]).await?;
        pin!(stream);
        let row = stream.next().await.unwrap()?;
        Ok(World {
            id: row.get(0),
            randomnumber: row.get(1),
        })
    }

    pub async fn get_world(&self) -> Result<World, PgError> {
        let mut rng = SmallRng::from_rng(&mut rng());
        let random_id = (rng.random::<u32>() % 10_000 + 1) as i32;

        self.query_one_world(random_id).await
    }

    pub async fn get_worlds(&self, num: u16) -> Result<Vec<World>, PgError> {
        let mut rng = SmallRng::from_rng(&mut rng());

        let worlds = FuturesUnordered::new();

        for _ in 0..num {
            let id = (rng.random::<u32>() % 10_000 + 1) as i32;
            worlds.push(self.query_one_world(id));
        }

        worlds.try_collect().await
    }

    pub async fn update(&self, num: u16) -> Result<Vec<World>, PgError> {
        let mut rng = SmallRng::from_rng(&mut rng());

        let worlds = FuturesUnordered::new();

        for _ in 0..num {
            let id = (rng.random::<u32>() % 10_000 + 1) as i32;
            let rid = (rng.random::<u32>() % 10_000 + 1) as i32;

            worlds.push(self.query_one_world(id).map_ok(move |mut world| {
                world.randomnumber = rid;
                world
            }));
        }

        let st = self.updates.get(&num).unwrap();
        let worlds = worlds.try_collect::<Vec<World>>().await?;

        let num = num as usize;
        let mut params = Vec::<&(dyn ToSql + Sync)>::with_capacity(num * 3);

        for w in &worlds {
            params.push(&w.id);
            params.push(&w.randomnumber);
        }

        for w in &worlds {
            params.push(&w.id);
        }

        self.client.query(st, &params).await?;

        Ok(worlds)
    }

    pub async fn tell_fortune(&self) -> Result<Vec<Fortune>, PgError> {
        let mut items = vec![Fortune {
            id: 0,
            message: "Additional fortune added at request time.".to_string(),
        }];

        let stream = self
            .client
            .query_raw::<_, _, &[i32; 0]>(&self.fortune, &[])
            .await?;
        pin!(stream);

        while let Some(row) = stream.next().await {
            let row = row?;

            items.push(Fortune {
                id: row.get(0),
                message: row.get(1),
            });
        }

        items.sort_by(|it, next| it.message.cmp(&next.message));

        Ok(items)
    }
}

pub fn get_conn(pool: Option<Arc<PgConnection>>) -> Result<Arc<PgConnection>, PgError> {
    pool.ok_or(PgError::Connect)
}
