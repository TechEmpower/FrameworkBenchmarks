use std::{borrow::Cow, fmt::Write, io, sync::Arc};

use futures_util::{stream::FuturesUnordered, TryFutureExt, TryStreamExt};
use nanorand::{Rng, WyRand};
use tokio_postgres::{connect, types::ToSql, Client, NoTls, Statement};
use viz::{Error, IntoResponse, Response, StatusCode};

use crate::models::{Fortune, World};
use crate::utils::RANGE;

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
        Error::Responder(e.into_response())
    }
}

impl IntoResponse for PgError {
    fn into_response(self) -> Response {
        (StatusCode::INTERNAL_SERVER_ERROR, self.to_string()).into_response()
    }
}

/// Postgres interface
pub struct PgConnection {
    rng: WyRand,
    client: Client,
    world: Statement,
    fortune: Statement,
    updates: Vec<Statement>,
}

impl PgConnection {
    pub async fn connect(db_url: &str) -> Self {
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
        let mut updates = Vec::new();

        for num in 1..=500u16 {
            let mut pl = 1;
            let mut q = String::new();

            q.push_str("UPDATE world SET randomnumber = CASE id ");

            for _ in 1..=num {
                let _ = write!(q, "when ${} then ${} ", pl, pl + 1);
                pl += 2;
            }

            q.push_str("ELSE randomnumber END WHERE id IN (");

            for _ in 1..=num {
                let _ = write!(q, "${},", pl);
                pl += 1;
            }

            q.pop();
            q.push(')');

            updates.push(client.prepare(&q).await.unwrap());
        }

        let world = client
            .prepare("SELECT * FROM world WHERE id = $1")
            .await
            .unwrap();

        PgConnection {
            rng: WyRand::new(),
            world,
            client,
            fortune,
            updates,
        }
    }
}

impl PgConnection {
    async fn query_one_world(&self, id: i32) -> Result<World, PgError> {
        let row = self.client.query_one(&self.world, &[&id]).await?;
        Ok(World {
            id: row.get(0),
            randomnumber: row.get(1),
        })
    }

    pub async fn get_world(&self) -> Result<World, PgError> {
        let random_id = self.rng.clone().generate_range(RANGE);
        self.query_one_world(random_id).await
    }

    pub async fn get_worlds(&self, num: u16) -> Result<Vec<World>, PgError> {
        let mut rng = self.rng.clone();

        let worlds = FuturesUnordered::new();

        for _ in 0..num {
            let id = rng.generate_range(RANGE);
            worlds.push(self.query_one_world(id));
        }

        worlds.try_collect().await
    }

    pub async fn get_worlds_by_limit(&self, limit: i64) -> Result<Vec<World>, PgError> {
        Ok(self
            .client
            .query("SELECT * FROM world LIMIT $1", &[&limit])
            .await?
            .iter()
            .map(|row| World {
                id: row.get(0),
                randomnumber: row.get(1),
            })
            .collect())
    }

    pub async fn update(&self, num: u16) -> Result<Vec<World>, PgError> {
        let mut rng = self.rng.clone();

        let worlds = FuturesUnordered::new();

        for _ in 0..num {
            let id = rng.generate_range(RANGE);
            let rid = rng.generate_range(RANGE);

            worlds.push(self.query_one_world(id).map_ok(move |mut world| {
                world.randomnumber = rid;
                world
            }));
        }

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

        self.client.query(&self.updates[num - 1], &params).await?;

        Ok(worlds)
    }

    pub async fn tell_fortune(&self) -> Result<Vec<Fortune>, PgError> {
        let mut items = self
            .client
            .query(&self.fortune, &[])
            .await?
            .iter()
            .map(|row| Fortune {
                id: row.get(0),
                message: Cow::Owned(row.get(1)),
            })
            .collect::<Vec<_>>();

        items.push(Fortune {
            id: 0,
            message: Cow::Borrowed("Additional fortune added at request time."),
        });

        items.sort_by(|it, next| it.message.cmp(&next.message));

        Ok(items)
    }
}

pub fn get_conn(pool: Option<Arc<PgConnection>>) -> Result<Arc<PgConnection>, PgError> {
    pool.ok_or(PgError::Connect)
}
