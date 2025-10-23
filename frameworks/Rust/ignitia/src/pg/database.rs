use super::models::{Fortune, World};
use crate::common::{random_id, random_ids};
use futures::StreamExt;
use rand::{SeedableRng, rng, rngs::SmallRng};
use std::{borrow::Cow, sync::Arc};
use tokio::pin;
use tokio_postgres::{Client, NoTls, Statement, connect};

pub struct PgConnection {
    client: Client,
    fortune: Statement,
    world: Statement,
    updates: Statement,
}

impl PgConnection {
    pub async fn connect(db_url: String) -> Arc<PgConnection> {
        let (cl, conn) = connect(&db_url, NoTls)
            .await
            .expect("cannot connect to postgresql");

        tokio::spawn(async move {
            if let Err(error) = conn.await {
                eprintln!("database connection error: {error}");
            }
        });

        let fortune = cl
            .prepare(crate::common::SELECT_ALL_FORTUNES)
            .await
            .unwrap();
        let world = cl.prepare(crate::common::SELECT_WORLD_BY_ID).await.unwrap();
        let updates = cl.prepare(crate::common::UPDATE_WORLDS).await.unwrap();

        Arc::new(PgConnection {
            client: cl,
            fortune,
            world,
            updates,
        })
    }

    pub async fn fetch_world_by_id(&self, id: i32) -> Result<World, tokio_postgres::Error> {
        self.client
            .query_one(&self.world, &[&id])
            .await
            .map(|row| World {
                id: row.get(0),
                randomnumber: row.get(1),
            })
    }

    pub async fn fetch_random_worlds(
        &self,
        num: usize,
    ) -> Result<Vec<World>, tokio_postgres::Error> {
        let mut rng = SmallRng::from_rng(&mut rng());
        let mut worlds = Vec::with_capacity(num);

        for id in random_ids(&mut rng, num) {
            worlds.push(self.fetch_world_by_id(id).await?);
        }
        Ok(worlds)
    }

    pub async fn update_worlds(&self, num: usize) -> Result<Vec<World>, tokio_postgres::Error> {
        let mut worlds = self.fetch_random_worlds(num).await?;
        let mut rng = SmallRng::from_rng(&mut rng());
        let mut ids = Vec::with_capacity(num);
        let mut nids = Vec::with_capacity(num);

        for w in &mut worlds {
            w.randomnumber = random_id(&mut rng);
            ids.push(&w.id);
            nids.push(&w.randomnumber);
        }

        self.client.execute(&self.updates, &[&ids, &nids]).await?;
        Ok(worlds)
    }

    pub async fn fetch_all_fortunes(&self) -> Result<Vec<Fortune>, tokio_postgres::Error> {
        let mut fortunes = vec![Fortune {
            id: 0,
            message: Cow::Borrowed("Additional fortune added at request time."),
        }];

        let rows = self
            .client
            .query_raw::<_, _, &[i32; 0]>(&self.fortune, &[])
            .await?;
        pin!(rows);

        while let Some(row) = rows.next().await.transpose()? {
            fortunes.push(Fortune {
                id: row.get(0),
                message: Cow::Owned(row.get(1)),
            });
        }

        fortunes.sort_by(|it, next| it.message.cmp(&next.message));
        Ok(fortunes)
    }
}
