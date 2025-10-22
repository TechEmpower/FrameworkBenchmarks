#![cfg(feature = "rt_tokio")]

use crate::models::{World, Fortune};
use std::sync::Arc;
use futures_util::stream::{StreamExt, FuturesUnordered};
use rand::{rngs::SmallRng, SeedableRng, Rng, distributions::Uniform, thread_rng};

#[derive(Clone)]
pub struct Postgres {
    client:     Arc<tokio_postgres::Client>,
    statements: TechEmpowerStatements,
}

#[derive(Clone)]
struct TechEmpowerStatements {
    select_world_by_id:  tokio_postgres::Statement,
    select_all_fortunes: tokio_postgres::Statement,
    update_worlds:       tokio_postgres::Statement,
}

impl Postgres {
    pub async fn new() -> Self {
        let (client, connection) = tokio_postgres::connect(
            &std::env::var("DATABASE_URL").unwrap(),
            tokio_postgres::NoTls
        ).await.expect("failed to connect database");

        tokio::spawn(async {
            if let Err(e) = connection.await {
                eprintln!("error in database connection: {e}");
            }
        });
        
        let statements = TechEmpowerStatements {
            select_world_by_id: client
                .prepare("SELECT id, randomnumber FROM world WHERE id = $1 LIMIT 1")
                .await
                .unwrap(),
            select_all_fortunes: client
                .prepare("SELECT id, message FROM fortune")
                .await
                .unwrap(),
            update_worlds: client
                .prepare("\
                    UPDATE world SET randomnumber = new.randomnumber FROM ( \
                        SELECT * FROM UNNEST($1::int[], $2::int[]) AS v(id, randomnumber) \
                    ) AS new WHERE world.id = new.id \
                ")
                .await
                .unwrap(),
        };

        Self { client: Arc::new(client), statements }
    }
}

impl Postgres {
    const ID_RANGE: std::ops::Range<i32> = 1..10001;
    
    async fn select_random_world_by_id(&self, id: i32) -> World {
        let row = self.client
            .query_one(&self.statements.select_world_by_id, &[&id])
            .await
            .expect("failed to fetch a world");

        World {
            id:           row.get(0),
            randomnumber: row.get(1),
        }
    }
}

impl Postgres {
    pub async fn select_random_world(&self) -> World {
        let mut rng = SmallRng::from_rng(&mut thread_rng()).unwrap();
        self.select_random_world_by_id(rng.gen_range(Self::ID_RANGE)).await
    }
    
    pub async fn select_n_random_worlds(&self, n: usize) -> Vec<World> {
        let rng = SmallRng::from_rng(&mut thread_rng()).unwrap();

        let selects = FuturesUnordered::new();
        for id in rng.sample_iter(Uniform::new(Self::ID_RANGE.start, Self::ID_RANGE.end)).take(n) {
            selects.push(self.select_random_world_by_id(id))
        }

        selects.collect::<Vec<World>>().await
    }
    
    pub async fn select_all_fortunes(&self) -> Vec<Fortune> {
        let mut rows = std::pin::pin!(self
            .client
            .query_raw::<_, _, &[i32; 0]>(&self.statements.select_all_fortunes, &[])
            .await
            .expect("failed to fetch fortunes")
        );

        let mut fortunes = Vec::new();
        while let Some(row) = rows.next().await.transpose().unwrap() {
            fortunes.push(Fortune {
                id:      row.get(0),
                message: row.get(1),
            });
        }

        fortunes
    }
    
    pub async fn update_randomnumbers_of_n_worlds(&self, n: usize) -> Vec<World> {
        let rng = SmallRng::from_rng(&mut thread_rng()).unwrap();

        let mut worlds = self.select_n_random_worlds(n).await;

        let mut ids = Vec::with_capacity(n);
        let new_randomnumbers = rng
            .sample_iter(Uniform::new(Self::ID_RANGE.start, Self::ID_RANGE.end))
            .take(n)
            .collect::<Vec<_>>();
        for i in 0..n {
            worlds[i].randomnumber = new_randomnumbers[i];
            ids.push(worlds[i].id);
        }

        self.client
            .execute(&self.statements.update_worlds, &[&ids, &new_randomnumbers])
            .await
            .expect("failed to update worlds");

        worlds
    }
}
