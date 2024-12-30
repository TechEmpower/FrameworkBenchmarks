use futures_util::{stream::FuturesUnordered, TryStreamExt};
use rand::{rngs::SmallRng, SeedableRng, Rng, thread_rng};
use crate::models::{World, Fortune};


#[derive(Clone)]
pub struct Postgres(sqlx::PgPool);

impl Postgres {
    pub async fn new() -> Self {
        macro_rules! load_env {
            ($($name:ident as $t:ty)*) => {$(
                #[allow(non_snake_case)]
                let $name = ::std::env::var(stringify!($name))
                    .expect(concat!(
                        "Failed to load environment variable ",
                        "`", stringify!($name), "`"
                    ))
                    .parse::<$t>()
                    .unwrap();
            )*};
        } load_env! {
            MAX_CONNECTIONS as u32
            MIN_CONNECTIONS as u32
            DATABASE_URL    as String
        }
        
        let pool = sqlx::postgres::PgPoolOptions::new()
            .max_connections(MAX_CONNECTIONS)
            .min_connections(MIN_CONNECTIONS)
            .connect(&DATABASE_URL).await
            .unwrap();

        Self(pool)
    }
}

impl Postgres {
    pub async fn select_random_world(&self) -> World {
        let mut rng = SmallRng::from_rng(&mut thread_rng()).unwrap();
    
        sqlx::query_as("SELECT id, randomnumber FROM World WHERE id = $1")
            .bind((rng.gen::<u32>() % 10_000 + 1) as i32)
            .fetch_one(&self.0).await
            .expect("Failed to fetch a world")
    }
    
    pub async fn select_all_fortunes(&self) -> Vec<Fortune> {
        sqlx::query_as("SELECT id, message FROM Fortune")
            .fetch_all(&self.0).await
            .expect("Failed to fetch fortunes")
    }
    
    pub async fn select_n_random_worlds(&self, n: usize) -> Vec<World> {
        let mut rng = SmallRng::from_rng(&mut thread_rng()).unwrap();
    
        let selects = FuturesUnordered::new();
        for _ in 0..n {
            selects.push(
                sqlx::query_as("SELECT id, randomnumber FROM World WHERE id = $1")
                    .bind((rng.gen::<u32>() % 10_000 + 1) as i32)
                    .fetch_one(&self.0)
            )
        }
    
        selects.try_collect().await.expect("Failed to fetch worlds")
    }
    
    pub async fn update_random_ids_of_worlds(&self, worlds: &mut Vec<World>) {
        let mut rng = SmallRng::from_rng(&mut thread_rng()).unwrap();
    
        let updates = FuturesUnordered::new();
        for w in worlds {
            w.randomnumber = (rng.gen::<u32>() % 10_000 + 1) as i32;
            updates.push(
                sqlx::query(
                    "UPDATE World SET randomnumber = $1 WHERE id = $2")
                    .bind(w.randomnumber)
                    .bind(w.id)
                    .execute(&self.0)
            )
        }
    
        let _: sqlx::postgres::PgQueryResult = updates.try_collect().await.expect("Failed to fetch worlds");
    }
}
