use serde::Serialize;
use tokio_postgres::{Client, Error, NoTls, Statement};

const DATABASE_URL: &str = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

pub struct Database {
    client: Client,
    world: Statement,
    fortune: Statement,
}

#[derive(Serialize)]
pub struct World {
    pub id: i32,
    pub randomnumber: i32,
}

pub struct Fortune {
    pub id: i32,
    pub message: String,
}

impl Database {
    pub async fn connect() -> Result<Self, Error> {
        let (client, connection) = tokio_postgres::connect(DATABASE_URL, NoTls).await?;
        tokio::spawn(async { connection.await.unwrap() });
        let world = client
            .prepare("SELECT id, randomnumber FROM world WHERE id=$1")
            .await?;
        let fortune = client.prepare("SELECT id, message FROM fortune").await?;
        Ok(Self {
            client,
            world,
            fortune,
        })
    }

    pub async fn get_world_by_id(&self, id: i32) -> World {
        let row = self.client.query_one(&self.world, &[&id]).await.unwrap();
        World {
            id: row.get(0),
            randomnumber: row.get(1),
        }
    }

    pub async fn query_fortunes(&self) -> Vec<Fortune> {
        self.client
            .query(&self.fortune, &[])
            .await
            .unwrap()
            .into_iter()
            .map(|row| Fortune {
                id: row.get(0),
                message: row.get(1),
            })
            .collect()
    }
}
