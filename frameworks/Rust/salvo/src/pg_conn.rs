use std::collections::HashMap;
use std::fmt::Write;
use std::io;

use futures_util::stream::{FuturesUnordered, TryStreamExt};
use rand::distributions::{Distribution, Uniform};
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use tokio_postgres::types::ToSql;
use tokio_postgres::{self, Client, NoTls, Statement};

use crate::models::*;

type DbResult<T> = Result<T, tokio_postgres::Error>;

pub struct PgConnection {
    client: Client,
    #[allow(dead_code)]
    fortune: Statement,
    world: Statement,
    #[allow(dead_code)]
    updates: HashMap<u16, Statement>,
}

impl PgConnection {
    pub async fn create(db_url: &str) -> Result<PgConnection, io::Error> {
        let (client, conn) = tokio_postgres::connect(db_url, NoTls)
            .await
            .expect("can not connect to postgresql");
        tokio::spawn(async move {
            if let Err(e) = conn.await {
                eprintln!("connection error: {}", e);
            }
        });

        let fortune = client
            .prepare("SELECT id, message FROM fortune")
            .await
            .unwrap();
        let world = client
            .prepare("SELECT * FROM world WHERE id=$1")
            .await
            .unwrap();
        let mut updates = HashMap::new();
        for num in 1..=500u16 {
            let mut pl: u16 = 1;
            let mut q = String::new();
            q.push_str("UPDATE world SET randomnumber = CASE id ");
            for _ in 1..=num {
                let _ = write!(&mut q, "when ${} then ${} ", pl, pl + 1);
                pl += 2;
            }
            q.push_str("ELSE randomnumber END WHERE id IN (");
            for _ in 1..=num {
                let _ = write!(&mut q, "${},", pl);
                pl += 1;
            }
            q.pop();
            q.push(')');
            updates.insert(num, client.prepare(&q).await.unwrap());
        }

        Ok(PgConnection {
            client,
            fortune,
            world,
            updates,
        })
    }

    async fn query_one_world(&self, w_id: i32) -> DbResult<World> {
        self.client
            .query_one(&self.world, &[&w_id])
            .await
            .map(|row| World {
                id: row.get(0),
                randomnumber: row.get(1),
            })
    }

    #[allow(dead_code)]
    pub async fn get_world(&self) -> DbResult<World> {
        let mut rng = SmallRng::from_entropy();
        let id: i32 = rng.gen_range(1..10_001);
        self.query_one_world(id).await
    }
    pub async fn get_worlds(&self, count: u16) -> DbResult<Vec<World>> {
        let worlds = {
            let mut rng = SmallRng::from_entropy();
            let between = Uniform::from(1..10_001);
            (0..count)
                .map(|_| {
                    let id: i32 = between.sample(&mut rng);
                    self.query_one_world(id)
                })
                .collect::<FuturesUnordered<_>>()
        };

        worlds.try_collect().await
    }

    #[allow(dead_code)]
    pub async fn update(&self, count: u16) -> DbResult<Vec<World>> {
        let worlds = {
            let mut rng = SmallRng::from_entropy();
            let between = Uniform::from(1..10_001);
            (0..count)
                .map(|_| {
                    let id: i32 = between.sample(&mut rng);
                    let w_id: i32 = between.sample(&mut rng);
                    async move {
                        let mut world = self.query_one_world(w_id).await?;
                        world.randomnumber = id;
                        Ok(world)
                    }
                })
                .collect::<FuturesUnordered<_>>()
        };

        let worlds = worlds.try_collect::<Vec<_>>().await?;
        let mut params = Vec::<&(dyn ToSql + Sync)>::with_capacity(count as usize * 3);
        for w in &worlds {
            params.push(&w.id);
            params.push(&w.randomnumber);
        }
        for w in &worlds {
            params.push(&w.id);
        }

        let st = self.updates.get(&count).unwrap();
        self.client.query(st, params.as_slice()).await?;
        Ok(worlds)
    }

    #[allow(dead_code)]
    pub async fn tell_fortune(&self) -> DbResult<FortunesTemplate> {
        let mut items = self
            .client
            .query(&self.fortune, &[])
            .await?
            .iter()
            .map(|row| Fortune {
                id: row.get(0),
                message: row.get(1),
            })
            .collect::<Vec<_>>();
        items.push(Fortune {
            id: 0,
            message: "Additional fortune added at request time.".to_string(),
        });
        items.sort_by(|it, next| it.message.cmp(&next.message));
        Ok(FortunesTemplate { items })
    }
}

markup::define! {
    FortunesTemplate(items: Vec<Fortune>) {
        {markup::doctype()}
        html {
            head {
                title { "Fortunes" }
            }
            body {
                table {
                    tr { th { "id" } th { "message" } }
                    @for item in items {
                        tr {
                            td { {item.id} }
                            td { {markup::raw(v_htmlescape::escape(&item.message).to_string())} }
                        }
                    }
                }
            }
        }
    }
}