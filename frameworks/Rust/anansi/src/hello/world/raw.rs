use crate::prelude::*;
use serde::Serialize;
use anansi::check;
use anansi::db::postgres::{PgDbRow, PgQuery};
use super::util::get_query;
use std::borrow::Cow;
use anansi::db::DbRow;
use rand::Rng;
use std::fmt::Write;
use tokio_postgres::types::ToSql;

thread_local!(static UPDATES: Vec<Cow<'static, str>> = {
    let mut updates = vec![Cow::from("")];
    for num in 1..=500u16 {
        let mut pl = 1;
        let mut q = "UPDATE world SET randomnumber = CASE id ".to_string();
        for _ in 1..=num {
            let _ = write!(q, "WHEN ${} THEN ${} ", pl, pl + 1);
            pl += 2;
        }

        q.push_str("ELSE randomnumber END WHERE id IN (");

        for _ in 1..=num {
            let _ = write!(q, "${},", pl);
            pl += 1;
        }

        q.pop();
        q.push(')');

        updates.push(Cow::from(q));
    }
    updates
});

fn random_num() -> i32 {
    rand::thread_rng().gen_range(1..=10_000)
}

#[derive(Copy, Clone, Serialize, Debug)]
pub struct World {
    id: i32,
    randomnumber: i32,
}

#[derive(Serialize, Debug)]
pub struct Fortune {
    id: i32,
    message: Cow<'static, str>,
}

#[base_view]
fn base<R: Request>(_req: &mut R) -> Result<Response> {}

#[viewer]
impl<R: Request> WorldView<R> {
    async fn get_world(req: &R) -> Result<PgDbRow> {
        PgQuery::new("SELECT * FROM world WHERE id = $1", &[&random_num()])
            .fetch_one(req)
            .await
    }
    async fn get_worlds(req: &R) -> Result<Vec<World>> {
        let q = get_query(req.params());
        let mut worlds = Vec::with_capacity(q as usize);
        for _ in 0..q {
            let row = Self::get_world(req).await?;
            let world = World {
                id: row.try_i32("id")?,
                randomnumber: row.try_i32("randomnumber")?,
            };
            worlds.push(world);
        }
        Ok(worlds)
    }
    #[check(Site::is_visitor)]
    pub async fn db(req: &mut R) -> Result<Response> {
        let row = Self::get_world(req).await?;
        let world = World {
            id: row.try_i32("id")?,
            randomnumber: row.try_i32("randomnumber")?,
        };
        Response::json(&world)
    }
    #[check(Site::is_visitor)]
    pub async fn queries(req: &mut R) -> Result<Response> {
        let worlds = Self::get_worlds(req).await?;
        Response::json(&worlds)
    }
    #[view(Site::is_visitor)]
    pub async fn raw_fortunes(req: &mut R) -> Result<Response> {
        let title = "Fortunes";
        let rows = PgQuery::new("SELECT * FROM fortune", &[])
            .fetch_all(req)
            .await?;
        let mut fortunes = vec![Fortune {
            id: 0,
            message: Cow::Borrowed("Additional fortune added at request time.")
        }];
        for row in rows {
            fortunes.push(Fortune {
                id: row.try_i32("id")?,
                message: Cow::Owned(row.try_string("message")?),
            })
        }
        fortunes.sort_by(|it, next| it.message.cmp(&next.message));
    }
    #[check(Site::is_visitor)]
    pub async fn updates(req: &mut R) -> Result<Response> {
        let q = get_query(req.params()) as usize;
        let mut worlds = Vec::with_capacity(q);
        let mut update = Cow::from("");
        UPDATES.with(|u| {
            update = u[q].clone();
        });
        let mut params: Vec<&(dyn ToSql + Sync)> = Vec::with_capacity(q * 3);
        for _ in 0..q {
            let row = Self::get_world(req).await?;
            let world = World {
                id: row.try_i32("id")?,
                randomnumber: random_num(),
            };
            worlds.push(world);
        }
        for world in &worlds {
            params.push(&world.id);
            params.push(&world.randomnumber);
        }
        for world in &worlds {
            params.push(&world.id);
        }
        PgQuery::new(&update, params.as_slice())
            .execute(req)
            .await?;
        Response::json(&worlds)
    }
}
