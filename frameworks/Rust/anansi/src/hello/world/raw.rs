use crate::prelude::*;
use serde::Serialize;
use anansi::{check, prep};
use super::util::get_query;
use std::borrow::Cow;
use anansi::db::DbRow;
use rand::Rng;
use std::fmt::Write;
use tokio_postgres::types::ToSql;

fn update_statement(num: u16) -> String {
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
    q
}

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
    async fn get_worlds(req: &R) -> Result<Vec<World>> {
        let q = get_query(req.params());
        let mut worlds = Vec::with_capacity(q as usize);
        for _ in 0..q {
            let row = req.get_world().await?;
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
        let row = req.get_world().await?;
        let world = World {
            id: row.get_i32(0),
            randomnumber: row.get_i32(1),
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
        let rows = req.get_fortunes().await?;
        let mut fortunes = Vec::with_capacity(rows.len() + 1);
        fortunes.push(Fortune {
            id: 0,
            message: Cow::Borrowed("Additional fortune added at request time.")
        });
        fortunes.extend(rows.iter().map(|row| Fortune {
            id: row.get(0),
            message: Cow::Owned(row.get(1)),
        }));
        fortunes.sort_by(|it, next| it.message.cmp(&next.message));
    }
    #[check(Site::is_visitor)]
    pub async fn updates(req: &mut R) -> Result<Response> {
        let q = get_query(req.params()) as usize;
        let mut worlds = Vec::with_capacity(q);
        let mut params: Vec<&(dyn ToSql + Sync)> = Vec::with_capacity(q * 3);
        for _ in 0..q {
            let row = req.get_world().await?;
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
        prep!(req, format!("update{}", q), update_statement(q as u16), params.as_slice(), execute)?;
        Response::json(&worlds)
    }
    #[check(Site::is_visitor)]
    pub async fn cached_queries(req: &mut R) -> Result<Response> {
        let q = get_query(req.params());
        let mut ids = vec![];
        for _ in 0..q {
            ids.push(random_num().to_string());
        }
        let mut worlds = vec!['[' as u8];
        for mut world in req.cache().get_many(ids).await? {
            worlds.append(&mut world);
        }
        worlds.pop();
        worlds.push(']' as u8);
        Response::json_bytes(worlds)
    }
}
