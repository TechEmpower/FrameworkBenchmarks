use crate::prelude::*;
use crate::hello::middleware::Pg;
use serde::Serialize;
use anansi::check;
use super::util::get_query;
use rand::Rng;
use tokio_postgres::types::ToSql;

fn random_num() -> i32 {
    rand::thread_rng().gen_range(1..=10_000)
}

#[derive(Copy, Clone, Serialize, Debug)]
pub struct World {
    id: i32,
    randomnumber: i32,
}

#[derive(Serialize, Debug)]
pub struct Fortune<'a> {
    id: i32,
    message: &'a str,
}

#[base_view]
fn base<R: Request>(_req: &mut R) -> Result<Response> {}

#[viewer]
impl<R: Request + Pg> WorldView<R> {
    async fn one_world(req: &R) -> Result<World> {
        let row = req.get_world().await?;
        let world = World {
            id: row.get_i32(0),
            randomnumber: row.get_i32(1),
        };
        Ok(world)
    }
    async fn get_worlds(req: &R) -> Result<Vec<World>> {
        let q = get_query(req.params());
        let mut worlds = Vec::with_capacity(q as usize);
        for _ in 0..q {
            let world = Self::one_world(req).await?;
            worlds.push(world);
        }
        Ok(worlds)
    }
    #[check(Site::is_visitor)]
    pub async fn db(req: &mut R) -> Result<Response> {
        let world = Self::one_world(req).await?;
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
            message: "Additional fortune added at request time.",
        });
        fortunes.extend(rows.iter().map(|row| Fortune {
            id: row.get(0),
            message: row.get(1),
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
                id: row.get_i32(0),
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
        req.update_worlds(q - 1, params.as_slice()).await?;
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
