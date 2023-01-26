use crate::prelude::*;
use anansi::cache::prelude::*;
use anansi::records::Int;
use super::super::records::{World, Fortune};
use super::util::get_query;
use serde::Serialize;
use anansi::{check, raw_bulk_update};
use rand::Rng;

fn random_i32() -> i32 {
    rand::thread_rng().gen_range(1..=10_000)
}

fn random_int() -> Int {
    Int::new(random_i32())
}

#[derive(Serialize)]
struct Message {
    message: &'static str,
}

#[base_view]
fn base<R: Request>(_req: &mut R) -> Result<Response> {}

#[viewer]
impl<R: Request> WorldView<R> {
    #[check(Site::is_visitor)]
    pub async fn json(req: &mut R) -> Result<Response> {
        let message = Message {message: "Hello, World!"};
        Response::json(&message)
    }
    async fn get_world(req: &R) -> Result<World> {
        World::find(random_int()).get(req).await
    }
    async fn get_worlds(req: &R) -> Result<Vec<World>> {
        let q = get_query(req.params());
        let mut worlds = Vec::with_capacity(q as usize);
        for _ in 0..q {
            let world = Self::get_world(req).await?;
            worlds.push(world);
        }
        Ok(worlds)
    }
    #[check(Site::is_visitor)]
    pub async fn db(req: &mut R) -> Result<Response> {
        let world = Self::get_world(req).await?;
        Response::json(&world)
    }
    #[check(Site::is_visitor)]
    pub async fn queries(req: &mut R) -> Result<Response> {
        let worlds = Self::get_worlds(req).await?;
        Response::json(&worlds)
    }
    #[view(Site::is_visitor)]
    pub async fn fortunes(req: &mut R) -> Result<Response> {
        let title = "Fortunes";
        let mut fortunes = Fortune::get_all().query(req).await?;
        fortunes.push(Fortune::additional());
        fortunes.sort_by(|it, next| it.message.cmp(&next.message));
    }
    #[check(Site::is_visitor)]
    pub async fn updates(req: &mut R) -> Result<Response> {
        let mut worlds = Self::get_worlds(req).await?;
        for world in &mut worlds {
            world.randomNumber = random_int();
        }
        transact!(req, raw_bulk_update!(req, World, &worlds, randomNumber).await)?;
        Response::json(&worlds)
    }
    #[check(Site::is_visitor)]
    pub async fn plaintext(req: &mut R) -> Result<Response> {
        Ok(Response::text("Hello, World!".to_string()))
    }
    #[check(Site::is_visitor)]
    pub async fn cached_queries(req: &mut R) -> Result<Response> {
        let q = get_query(req.params());
        let mut ids = vec![];
        for _ in 0..q {
            ids.push(random_i32().to_string());
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
