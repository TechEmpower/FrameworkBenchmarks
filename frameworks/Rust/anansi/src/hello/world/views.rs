use crate::prelude::*;
use super::super::records::{World, Fortune};
use serde::Serialize;
use anansi::check;
use anansi::web::Parameters;
use anansi::records::BigInt;
use rand::Rng;

#[derive(Serialize)]
struct Message {
    message: &'static str,
}

fn get_query(params: &Parameters) -> i16 {
    if let Ok(q) = params.get("queries") {
        if let Ok(q) = q.parse() {
            if q > 1 {
                return if q <= 500 {
                    q
                } else {
                    500
                };
            }
        } 
    }
    1
}

#[base_view]
fn base<R: Request>(_req: &mut R) -> Result<Response> {}

#[viewer]
impl<R: Request> WorldView<R> {
    #[check(Group::is_visitor)]
    pub async fn json(req: &mut R) -> Result<Response> {
        let message = Message {message: "Hello, World!"};
        Response::json(&message)
    }
    async fn get_world(req: &R) -> Result<World> {
        let random_id = rand::thread_rng().gen_range(1..=10_000);
        World::find(BigInt::new(random_id)).get(req).await
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
    #[check(Group::is_visitor)]
    pub async fn db(req: &mut R) -> Result<Response> {
        let world = Self::get_world(req).await?;
        Response::json(&world)
    }
    #[check(Group::is_visitor)]
    pub async fn queries(req: &mut R) -> Result<Response> {
        let worlds = Self::get_worlds(req).await?;
        Response::json(&worlds)
    }
    #[view(Group::is_visitor)]
    pub async fn fortunes(req: &mut R) -> Result<Response> {
        let title = "Fortunes";
        let mut fortunes = Fortune::get_all().query(req).await?;
        fortunes.push(Fortune::additional());
        fortunes.sort_by(|it, next| it.message.cmp(&next.message));
    }
    #[check(Group::is_visitor)]
    pub async fn updates(req: &mut R) -> Result<Response> {
        let mut worlds = Self::get_worlds(req).await?;
        transact!(req, {
            for world in &mut worlds {
                world.randomNumber = BigInt::new(rand::thread_rng().gen_range(1..=10_000));
                world.save(req).await?;
            }
            Ok(())
        })?;
        Response::json(&worlds)
    }
    #[check(Group::is_visitor)]
    pub async fn plaintext(req: &mut R) -> Result<Response> {
        Ok(Response::text("Hello, World!".to_string()))
    }
}
