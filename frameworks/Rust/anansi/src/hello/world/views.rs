use crate::prelude::*;
use super::super::records::{World, Fortune};
use serde::Serialize;
use anansi::check;
use anansi::web::Parameters;
use anansi::records::Int;
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

fn random_num() -> Int {
    Int::new(rand::thread_rng().gen_range(1..=10_000))
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
        World::find(random_num()).get(req).await
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
        transact!(req, {
            for world in &mut worlds {
                world.randomNumber = random_num();
                world.save(req).await?;
            }
            Ok(())
        })?;
        Response::json(&worlds)
    }
    #[check(Site::is_visitor)]
    pub async fn plaintext(req: &mut R) -> Result<Response> {
        Ok(Response::text("Hello, World!".to_string()))
    }
}
