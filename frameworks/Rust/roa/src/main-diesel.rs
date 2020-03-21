#[macro_use]
extern crate diesel;

use askama::Template;
use diesel::pg::PgConnection;
use diesel::prelude::*;
use futures::future::join_all;
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use roa::diesel::{Pool};
use roa::http::header::SERVER;
use roa::http::StatusCode;
use roa::preload::*;
use roa::query::query_parser;
use roa::router::{get, Router};
use roa::{async_trait, throw, App, Context, Next, Result};
use std::error::Error as StdError;
use std::result::Result as StdResult;
mod models;
mod schema;
pub mod utils;
use crate::models::*;
use diesel::r2d2::ConnectionManager;
use utils::{POSTGRES_URI, SERVER_HEADER};

#[derive(Template)]
#[template(path = "fortune.html")]
pub struct Fortunes<'a> {
    pub items: &'a [Fortune],
}

#[derive(Clone)]
struct State {
    pool: Pool<PgConnection>,
    pub rng: SmallRng,
}

impl AsRef<Pool<PgConnection>> for State {
    #[inline]
    fn as_ref(&self) -> &Pool<PgConnection> {
        &self.pool
    }
}

impl State {
    fn new(pool: Pool<PgConnection>) -> Self {
        Self {
            pool,
            rng: SmallRng::from_entropy(),
        }
    }
}

#[async_trait(?Send)]
trait Service {
    fn random_id(&mut self) -> i32;
    fn get_queries(&self) -> i32;
    async fn query_world(&self, wid: i32) -> Result<World>;
    async fn update_world(&self, wid: i32) -> Result<World>;
}

#[async_trait(?Send)]
impl Service for Context<State> {
    #[inline]
    fn random_id(&mut self) -> i32 {
        self.rng.gen_range(0, 10_001)
    }

    #[inline]
    fn get_queries(&self) -> i32 {
        use std::cmp::{max, min};
        let nums = self
            .query("q")
            .and_then(|var| var.parse().ok())
            .unwrap_or(1);
        min(500, max(1, nums))
    }

    #[inline]
    async fn query_world(&self, wid: i32) -> Result<World> {
        use crate::schema::world::dsl::*;
        let data = self.first(world.filter(id.eq(wid))).await?;
        match data {
            None => throw!(StatusCode::NOT_FOUND),
            Some(item) => Ok(item),
        }
    }

    #[inline]
    async fn update_world(&self, wid: i32) -> Result<World> {
        use crate::schema::world::dsl::*;
        let data = self
            .get_result(
                diesel::update(world)
                    .filter(id.eq(wid))
                    .set(randomnumber.eq(wid)),
            )
            .await?;
        match data {
            None => throw!(StatusCode::NOT_FOUND),
            Some(item) => Ok(item),
        }
    }
}

#[inline]
async fn gate(ctx: &mut Context<State>, next: Next<'_>) -> Result {
    // avoid to re-allocate a header map
    ctx.resp.headers = std::mem::take(&mut ctx.req.headers);
    ctx.resp.headers.clear();
    ctx.resp.headers.insert(SERVER, SERVER_HEADER.clone());
    next.await
}

#[inline]
async fn db(ctx: &mut Context<State>) -> Result {
    let id = ctx.random_id();
    let data = ctx.query_world(id).await?;
    ctx.write_json(&data)?;
    Ok(())
}

#[inline]
async fn queries(ctx: &mut Context<State>) -> Result {
    let random_ids: Vec<_> = (0..ctx.get_queries()).map(|_| ctx.random_id()).collect();
    let data: Vec<_> = {
        let ctx = &*ctx;
        join_all(random_ids.into_iter().map(move |id| ctx.query_world(id)))
            .await
            .into_iter()
            .filter_map(|item| item.ok())
            .collect()
    };
    ctx.write_json(&data)?;
    Ok(())
}

#[inline]
async fn fortune(ctx: &mut Context<State>) -> Result {
    use crate::schema::fortune::dsl::*;
    let mut fortunes = ctx.load_data(fortune).await?;
    fortunes.push(Fortune {
        id: 0,
        message: "Additional fortune added at request time.".to_owned(),
    });
    fortunes.sort_by(|it, next| it.message.cmp(&next.message));
    ctx.render(&Fortunes { items: &fortunes })
}

#[inline]
async fn updates(ctx: &mut Context<State>) -> Result {
    let random_ids: Vec<_> = (0..ctx.get_queries()).map(|_| ctx.random_id()).collect();
    let data: Vec<_> = {
        let ctx = &*ctx;
        join_all(random_ids.into_iter().map(move |id| ctx.update_world(id)))
            .await
            .into_iter()
            .filter_map(|item| item.ok())
            .collect()
    };
    ctx.write_json(&data)?;
    Ok(())
}

#[async_std::main]
async fn main() -> StdResult<(), Box<dyn StdError>> {
    let pool = Pool::builder()
        .max_size(500)
        .build(ConnectionManager::<PgConnection>::new(POSTGRES_URI))?;
    let router = Router::new()
        .gate(query_parser)
        .gate(gate)
        .on("/db", get(db))
        .on("/queries", get(queries))
        .on("/fortune", get(fortune))
        .on("/updates", get(updates));
    let app = App::new(State::new(pool)).end(router.routes("/")?);
    app.listen("0.0.0.0:8080", |addr| {
        println!("Server listen on {}...", addr);
    })?
    .await?;
    Ok(())
}
