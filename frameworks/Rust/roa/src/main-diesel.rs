#[macro_use]
extern crate diesel;

use askama::Template;
use diesel::pg::PgConnection;
use diesel::prelude::*;
use futures::future::join_all;
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use roa::http::header::SERVER;
use roa::http::StatusCode;
use roa::preload::*;
use roa::query::query_parser;
use roa::router::{get, Router};
use roa::{throw, App, Context, Next, Result};
use roa::diesel::{Pool, WrapError};
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

#[inline]
async fn gate(ctx: &mut Context<State>, next: Next<'_>) -> Result {
    // avoid to re-allocate a header map
    ctx.resp.headers = std::mem::take(&mut ctx.req.headers);
    ctx.resp.headers.clear();
    ctx.resp.headers.insert(SERVER, SERVER_HEADER.clone());
    next.await
}

#[inline]
async fn query_world(ctx: &Context<State>, random: i32) -> Result<World> {
    use crate::schema::world::dsl::*;
    let data = ctx.first(world.filter(id.eq(random))).await?;
    match data {
        None => throw!(StatusCode::NOT_FOUND),
        Some(item) => Ok(item),
    }
}

#[inline]
fn random_id(ctx: &mut Context<State>) -> i32 {
    ctx.rng.gen_range(0, 10_001)
}

#[inline]
async fn get_queries(ctx: &mut Context<State>) -> Vec<World> {
    use std::cmp::{max, min};
    let nums = ctx
        .query("q")
        .and_then(|var| var.parse().ok())
        .unwrap_or(1);
    let queries = min(500, max(1, nums));
    let random_ids: Vec<_> = (0..queries).map(|_| random_id(ctx)).collect();
    let ctx = &*ctx;
    join_all(random_ids.iter().map(move |id| query_world(ctx, *id)))
        .await
        .into_iter()
        .filter_map(|item| item.ok())
        .collect()
}

#[inline]
async fn db(ctx: &mut Context<State>) -> Result {
    let id = random_id(ctx);
    let data = query_world(ctx, id).await?;
    ctx.write_json(&data)?;
    Ok(())
}

#[inline]
async fn queries(ctx: &mut Context<State>) -> Result {
    let data = get_queries(ctx).await;
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
    let mut worlds = get_queries(ctx).await;
    for world in worlds.iter_mut() {
        world.randomnumber = random_id(ctx)
    }
    ctx.write_json(&worlds)?;
    let conn = ctx.get_conn().await?;
    ctx.exec.spawn_blocking(move || {
        conn.transaction::<_, WrapError, _>(|| {
            use crate::schema::world::dsl::*;
            for data in worlds {
                diesel::update(world)
                    .filter(id.eq(data.id))
                    .set(randomnumber.eq(data.randomnumber))
                    .execute(&conn)?;
            }
            Ok(())
        })
    }).await?;
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
