#[cfg(feature = "orm")]
#[macro_use]
extern crate diesel;

#[cfg(feature = "orm")]
mod db_diesel;

#[cfg(feature = "orm")]
mod schema;

#[cfg(feature = "orm")]
use db_diesel::State;

#[cfg(feature = "pg")]
mod db_pg;

#[cfg(feature = "pg")]
use db_pg::State;

#[cfg(feature = "sqlx-pg")]
mod db_sqlx;

#[cfg(feature = "sqlx-pg")]
use db_sqlx::State;

use futures::stream::{FuturesUnordered, TryStreamExt};
use roa::http::header::SERVER;
use roa::preload::*;
use roa::router::{get, RouteTable, Router};
use roa::{async_trait, throw, App, Context, Next, Result};
mod models;
pub mod utils;
use dotenv_codegen::dotenv;
use models::*;
use utils::SERVER_HEADER;

type StdResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[async_trait(?Send)]
trait Service {
    fn random_id(&mut self) -> i32;
    fn get_queries(&self) -> usize;
    async fn query_world(&self, wid: i32) -> Result<World>;
    async fn fortunes(&self) -> Result<Vec<Fortune>>;
    async fn update_worlds(&mut self) -> Result<Vec<World>>;
    async fn query_worlds(&mut self) -> Result<Vec<World>> {
        let worlds = FuturesUnordered::new();
        let random_ids: Vec<_> =
            (0..self.get_queries()).map(|_| self.random_id()).collect();
        for id in random_ids {
            worlds.push(self.query_world(id));
        }
        worlds.try_collect().await
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
    let data = ctx.query_worlds().await?;
    ctx.write_json(&data)?;
    Ok(())
}

#[inline]
async fn fortune(ctx: &mut Context<State>) -> Result {
    let mut fortunes = ctx.fortunes().await?;
    fortunes.push(Fortune {
        id: 0,
        message: "Additional fortune added at request time.".to_owned(),
    });
    fortunes.sort_by(|it, next| it.message.cmp(&next.message));
    ctx.render(&Fortunes { items: &fortunes })
}

#[inline]
async fn updates(ctx: &mut Context<State>) -> Result {
    let data = ctx.update_worlds().await?;
    ctx.write_json(&data)?;
    Ok(())
}

fn routes(prefix: &'static str) -> StdResult<RouteTable<State>> {
    Router::new()
        .gate(gate)
        .on("/db", get(db))
        .on("/queries", get(queries))
        .on("/fortunes", get(fortune))
        .on("/updates", get(updates))
        .routes(prefix)
        .map_err(Into::into)
}

#[async_std::main]
async fn main() -> StdResult<()> {
    let app = App::state(State::bind(dotenv!("DATABASE_URL")).await?).end(routes("/")?);
    app.listen("0.0.0.0:8080", |addr| {
        println!("Server listen on {}...", addr);
    })?
    .await?;
    Ok(())
}
