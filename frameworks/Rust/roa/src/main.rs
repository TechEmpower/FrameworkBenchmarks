use roa::http::header::SERVER;
use roa::preload::*;
use roa::router::{get, Router};
use roa::{App, Context, Next, Result};
use std::error::Error;
use std::result::Result as StdResult;

pub mod endpoints;
pub mod utils;
use endpoints::{json, plaintext};
use utils::SERVER_HEADER;

#[inline]
async fn gate(ctx: &mut Context<()>, next: Next<'_>) -> Result {
    // avoid to re-allocate a header map
    ctx.resp.headers = std::mem::take(&mut ctx.req.headers);
    ctx.resp.headers.clear();
    ctx.resp.headers.insert(SERVER, SERVER_HEADER.clone());
    next.await
}

#[async_std::main]
async fn main() -> StdResult<(), Box<dyn Error>> {
    let router = Router::new()
        .gate(gate)
        .on("/json", get(json))
        .on("/plaintext", get(plaintext));
    let app = App::new().end(router.routes("/")?);
    app.listen("0.0.0.0:8080", |addr| {
        println!("Server listen on {}...", addr);
    })?
    .await?;
    Ok(())
}
