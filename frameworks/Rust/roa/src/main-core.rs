use roa::http::header::SERVER;
use roa::{App, Context, Result};
use std::error::Error as StdError;
use std::result::Result as StdResult;

pub mod endpoints;
pub mod utils;
use endpoints::{json, plaintext};
use utils::SERVER_HEADER;

#[inline]
async fn endpoint(ctx: &mut Context<()>) -> Result {
    // avoid to re-allocate a header map
    ctx.resp.headers = std::mem::take(&mut ctx.req.headers);
    ctx.resp.headers.clear();
    ctx.resp.headers.insert(SERVER, SERVER_HEADER.clone());
    match ctx.uri().path() {
        "/plaintext" => plaintext(ctx).await,
        _ => json(ctx).await,
    }
}

#[cfg(not(feature = "tokio_rt"))]
#[async_std::main]
async fn main() -> StdResult<(), Box<dyn StdError>> {
    use roa::preload::*;
    let app = App::new().end(endpoint);
    app.listen("0.0.0.0:8080", |addr| {
        println!("Server listen on {}...", addr);
    })?
    .await?;
    Ok(())
}

#[cfg(feature = "tokio_rt")]
#[tokio::main]
async fn main() -> StdResult<(), Box<dyn StdError>> {
    use roa_tokio::{Exec, TcpIncoming};
    let app = App::with_exec((), Exec).end(endpoint);
    let incoming = TcpIncoming::bind("0.0.0.0:8080")?;
    println!("Server listen on {}...", incoming.local_addr());
    app.accept(incoming).await?;
    Ok(())
}
