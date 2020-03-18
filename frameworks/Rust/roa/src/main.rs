use lazy_static::lazy_static;
use roa::http::header::{HeaderValue, CONTENT_LENGTH, SERVER};
use roa::preload::*;
use roa::{App, Context, Result};
use std::error::Error as StdError;
use std::result::Result as StdResult;

mod utils;
use utils::Message;

static HELLO_WORLD: &str = "Hello, World!";

lazy_static! {
    static ref SERVER_HEADER: HeaderValue = HeaderValue::from_static("roa");
    static ref JSON_LEN: HeaderValue = HeaderValue::from_static("27");
    static ref PLAINTEXT_LEN: HeaderValue = HeaderValue::from_static("13");
}

#[inline]
async fn json(ctx: &mut Context<()>) -> Result {
    ctx.resp.headers.insert(CONTENT_LENGTH, JSON_LEN.clone());
    ctx.write_json(&Message {
        message: HELLO_WORLD,
    })
}

#[inline]
async fn plaintext(ctx: &mut Context<()>) -> Result {
    ctx.resp
        .headers
        .insert(CONTENT_LENGTH, PLAINTEXT_LEN.clone());
    ctx.write_text(HELLO_WORLD);
    Ok(())
}

#[inline]
async fn endpoint(ctx: &mut Context<()>) -> Result {
    ctx.resp.headers.insert(SERVER, SERVER_HEADER.clone());
    match ctx.uri().path() {
        "/json" => json(ctx).await,
        _ => plaintext(ctx).await,
    }
}

#[async_std::main]
async fn main() -> StdResult<(), Box<dyn StdError>> {
    let app = App::new(()).end(endpoint);
    app.listen("0.0.0.0:8080", |addr| {
        println!("Server listen on {}...", addr);
    })?
    .await?;
    Ok(())
}
