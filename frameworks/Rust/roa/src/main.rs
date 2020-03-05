use roa::{App, Result, Context, throw};
use roa::preload::*;
use roa::http::header::{SERVER, HeaderValue, CONTENT_LENGTH};
use roa::http::StatusCode;
use std::error::Error as StdError;
use std::result::Result as StdResult;
use lazy_static::lazy_static;

mod utils;
use utils::Message;

static HELLO_WORLD: &str = "Hello, World!";

lazy_static! {
    static ref SERVER_HEADER: HeaderValue = HeaderValue::from_static("roa");
    static ref JSON_LEN: HeaderValue = HeaderValue::from_static("27");
    static ref PLAINTEXT_LEN: HeaderValue = HeaderValue::from_static("13");
}

#[inline]
async fn json(mut ctx: Context<()>) -> Result {
    ctx.resp_mut().headers.insert(CONTENT_LENGTH, JSON_LEN.clone());
    ctx.write_json(&Message { message: HELLO_WORLD })
}

#[inline]
async fn plaintext(mut ctx: Context<()>) -> Result {
    ctx.resp_mut().headers.insert(CONTENT_LENGTH, PLAINTEXT_LEN.clone());
    ctx.write_text(HELLO_WORLD)
}

#[async_std::main]
async fn main() -> StdResult<(), Box<dyn StdError>> {
    let mut app = App::new(());
    app.end(|mut ctx| async {
        ctx.resp_mut().headers.insert(SERVER, SERVER_HEADER.clone());
        match ctx.uri().path() {
            "/plaintext" => plaintext(ctx).await,
            "/json" => json(ctx).await,
            _ => throw!(StatusCode::NOT_FOUND),
        }
    });
    app.listen("0.0.0.0:8080", |addr| {
        println!("Server listen on {}...", addr);
    })?
    .await?;
    Ok(())
}
