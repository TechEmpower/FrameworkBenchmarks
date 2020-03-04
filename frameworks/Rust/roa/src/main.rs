use roa::{App, Result, Context};
use roa::preload::*;
use roa::router::{Router};
use roa::http::header::{SERVER, HeaderValue};
use std::error::Error as StdError;
use std::result::Result as StdResult;

mod utils;
use utils::Message;

async fn json(mut ctx: Context<()>) -> Result {
    ctx.resp_mut().insert(SERVER, HeaderValue::from_static("Roa"))?;
    ctx.write_json(&Message { message: "Hello, World!" })
}

async fn plaintext(mut ctx: Context<()>) -> Result {
    ctx.resp_mut().insert(SERVER, HeaderValue::from_static("Roa"))?;
    ctx.write_text("Hello, World!")
}

#[async_std::main]
async fn main() -> StdResult<(), Box<dyn StdError>> {
    let mut app = App::new(());
    let mut router = Router::new();
    router
        .get("/json", json)
        .get("/plaintext", plaintext);
    app.gate(router.routes("/")?)
        .listen("0.0.0.0:8080", |addr| {
            println!("Server listen on {}...", addr);
        })?
        .await?;
    Ok(())
}
