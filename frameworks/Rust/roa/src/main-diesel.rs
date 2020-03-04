use diesel::prelude::*;
use roa::{App, Result, Context};
use roa::preload::*;
use roa::router::{Router};
use roa::http::header::{SERVER, HeaderValue};
use std::error::Error as StdError;
use std::result::Result as StdResult;

#[async_std::main]
async fn main() -> StdResult<(), Box<dyn StdError>> {
    let mut app = App::new(());
    app.listen("127.0.0.1:8080", |addr| {
            println!("Server listen on {}...", addr);
        })?
        .await?;
    Ok(())
}
