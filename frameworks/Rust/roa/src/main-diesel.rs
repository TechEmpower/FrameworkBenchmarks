//use diesel::prelude::*;
use roa::preload::*;
use roa::App;
use std::error::Error as StdError;
use std::result::Result as StdResult;

#[async_std::main]
async fn main() -> StdResult<(), Box<dyn StdError>> {
    let app = App::new(());
    app.listen("0.0.0.0:8080", |addr| {
        println!("Server listen on {}...", addr);
    })?
    .await?;
    Ok(())
}
