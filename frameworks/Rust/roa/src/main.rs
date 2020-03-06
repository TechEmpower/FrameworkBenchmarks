use lazy_static::lazy_static;
use pprof::ProfilerGuard;
use roa::http::header::{HeaderValue, CONTENT_LENGTH, SERVER};
use roa::http::StatusCode;
use roa::preload::*;
use roa::{throw, App, Context, Result};
use roa::body::DispositionType::*;
use std::error::Error as StdError;
use std::io;
use std::result::Result as StdResult;

mod utils;
use utils::Message;

static HELLO_WORLD: &str = "Hello, World!";

lazy_static! {
    static ref SERVER_HEADER: HeaderValue = HeaderValue::from_static("roa");
    static ref JSON_LEN: HeaderValue = HeaderValue::from_static("27");
    static ref PLAINTEXT_LEN: HeaderValue = HeaderValue::from_static("13");
    static ref GUARD: ProfilerGuard<'static> = ProfilerGuard::new(100).unwrap();
}

#[inline]
async fn json(mut ctx: Context<()>) -> Result {
    ctx.resp_mut()
        .headers
        .insert(CONTENT_LENGTH, JSON_LEN.clone());
    ctx.write_json(&Message {
        message: HELLO_WORLD,
    })
}

#[inline]
async fn plaintext(mut ctx: Context<()>) -> Result {
    ctx.resp_mut()
        .headers
        .insert(CONTENT_LENGTH, PLAINTEXT_LEN.clone());
    ctx.write_text(HELLO_WORLD)
}

#[async_std::main]
async fn main() -> StdResult<(), Box<dyn StdError>> {
    GUARD.report().build().unwrap();
    let mut app = App::new(());
    app.end(|mut ctx| async {
        ctx.resp_mut().headers.insert(SERVER, SERVER_HEADER.clone());
        match ctx.uri().path() {
            "/plaintext" => plaintext(ctx).await,
            "/json" => json(ctx).await,
            "/flamegraph" => {
                if let Ok(report) = GUARD.report().build() {
                    ctx.exec
                        .spawn_blocking(move || -> io::Result<()> {
                            let file = std::fs::File::create("flamegraph.svg")?;
                            report.flamegraph(file).unwrap();
                            Ok(())
                        })
                        .await?;
                    ctx.write_file("flamegraph.svg", Inline).await?;
                }
                Ok(())
            }
            _ => throw!(StatusCode::NOT_FOUND),
        }
    });
    app.listen("0.0.0.0:8080", |addr| {
        println!("Server listen on {}...", addr);
    })?
    .await?;
    Ok(())
}
