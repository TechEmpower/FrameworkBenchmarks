#![allow(clippy::unused_async)]

use serde::Serialize;
use viz::{
    header::{HeaderValue, SERVER},
    Error, Request, Response, ResponseExt, Result, Router, Tree,
};

mod server;
mod utils;

#[derive(Serialize)]
struct Message {
    message: &'static str,
}

async fn plaintext(_: Request) -> Result<Response> {
    let mut res = Response::text("Hello, World!");
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Viz"));
    Ok(res)
}

async fn json(_: Request) -> Result<Response> {
    let mut res = Response::json(Message {
        message: "Hello, World!",
    })?;
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Viz"));
    Ok(res)
}

#[tokio::main]
async fn main() -> Result<()> {
    let app = Router::new()
        .get("/plaintext", plaintext)
        .get("/json", json);

    let tree = Tree::from(app);

    server::serve(tree).await.map_err(Error::Normal)
}
