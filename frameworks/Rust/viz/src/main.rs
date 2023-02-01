use serde::Serialize;
use viz::{
    header::{HeaderValue, SERVER},
    Error, Request, Response, ResponseExt, Result, Router, ServiceMaker,
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

    server::builder()
        .http1_pipeline_flush(true)
        .serve(ServiceMaker::from(app))
        .await
        .map_err(Error::normal)
}
