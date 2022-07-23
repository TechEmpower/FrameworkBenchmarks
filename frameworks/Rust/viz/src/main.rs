use serde::Serialize;
use viz::{
    header::SERVER, Error, Request, Response, ResponseExt, Result, Router, ServiceMaker,
};

mod server;
mod utils;

#[derive(Serialize)]
struct Message {
    message: &'static str,
}

async fn plaintext(_: Request) -> Result<Response> {
    let mut res = Response::text("Hello, World!");
    res.headers_mut().insert(SERVER, utils::HDR_SERVER);
    Ok(res)
}

async fn json(_: Request) -> Result<Response> {
    let mut res = Response::json(Message {
        message: "Hello, World!",
    })?;
    res.headers_mut().insert(SERVER, utils::HDR_SERVER);
    Ok(res)
}

#[tokio::main]
async fn main() -> Result<()> {
    let app = Router::new()
        .get("/plaintext", plaintext)
        .get("/json", json);

    server::builder()
        .serve(ServiceMaker::from(app))
        .await
        .map_err(Error::normal)
}
