#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[macro_use]
extern crate diesel;

mod db_diesel;
mod schema;
mod ser;
mod util;

use serde::Serialize;
use xitca_web::{
    dev::service::Service,
    handler::{handler_service, html::Html, json::Json, state::StateRef, uri::UriRef, Responder},
    http::header::SERVER,
    request::WebRequest,
    response::WebResponse,
    route::get,
    App, HttpServer,
};

use self::{
    db_diesel::{create, DieselPool},
    util::{Error, HandleResult, QueryParse, DB_URL, SERVER_HEADER_VALUE},
};

type Request<'a> = WebRequest<'a, DieselPool>;

fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::with_async_state(|| create(DB_URL))
            .at("/plaintext", get(handler_service(plain_text)))
            .at("/json", get(handler_service(json)))
            .at("/db", get(handler_service(db)))
            .at("/fortunes", get(handler_service(fortunes)))
            .at("/queries", get(handler_service(queries)))
            .at("/updates", get(handler_service(updates)))
            .enclosed_fn(middleware_fn)
            .finish()
    })
    .bind("0.0.0.0:8080")?
    .run()
    .wait()
}

async fn middleware_fn<S, E>(service: &S, mut ctx: Request<'_>) -> Result<WebResponse, E>
where
    S: for<'r> Service<Request<'r>, Response = HandleResult<WebResponse>, Error = E>,
{
    let mut res = match service.call(ctx.reborrow()).await? {
        Ok(res) => res,
        Err(err) => err.respond_to(ctx).await,
    };
    res.headers_mut().append(SERVER, SERVER_HEADER_VALUE);
    Ok(res)
}

async fn plain_text() -> Result<&'static str, Error> {
    Ok("Hello, World!")
}

async fn json() -> Result<Json<impl Serialize>, Error> {
    Ok(Json(ser::Message::new()))
}

async fn db(StateRef(pool): StateRef<'_, DieselPool>) -> HandleResult<Json<impl Serialize>> {
    pool.get_world().await.map(Json)
}

async fn fortunes(StateRef(pool): StateRef<'_, DieselPool>) -> HandleResult<Html<String>> {
    use sailfish::TemplateOnce;
    let fortunes = pool.tell_fortune().await?.render_once()?;
    Ok(Html(fortunes))
}

async fn queries(
    StateRef(pool): StateRef<'_, DieselPool>,
    UriRef(uri): UriRef<'_>,
) -> HandleResult<Json<impl Serialize>> {
    let num = uri.query().parse_query();
    pool.get_worlds(num).await.map(Json)
}

async fn updates(
    StateRef(pool): StateRef<'_, DieselPool>,
    UriRef(uri): UriRef<'_>,
) -> HandleResult<Json<impl Serialize>> {
    let num = uri.query().parse_query();
    pool.update(num).await.map(Json)
}
