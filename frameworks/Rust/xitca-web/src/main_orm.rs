mod db_diesel_async;
mod schema;
mod ser;
mod util;

use xitca_web::{
    App,
    codegen::route,
    handler::{html::Html, json::Json, query::Query, state::StateRef, text::Text},
    http::{WebResponse, header::SERVER},
    route::get,
};

use db_diesel_async::Pool;
use ser::{Num, World};
use util::{HandleResult, SERVER_HEADER_VALUE};

fn main() -> std::io::Result<()> {
    App::new()
        .with_async_state(db_diesel_async::create)
        .at("/plaintext", get(Text("Hello, World!")))
        .at("/json", get(Json(ser::Message::new())))
        .at_typed(db)
        .at_typed(fortunes)
        .at_typed(queries)
        .at_typed(updates)
        .map(header)
        .serve()
        .disable_vectored_write()
        .bind("0.0.0.0:8080")?
        .run()
        .wait()
}

fn header(mut res: WebResponse) -> WebResponse {
    res.headers_mut().insert(SERVER, SERVER_HEADER_VALUE);
    res
}

#[route("/db", method = get)]
async fn db(StateRef(pool): StateRef<'_, Pool>) -> HandleResult<Json<World>> {
    pool.get_world().await.map(Json)
}

#[route("/fortunes", method = get)]
async fn fortunes(StateRef(pool): StateRef<'_, Pool>) -> HandleResult<Html<String>> {
    use sailfish::TemplateOnce;
    let html = pool.tell_fortune().await?.render_once()?;
    Ok(Html(html))
}

#[route("/queries", method = get)]
async fn queries(Query(Num(num)): Query<Num>, StateRef(pool): StateRef<'_, Pool>) -> HandleResult<Json<Vec<World>>> {
    pool.get_worlds(num).await.map(Json)
}

#[route("/updates", method = get)]
async fn updates(Query(Num(num)): Query<Num>, StateRef(pool): StateRef<'_, Pool>) -> HandleResult<Json<Vec<World>>> {
    pool.update(num).await.map(Json)
}
