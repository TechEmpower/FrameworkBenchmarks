mod db_diesel;
mod schema;
mod ser;
mod util;

use serde::Serialize;
use xitca_web::{
    codegen::route,
    handler::{html::Html, json::Json, query::Query, state::StateOwn, text::Text},
    http::{header::SERVER, WebResponse},
    route::get,
    App,
};

use db_diesel::Pool;
use ser::Num;
use util::{HandleResult, SERVER_HEADER_VALUE};

fn main() -> std::io::Result<()> {
    App::new()
        .with_state(db_diesel::create()?)
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
fn db(StateOwn(pool): StateOwn<Pool>) -> HandleResult<Json<impl Serialize>> {
    pool.get_world().map(Json)
}

#[route("/fortunes", method = get)]
fn fortunes(StateOwn(pool): StateOwn<Pool>) -> HandleResult<Html<String>> {
    use sailfish::TemplateOnce;
    let html = pool.tell_fortune()?.render_once()?;
    Ok(Html(html))
}

#[route("/queries", method = get)]
fn queries(Query(Num(num)): Query<Num>, StateOwn(pool): StateOwn<Pool>) -> HandleResult<Json<impl Serialize>> {
    pool.get_worlds(num).map(Json)
}

#[route("/updates", method = get)]
fn updates(Query(Num(num)): Query<Num>, StateOwn(pool): StateOwn<Pool>) -> HandleResult<Json<impl Serialize>> {
    pool.update(num).map(Json)
}
