use crate::db::{
    fortune::{Entity as Fortunes, Model as Fortune},
    DbConnExt,
};
use sea_orm::entity::prelude::*;
use trillium::{conn_try, Conn, KnownHeaderName::ContentType};
use trillium_askama::{AskamaConnExt, Template};

#[derive(Template)]
#[template(path = "fortunes.html")]
struct FortuneTemplate<'a> {
    fortunes: &'a [Fortune],
}

pub async fn handler(conn: Conn) -> Conn {
    let db = conn.db();

    let mut fortunes = conn_try!(Fortunes::find().all(db).await, conn);
    fortunes.push(Fortune {
        id: 0,
        message: String::from("Additional fortune added at request time."),
    });

    fortunes.sort_by(|a, b| a.message.cmp(&b.message));

    conn.render(FortuneTemplate {
        fortunes: &fortunes,
    })
    .with_header(ContentType, "text/html; charset=utf-8")
}
