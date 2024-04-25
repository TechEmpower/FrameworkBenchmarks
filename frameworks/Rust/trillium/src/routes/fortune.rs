use crate::db::{
    fortune::{Entity as Fortunes, Model as Fortune},
    DbConnExt,
};
use futures_lite::StreamExt;
use sea_orm::EntityTrait;
use std::collections::BTreeSet;
use trillium::{Conn, KnownHeaderName::ContentType, Status};
use trillium_askama::{AskamaConnExt, Template};

#[derive(Template)]
#[template(path = "fortunes.html")]
struct FortuneTemplate<'a> {
    fortunes: &'a BTreeSet<Fortune>,
}

pub async fn handler(conn: Conn) -> Conn {
    let db = conn.db().clone();

    let mut fortunes = BTreeSet::new();
    fortunes.insert(Fortune {
        id: 0,
        message: String::from("Additional fortune added at request time."),
    });

    let Ok(mut stream) = Fortunes::find().stream(&db).await else {
        return conn.with_status(Status::InternalServerError);
    };

    while let Some(Ok(fortune)) = stream.next().await {
        fortunes.insert(fortune);
    }

    conn.render(FortuneTemplate {
        fortunes: &fortunes,
    })
    .with_response_header(ContentType, "text/html; charset=utf-8")
}
