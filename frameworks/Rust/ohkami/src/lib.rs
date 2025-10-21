mod fangs;
mod models;
#[cfg(feature = "rt_tokio")] mod postgres;
#[cfg(feature = "rt_tokio")] mod templates;

use {
    fangs::SetServer,
    models::Message,
    ohkami::prelude::*,
};
#[cfg(feature = "rt_tokio")] use {
    models::{Fortune, World, WorldsMeta},
    postgres::Postgres,
    templates::FortunesTemplate,
};

pub async fn ohkami() -> Ohkami {
    Ohkami::new((
        SetServer,
        #[cfg(feature = "rt_tokio")]
        Context::new(Postgres::new().await),
        
        "/plaintext".GET(plaintext),
        "/json".GET(json_serialization),
        #[cfg(feature = "rt_tokio")]
        "/db".GET(single_database_query),
        #[cfg(feature = "rt_tokio")]
        "/queries".GET(multiple_database_query),
        #[cfg(feature = "rt_tokio")]
        "/fortunes".GET(fortunes),
        #[cfg(feature = "rt_tokio")]
        "/updates".GET(database_updates),
    ))
}

async fn plaintext() -> &'static str {
    "Hello, World!"
}

async fn json_serialization() -> Json<Message> {
    Json(Message {
        message: "Hello, World!"
    })
}

#[cfg(feature = "rt_tokio")]
async fn single_database_query(
    Context(db): Context<'_, Postgres>,
) -> Json<World> {
    let world = db.select_random_world().await;
    Json(world)
}

#[cfg(feature = "rt_tokio")]
async fn multiple_database_query(
    Query(q): Query<WorldsMeta<'_>>,
    Context(db): Context<'_, Postgres>,
) -> Json<Vec<World>> {
    let n = q.parse();
    let worlds = db.select_n_random_worlds(n).await;
    Json(worlds)
}

#[cfg(feature = "rt_tokio")]
async fn fortunes(
    Context(db): Context<'_, Postgres>,
) -> FortunesTemplate {
    let mut fortunes = db.select_all_fortunes().await;
    fortunes.push(Fortune {
        id:      0,
        message: String::from("Additional fortune added at request time."),
    });
    fortunes.sort_unstable_by(|a, b| str::cmp(&a.message, &b.message));
    FortunesTemplate { fortunes }
}

#[cfg(feature = "rt_tokio")]
async fn database_updates(
    Query(q): Query<WorldsMeta<'_>>,
    Context(db): Context<'_, Postgres>,
) -> Json<Vec<World>> {
    let n = q.parse();
    let worlds = db.update_randomnumbers_of_n_worlds(n).await;
    Json(worlds)
}
