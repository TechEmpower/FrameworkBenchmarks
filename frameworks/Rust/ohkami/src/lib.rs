mod fangs;
mod models;
#[cfg(feature = "db")] mod postgres;
#[cfg(feature = "db")] mod templates;

use {
    fangs::SetServer,
    models::Message,
    ohkami::prelude::*,
    ohkami::format::JSON,
};
#[cfg(feature = "db")] use {
    models::{Fortune, World, WorldsMeta},
    postgres::Postgres,
    templates::FortunesTemplate,
    ohkami::format::Query,
};

pub async fn ohkami() -> Ohkami {
    #[cfg(feature = "db")] {
        Ohkami::new((
            SetServer,
            Postgres::init().await,
            "/json"     .GET(json_serialization),
            "/db"       .GET(single_database_query),
            "/queries"  .GET(multiple_database_query),
            "/fortunes" .GET(fortunes),
            "/updates"  .GET(database_updates),
            "/plaintext".GET(plaintext),
        ))
    }
    #[cfg(not(feature = "db"))] {
        Ohkami::new((
            SetServer,
            "/json"     .GET(json_serialization),
            "/plaintext".GET(plaintext),
        ))
    }
}

async fn json_serialization() -> JSON<Message> {
    JSON(Message {
        message: "Hello, World!"
    })
}

#[cfg(feature = "db")]
async fn single_database_query(
    Context(db): Context<'_, Postgres>,
) -> JSON<World> {
    let world = db.select_random_world().await;
    JSON(world)
}

#[cfg(feature = "db")]
async fn multiple_database_query(
    Query(q): Query<WorldsMeta<'_>>,
    Context(db): Context<'_, Postgres>,
) -> JSON<Vec<World>> {
    let n = q.parse();
    let worlds = db.select_n_random_worlds(n).await;
    JSON(worlds)
}

#[cfg(feature = "db")]
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

#[cfg(feature = "db")]
async fn database_updates(
    Query(q): Query<WorldsMeta<'_>>,
    Context(db): Context<'_, Postgres>,
) -> JSON<Vec<World>> {
    let n = q.parse();
    let mut worlds = db.select_n_random_worlds(n).await;
    db.update_random_ids_of_worlds(&mut worlds).await;
    JSON(worlds)
}

async fn plaintext() -> &'static str {
    "Hello, World!"
}
