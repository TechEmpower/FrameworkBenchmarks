mod fangs;
use fangs::SetServer;

mod models;
use models::{Fortune, Message, World, WorldsQuery};

mod postgres;
use postgres::Postgres;

mod templates;
use templates::FortunesTemplate;

use ohkami::prelude::*;
use ohkami::format::{JSON, Query};
use ohkami::Memory;


#[tokio::main]
async fn main() {
    Ohkami::with((
        SetServer,
        Memory::new(Postgres::new().await),
    ), (
        "/json"     .GET(json_serialization),
        "/db"       .GET(single_database_query),
        "/queries"  .GET(multiple_database_query),
        "/fortunes" .GET(fortunes),
        "/updates"  .GET(database_updates),
        "/plaintext".GET(plaintext),
    )).howl("0.0.0.0:8000").await
}

async fn json_serialization() -> JSON<Message> {
    JSON(Message {
        message: "Hello, World!"
    })
}

async fn single_database_query(p: Memory<'_, Postgres>) -> JSON<World> {
    let world = p.select_random_world().await;
    JSON(world)
}

async fn multiple_database_query(
    Query(q): Query<WorldsQuery<'_>>,
    p: Memory<'_, Postgres>
) -> JSON<Vec<World>> {
    let n = q.parse();
    let worlds = p.select_n_random_worlds(n).await;
    JSON(worlds)
}

async fn fortunes(p: Memory<'_, Postgres>) -> FortunesTemplate {
    let mut fortunes = p.select_all_fortunes().await;
    fortunes.push(Fortune {
        id:      0,
        message: String::from("Additional fortune added at request time."),
    });
    fortunes.sort_unstable_by(|a, b| str::cmp(&a.message, &b.message));
    FortunesTemplate { fortunes }
}

async fn database_updates(
    Query(q): Query<WorldsQuery<'_>>,
    p: Memory<'_, Postgres>
) -> JSON<Vec<World>> {
    let n = q.parse();
    let mut worlds = p.select_n_random_worlds(n).await;
    p.update_random_ids_of_worlds(&mut worlds).await;
    JSON(worlds)
}

async fn plaintext() -> &'static str {
    "Hello, World!"
}
