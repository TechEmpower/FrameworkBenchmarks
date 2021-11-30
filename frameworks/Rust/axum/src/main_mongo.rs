mod models_common;
mod models_mongo;
mod database_mongo;
mod utils;
mod server;
mod common;

use dotenv::dotenv;
use std::env;
use std::time::Duration;
use axum::{
    extract::{Query},
    http::StatusCode,
    response::IntoResponse,
    routing::get,
    AddExtensionLayer, Json, Router,
};
use axum::http::{header, HeaderValue};
use futures::stream::StreamExt;
use tower_http::set_header::SetResponseHeaderLayer;
use hyper::Body;
use rand::rngs::SmallRng;
use rand::{SeedableRng};
use yarte::Template;
use mongodb::{bson::doc, Client, Database};
use mongodb::options::ClientOptions;

use models_mongo::{World, Fortune};
use utils::{Params, parse_params, random_number, Utf8Html};
use database_mongo::DatabaseConnection;
use models_mongo::FortuneInfo;

async fn db(DatabaseConnection(mut db): DatabaseConnection) -> impl IntoResponse {
    let mut rng = SmallRng::from_entropy();
    let number = random_number(&mut rng);

    let world = find_world_by_id(&mut db, number).await;

    (StatusCode::OK, Json(world))
}

async fn find_world_by_id(db: &mut Database, number: i32) -> World {
    let world_collection = db.collection::<World>("world");

    let filter = doc! { "id": number as f32 };

    let world: World = world_collection.find_one(Some(filter), None).await.expect("world could not be found").unwrap();
    world
}

async fn queries(DatabaseConnection(mut db): DatabaseConnection, Query(params): Query<Params>) -> impl IntoResponse {
    let q = parse_params(params);

    let mut rng = SmallRng::from_entropy();

    let mut results = Vec::with_capacity(q as usize);

    for _ in 0..q {
        let query_id = random_number(&mut rng);

        let result :World =  find_world_by_id(&mut db, query_id).await;

        results.push(result);
    }

    (StatusCode::OK, Json(results))
}

async fn fortunes(DatabaseConnection(db): DatabaseConnection) -> impl IntoResponse {
    let fortune_collection = db.collection::<Fortune>("fortune");

    let mut fortune_cursor = fortune_collection.find(None, None).await.expect("fortunes could not be loaded");

    let mut fortunes: Vec<Fortune> = Vec::with_capacity(100 as usize);

    while let Some(doc) = fortune_cursor.next().await {
        fortunes.push(doc.expect("could not load fortune"));
    }

    fortunes.push(Fortune {
        id: 0.0,
        message: "Additional fortune added at request time.".to_string(),
    });

    fortunes.sort_by(|a, b| a.message.cmp(&b.message));

    let fortune_infos: Vec<FortuneInfo> = fortunes.iter().map(|f| FortuneInfo { id: f.id as i32, message: f.message.clone() }).collect();

    Utf8Html(
        FortunesTemplate {
            fortunes: &fortune_infos,
        }
        .call()
        .expect("error rendering template"),
    )
}

#[tokio::main]
async fn main() {
    dotenv().ok();

    let database_url = env::var("AXUM_TECHEMPOWER_MONGODB_URL").ok()
        .expect("AXUM_TECHEMPOWER_MONGODB_URL environment variable was not set");

    // setup connection pool
    let mut client_options = ClientOptions::parse(database_url).await.unwrap();
    client_options.max_pool_size = Some(common::POOL_SIZE);
    client_options.min_pool_size = Some(common::POOL_SIZE);
    client_options.connect_timeout = Some(Duration::from_millis(200));

    let client = Client::with_options(client_options).unwrap();

    let app = Router::new()
        .route("/fortunes", get(fortunes))
        .route("/db", get(db))
        .route("/queries", get(queries))
        .layer(AddExtensionLayer::new(client))
        .layer(SetResponseHeaderLayer::<_, Body>::if_not_present(header::SERVER, HeaderValue::from_static("Axum")));

    server::builder()
        .serve(app.into_make_service())
        .await
        .unwrap();
}

#[derive(Template)]
#[template(path = "fortunes.html.hbs")]
pub struct FortunesTemplate<'a> {
    pub fortunes: &'a Vec<FortuneInfo>,
}