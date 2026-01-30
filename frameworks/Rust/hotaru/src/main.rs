use serde::{Deserialize, Serialize};

mod models;
mod utils;
mod database;

#[derive(Serialize, Deserialize)]
struct Message {
    message: String,
}

// Hotaru implementation
mod hotaru_impl {
    use super::{database, models::Fortune, models::World, utils, Message};
    use akari::Value;
    use hotaru::http::*;
    use hotaru::hotaru_core::http::start_line::HttpStartLine;
    use hotaru::prelude::*;
    use serde::Serialize;
    use std::collections::HashMap;
    use std::sync::Arc;
    use std::time::SystemTime;
    use database::{DbPool, WorldCache};

    #[derive(Clone)]
    struct FortuneView {
        id: i32,
        message: String,
    }

    impl From<World> for Value {
        fn from(world: World) -> Self {
            object!({
                id: world.id,
                randomNumber: world.random_number,
            })
        }
    }

    impl From<FortuneView> for Value {
        fn from(fortune: FortuneView) -> Self {
            object!({
                id: fortune.id,
                message: fortune.message,
            })
        }
    }

    pub static APP: SApp = Lazy::new(|| {
        let pool = database::create_pool();
        let cache = database::create_cache();
        let runtime = tokio::runtime::Runtime::new().expect("Failed to create runtime");
        if let Err(err) = runtime.block_on(database::warm_cache(&pool, &cache)) {
            eprintln!("Failed to warm cache at startup: {err}");
        }

        App::new()
            .binding("0.0.0.0:8080")
            .set_statics("db_pool", pool)
            .set_statics("world_cache", cache)
            .build()
    });

    fn pool_from(req: &HttpContext) -> DbPool {
        req.app()
            .expect("Hotaru app not available")
            .statics()
            .get::<DbPool>("db_pool")
            .expect("Database pool missing")
            .clone()
    }

    fn cache_from(req: &HttpContext) -> WorldCache {
        req.app()
            .expect("Hotaru app not available")
            .statics()
            .get::<WorldCache>("world_cache")
            .expect("World cache missing")
            .clone()
    }

    fn append_fortune(mut fortunes: Vec<Fortune>) -> Vec<Fortune> {
        fortunes.push(Fortune {
            id: 0,
            message: "Additional fortune added at request time.".to_string(),
        });
        fortunes.sort_by(|a, b| a.message.cmp(&b.message));
        fortunes
    }

    fn with_standard_headers(response: HttpResponse) -> HttpResponse {
        let date = httpdate::fmt_http_date(SystemTime::now());
        response
            .add_header("Server", "hotaru")
            .add_header("Date", date)
    }

    fn json_response_direct<T: Serialize>(data: &T) -> HttpResponse {
        let json_bytes = serde_json::to_vec(data).expect("JSON serialization failed");
        let start_line = HttpStartLine::new_response(HttpVersion::Http11, StatusCode::OK);
        let mut meta = HttpMeta::new(start_line, HashMap::new());
        meta.set_content_type(HttpContentType::ApplicationJson());
        with_standard_headers(HttpResponse::new(meta, HttpBody::Binary(json_bytes)))
    }

    endpoint! {
        APP.url("/json"),
        pub json_endpoint<HTTP> {
            json_response_direct(&Message {
                message: "Hello, World!".to_string(),
            })
        }
    }

    endpoint! {
        APP.url("/plaintext"),
        pub plaintext_endpoint<HTTP> {
            with_standard_headers(text_response("Hello, World!"))
        }
    }

    endpoint! {
        APP.url("/db"),
        pub db_endpoint<HTTP> {
            let pool = pool_from(&req);
            let client = pool.get().await.expect("DB pool error");
            let id = utils::random_id();
            let world = database::fetch_world_by_id(&client, id)
                .await
                .expect("DB query failed");

            json_response_direct(&world)
        }
    }

    endpoint! {
        APP.url("/queries"),
        pub queries_endpoint<HTTP> {
            let count = utils::parse_query_count(req.query("queries").as_deref());
            let pool = pool_from(&req);
            let client = pool.get().await.expect("DB pool error");

            let mut worlds = Vec::with_capacity(count);
            for _ in 0..count {
                let id = utils::random_id();
                let world = database::fetch_world_by_id(&client, id)
                    .await
                    .expect("DB query failed");
                worlds.push(world);
            }

            json_response_direct(&worlds)
        }
    }

    endpoint! {
        APP.url("/updates"),
        pub updates_endpoint<HTTP> {
            let count = utils::parse_query_count(req.query("queries").as_deref());
            let pool = pool_from(&req);
            let client = pool.get().await.expect("DB pool error");

            let mut worlds = Vec::with_capacity(count);
            for _ in 0..count {
                let id = utils::random_id();
                let mut world = database::fetch_world_by_id(&client, id)
                    .await
                    .expect("DB query failed");
                world.random_number = utils::random_id();
                database::update_world(&client, &world)
                    .await
                    .expect("DB update failed");
                worlds.push(world);
            }

            json_response_direct(&worlds)
        }
    }

    endpoint! {
        APP.url("/cached-worlds"),
        pub cached_worlds_endpoint<HTTP> {
            let count = utils::parse_query_count(req.query("count").as_deref());
            let pool = pool_from(&req);
            let cache = cache_from(&req);
            let client = pool.get().await.expect("DB pool error");

            let mut worlds = Vec::with_capacity(count);
            for _ in 0..count {
                let id = utils::random_id();
                if let Some(world) = cache.get(&id) {
                    worlds.push(world.as_ref().clone());
                } else {
                    let world = database::fetch_cached_world_by_id(&client, id)
                        .await
                        .expect("DB query failed");
                    cache.insert(id, Arc::new(world.clone()));
                    worlds.push(world);
                }
            }

            json_response_direct(&worlds)
        }
    }

    endpoint! {
        APP.url("/fortunes"),
        pub fortunes_endpoint<HTTP> {
            let pool = pool_from(&req);
            let client = pool.get().await.expect("DB pool error");
            let fortunes = database::fetch_all_fortunes(&client)
                .await
                .expect("DB query failed");
            let fortunes = append_fortune(fortunes)
                .into_iter()
                .map(|fortune| FortuneView {
                    id: fortune.id,
                    message: utils::escape_html(&fortune.message),
                })
                .collect::<Vec<_>>();

            with_standard_headers(akari_render!("fortunes_hotaru.html", fortunes = fortunes))
        }
    }

    pub async fn run() {
        println!("🔥 Hotaru server running on http://0.0.0.0:8080");
        let _ = APP.clone().run().await;
    }
}

#[tokio::main]
async fn main() {
    hotaru_impl::run().await;
}
