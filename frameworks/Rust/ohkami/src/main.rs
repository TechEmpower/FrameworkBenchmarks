use ohkami::{prelude::*, json};
use sqlx::postgres::PgPoolOptions;
mod utils; use utils::{
    consts::{DB_URL, MAX_CONNECTIONS},
    models::{World, Fortune},
    functions::{random_i32, random_i32s, html_from},
};

fn main() -> Result<()> {
    let config = Config {
        db_profile: DBprofile {
            pool_options: PgPoolOptions::new().max_connections(MAX_CONNECTIONS),
            url:          DB_URL,
        },
        ..Default::default()
    };

    Server::setup_with(config)
        .GET("/json",      || async {Response::OK(json!("message": "Hello, World!"))})
        .GET("/plaintext", || async {Response::OK("Hello, World!")})
        .GET("/db",        handle_db)
        .GET("/fortune",  handle_fortune)
        .GET("/queries",   handle_queries)
        .GET("/updates",   handle_updates)
        .serve_on(":8080")
}

async fn handle_db(ctx: Context) -> Result<Response> {
    let id = random_i32();
    let world = sqlx::query_as::<_, World>(
        "SELECT id, randomnumber FROM world WHERE id = $1"
    ).bind(id)
        .fetch_one(ctx.pool())
        .await?;
    Response::OK(json(&world)?)
}

async fn handle_fortune(ctx: Context) -> Result<Response> {
    let mut fortunes = sqlx::query_as::<_, Fortune>(
        "SELECT id, message FROM fortune"
    )
        .fetch_all(ctx.pool())
        .await?;
    fortunes.push(Fortune {
        id:      0,
        message: "Additional fortune added at request time.".into(),
    });
    fortunes.sort_unstable_by(|it, next| it.message.cmp(&next.message));
    Response::OK(html_from(fortunes))
}

async fn handle_queries(ctx: Context) -> Result<Response> {
    let count = {
        let queries = ctx.query::<&str>("q").unwrap_or("1").parse::<usize>().unwrap_or(1);
        if queries < 1 {1} else if 500 < queries {500} else {queries}
    };
    let mut worlds = Vec::with_capacity(count);
    for id in random_i32s(count) {
        worlds.push(
            sqlx::query_as::<_, World>(
                "SELECT id, randomnumber FROM world WHERE id = $1"
            ).bind(id)
                .fetch_one(ctx.pool())
                .await?
        )
    }
    Response::OK(json(&worlds)?)
}

async fn handle_updates(ctx: Context) -> Result<Response> {
    let count = {
        let queries = ctx.query::<&str>("q").unwrap_or("1").parse::<usize>().unwrap_or(1);
        if queries < 1 {1} else if 500 < queries {500} else {queries}
    };
    let mut worlds = Vec::with_capacity(count);
    let mut new_randomnumbers = random_i32s(count);
    for id in random_i32s(count) {
        let randomnumber = new_randomnumbers.next().unwrap();
        worlds.push(
            sqlx::query_as::<_, World>(
                "UPDATE world SET randomnumber = $1 WHERE id = $2 RETURNING id, randomnumber"
            ).bind(randomnumber).bind(id)
                .fetch_one(ctx.pool())
                .await?
        )
    }
    Response::OK(json(&worlds)?)
}
