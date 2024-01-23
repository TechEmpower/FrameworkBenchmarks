#[macro_use]
extern crate diesel;

mod db;
mod handlers;
mod middleware;
mod models;
mod rand;
mod schema;

use db::Pool;

/// Configure server routing to handlers.
fn configure(app: &mut tide::Server<Pool>) {
    // 1. JSON Serialization
    app.at("/json").get(handlers::json);

    // 2. Single Database Query
    app.at("/db").get(handlers::db);

    // 3. Multiple Database Queries
    //
    // AFAIK tide doesn't support optional route paramters.
    //
    // In the cases where the count parameter isn't given, we still want
    // to use the same handler.
    //
    // Using a query parameter might be more idiomatic.
    app.at("/queries/").get(handlers::queries);
    app.at("/queries/:count").get(handlers::queries);

    // 4. Fortunes
    app.at("/fortunes").get(handlers::fortunes);

    // 5. Database Updates
    app.at("/updates/").get(handlers::updates);
    app.at("/updates/:count").get(handlers::updates);

    // 6. Plaintext
    app.at("/plaintext").get(handlers::plaintext);
}

#[async_std::main]
async fn main() -> Result<(), std::io::Error> {
    let pool = db::connect();
    let mut app = tide::with_state(pool);
    configure(&mut app);

    // The benchmarks require the 'server' header is set on every response
    app.with(middleware::ServerHeader);

    // Uncomment these lines to enable debug logging
    // tide::log::start();
    // app.with(tide::log::LogMiddleware::new());

    app.listen("0.0.0.0:8080").await?;
    Ok(())
}
