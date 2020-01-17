use serde::Serialize;
use warp::http::header;
use warp::Filter;

#[derive(Serialize)]
struct Message {
    message: &'static str,
}

#[tokio::main]
async fn main() {
    let json = warp::path!("json").map(|| {
        warp::reply::json(&Message {
            message: "Hello, world!",
        })
    });
    let plaintext = warp::path!("plaintext").map(|| "Hello, World!");
    let routes = json
        .or(plaintext)
        .map(|reply| warp::reply::with_header(reply, header::SERVER, "warp"));
    warp::serve(routes).run(([0, 0, 0, 0], 8080)).await;
}
