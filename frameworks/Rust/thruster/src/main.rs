extern crate futures;
extern crate serde;
extern crate smallvec;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate thruster;

mod context;

use futures::future;

use context::{generate_context, Ctx};
use thruster::{App, MiddlewareChain, MiddlewareReturnValue};

#[derive(Serialize)]
struct Message<'a> {
    message: &'a str,
}

fn json(mut context: Ctx, _chain: &MiddlewareChain<Ctx>) -> MiddlewareReturnValue<Ctx> {
    let json = Message {
        message: "Hello, World!",
    };

    let val = serde_json::to_string(&json).unwrap();

    context.body = val;
    context.set_header("Content-Type", "application/json");

    Box::new(future::ok(context))
}

fn plaintext(mut context: Ctx, _chain: &MiddlewareChain<Ctx>) -> MiddlewareReturnValue<Ctx> {
    let val = "Hello, world!".to_owned();

    context.body = val;
    context.set_header("Content-Type", "text/plain");

    Box::new(future::ok(context))
}

fn main() {
    println!("Starting server...");

    let mut app = App::create(generate_context);

    app.get("/json", vec![json]);
    app.get("/plaintext", vec![plaintext]);

    App::start_small_load_optimized(app, "0.0.0.0", 8080);
}
