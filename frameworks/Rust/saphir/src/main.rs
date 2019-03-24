extern crate saphir;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate serde;

use saphir::*;

static HELLO_WORLD: &'static [u8] = b"Hello, world!";

#[derive(Serialize)]
struct ResponseJsonBody<'t0> {
    message: &'t0 str,
}

fn main() {
    let server_builder = Server::builder();

    let server = server_builder
        .configure_router(|router| {
            let plaintext = BasicController::new("^/plaintext", ());
            plaintext.add(Method::GET, reg!("^/$"), |_, _, res| { 
                res.header(header::CONTENT_TYPE, "text/plain")
                    .header(header::SERVER, "Saphir")
                    .status(StatusCode::OK)
                    .body(HELLO_WORLD); 
            });

            let json = BasicController::new("^/json", ());
            json.add(Method::GET, reg!("^/$"), |_, _, res| { 
                let body = ResponseJsonBody{ message: "hello, world!" };
                let json_str = ::serde_json::to_string(&body).expect("this body serialization should never fail");
                res.header(header::CONTENT_TYPE, "application/json")
                    .header(header::SERVER, "Saphir")
                    .status(StatusCode::OK)
                    .body(json_str);
            });

            router.add(plaintext)
            .add(json)
        })
        .configure_listener(|listener_config| {
            listener_config.set_uri("http://0.0.0.0:8080")
                .set_request_timeout_ms(5000) // 10 sec
                .set_panic_handler(|panic| {
                    println!("HA HA! : {:?}", panic);
                })
        })
        .build();

    server.run().unwrap();
}