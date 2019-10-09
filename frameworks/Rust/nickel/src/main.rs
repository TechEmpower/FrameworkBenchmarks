#[macro_use]
extern crate nickel;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

use nickel::{HttpRouter, MediaType, Nickel};

#[derive(Serialize, Deserialize)]
struct Message {
    message: String,
}

fn main() {
    let mut server = Nickel::new();
    let mut router = Nickel::router();

    router.get(
        "/json",
        middleware! { |_, mut response|
            response.set(MediaType::Json);
            let message: Message = Message{
                message: "Hello, World!".to_string(),
            };
            serde_json::to_string(&message).unwrap()
        },
    );

    router.get(
        "/plaintext",
        middleware! { |_, mut response|
            response.set(MediaType::Txt);
            "Hello, World!"
        },
    );

    server.utilize(router);
    server.listen("0.0.0.0:8080").unwrap();
}
