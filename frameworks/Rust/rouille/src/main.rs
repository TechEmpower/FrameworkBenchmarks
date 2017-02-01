#[macro_use] extern crate rouille;
#[macro_use] extern crate serde_json;

fn main() {
    rouille::start_server("0.0.0.0:8080", move |req| {
        router!(req,
                (GET) (/plaintext) => {
                    rouille::Response::text("Hello, World!")
                },
                (GET) (/json) => {
                    let json = json!({"message": "Hello, World!"});
                    rouille::Response::text(json.to_string())
                },
                _ => rouille::Response::empty_404()
            )
    });
}
