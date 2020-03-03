#[macro_use]
extern crate rouille;
#[macro_use]
extern crate serde_json;

fn main() {
    rouille::start_server("0.0.0.0:8080", move |req| {
        router!(req,
            (GET) (/plaintext) => {
                rouille::Response::from_data("text/plain", "Hello, World!")
            },
            (GET) (/json) => {
                let json = json!({"message": "Hello, World!"});
                rouille::Response::from_data("application/json", json.to_string())
            },
            _ => rouille::Response::empty_404()
        )
    });
}
