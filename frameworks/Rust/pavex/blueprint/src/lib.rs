use pavex_builder::{f, router::GET, Blueprint};
use pavex_runtime::{
    http::HeaderValue,
    response::{IntoResponse, Response},
};

/// Return the application blueprint that will be used by `pavex`
/// to generate the application runtime code.
pub fn blueprint() -> Blueprint {
    let mut bp = Blueprint::new();
    bp.route(GET, "/plaintext", f!(crate::plaintext));
    bp
}

pub fn plaintext() -> Response {
    let mut response = "Hello, World!".into_response();
    response
        .headers_mut()
        .insert("Server", HeaderValue::from_static("pavex"));
    response
}
