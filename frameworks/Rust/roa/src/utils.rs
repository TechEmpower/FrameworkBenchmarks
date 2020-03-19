use lazy_static::lazy_static;
use askama::Template;
use serde::{Deserialize, Serialize};
use roa::http::header::HeaderValue;

lazy_static! {
    pub static ref SERVER_HEADER: HeaderValue = HeaderValue::from_static("roa");
    pub static ref JSON_LEN: HeaderValue = HeaderValue::from_static("27");
    pub static ref PLAINTEXT_LEN: HeaderValue = HeaderValue::from_static("13");
}

#[derive(Serialize, Debug)]
pub struct Fortune {
    pub id: i32,
    pub message: String,
}

#[derive(Serialize, Deserialize)]
pub struct Message {
    pub message: &'static str,
}

#[derive(Template)]
#[template(path = "fortune.html")]
pub struct Fortunes {
    pub items: Vec<Fortune>,
}
