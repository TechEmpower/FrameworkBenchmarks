use askama::Template;
use serde::{Serialize, Deserialize};

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
