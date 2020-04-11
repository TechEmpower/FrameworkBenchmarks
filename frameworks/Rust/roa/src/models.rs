#![allow(dead_code)]

use askama::Template;
use serde::Serialize;

#[derive(Serialize)]
pub struct Message {
    pub message: &'static str,
}

#[cfg_attr(feature = "orm", derive(Queryable))]
#[allow(non_snake_case)]
#[derive(Serialize, Debug)]
pub struct World {
    pub id: i32,
    pub randomnumber: i32,
}

#[cfg_attr(feature = "orm", derive(Queryable))]
#[derive(Serialize, Debug)]
pub struct Fortune {
    pub id: i32,
    pub message: String,
}

#[derive(Template)]
#[template(path = "fortune.html")]
pub struct Fortunes<'a> {
    pub items: &'a [Fortune],
}
