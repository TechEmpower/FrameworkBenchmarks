#![allow(non_snake_case)]
use anansi::{record, Relate, FromParams};
use anansi::records::{BigInt, Text};
use serde::Serialize;

#[record(table_name = "World")]
#[derive(Relate, FromParams, Serialize)]
pub struct World {
    pub randomNumber: BigInt,
}

#[record(table_name = "Fortune")]
#[derive(Relate, FromParams)]
pub struct Fortune {
    pub message: Text,
}

impl Fortune {
    pub fn additional() -> Self {
        Self {id: BigInt::new(0), message: Text::from("Additional fortune added at request time.".to_string())}
    }
}
