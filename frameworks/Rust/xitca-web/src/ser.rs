use std::borrow::Cow;

use diesel::Queryable;
use sailfish::TemplateOnce;
use serde::{Deserialize, Serialize};

#[derive(Deserialize, Serialize)]
pub struct Message {
    message: &'static str,
}

impl Message {
    #[inline]
    #[allow(dead_code)]
    pub(super) fn new() -> Self {
        Self {
            message: "Hello, World!",
        }
    }
}

#[allow(non_snake_case)]
#[derive(Debug, Serialize, Queryable)]
pub struct World {
    pub id: i32,
    pub randomnumber: i32,
}

impl World {
    #[inline]
    #[allow(dead_code)]
    pub fn new(id: i32, randomnumber: i32) -> Self {
        Self { id, randomnumber }
    }
}

#[derive(Queryable)]
pub struct Fortune {
    pub id: i32,
    pub message: Cow<'static, str>,
}

impl Fortune {
    #[inline]
    pub fn new(id: i32, message: impl Into<Cow<'static, str>>) -> Self {
        Self {
            id,
            message: message.into(),
        }
    }
}

#[derive(TemplateOnce)]
#[template(path = "fortune.stpl", rm_whitespace = true)]
pub struct Fortunes {
    items: Vec<Fortune>,
}

impl Fortunes {
    #[inline]
    pub fn new(items: Vec<Fortune>) -> Self {
        Self { items }
    }
}
