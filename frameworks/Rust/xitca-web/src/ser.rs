use std::borrow::Cow;

use sailfish::TemplateOnce;

#[cfg(feature = "serde")]
use serde::Serialize;
#[cfg(feature = "simd")]
use simd_json_derive::Serialize;

#[derive(Serialize)]
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
#[cfg_attr(feature = "orm", derive(Queryable))]
#[derive(Debug, Serialize)]
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

#[cfg_attr(feature = "orm", derive(Queryable))]
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
