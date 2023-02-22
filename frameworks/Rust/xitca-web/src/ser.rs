use std::borrow::Cow;

#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct Message {
    message: &'static str,
}

impl Message {
    #[allow(dead_code)]
    #[inline]
    pub(super) const fn new() -> Self {
        Self {
            message: "Hello, World!",
        }
    }
}

#[allow(non_snake_case)]
#[derive(Debug)]
#[cfg_attr(feature = "orm", derive(Queryable))]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct World {
    pub id: i32,
    pub randomnumber: i32,
}

impl World {
    #[allow(dead_code)]
    #[inline]
    pub const fn new(id: i32, randomnumber: i32) -> Self {
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

#[cfg_attr(
    feature = "sailfish",
    derive(sailfish::TemplateOnce),
    template(path = "fortune.stpl", rm_whitespace = true)
)]
pub struct Fortunes {
    items: Vec<Fortune>,
}

impl Fortunes {
    #[inline]
    pub const fn new(items: Vec<Fortune>) -> Self {
        Self { items }
    }
}
