use std::borrow::Cow;

use diesel::Queryable;
use sailfish::TemplateOnce;
use serde::Serialize;

#[derive(Serialize, Queryable, Debug)]
pub struct World {
    pub id: i32,
    pub randomnumber: i32,
}

#[derive(Serialize, Queryable, Debug)]
pub struct Fortune {
    pub id: i32,
    pub message: Cow<'static, str>,
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
