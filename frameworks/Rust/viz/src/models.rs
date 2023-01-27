use std::borrow::Cow;

use serde::Serialize;

#[derive(Serialize, Debug, yarte::Serialize)]
pub struct World {
    pub id: i32,
    pub randomnumber: i32,
}

#[derive(Serialize, Debug)]
pub struct Fortune {
    pub id: i32,
    pub message: Cow<'static, str>,
}
