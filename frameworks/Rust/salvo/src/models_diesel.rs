use diesel::Queryable;
use serde::Serialize;

#[allow(non_snake_case)]
#[derive(Serialize, Queryable, Clone, Debug)]
pub struct World {
    pub id: i32,
    pub randomnumber: i32,
}

#[allow(non_snake_case)]
#[derive(Serialize, Queryable, Debug)]
pub struct Fortune {
    pub id: i32,
    pub message: String,
}
