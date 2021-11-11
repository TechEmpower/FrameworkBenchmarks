#[derive(Serialize)]
pub struct Message {
    pub message: &'static str,
}

#[allow(non_snake_case)]
#[derive(Serialize, Queryable)]
pub struct World {
    pub id: i32,
    pub randomNumber: i32,
}

#[derive(Serialize, Queryable)]
pub struct Fortune {
    pub id: i32,
    pub message: String,
}
