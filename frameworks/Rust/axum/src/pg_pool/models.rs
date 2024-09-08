use serde::{Deserialize, Serialize};
use tokio_pg_mapper_derive::PostgresMapper;

#[allow(non_snake_case)]
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize, PostgresMapper)]
#[pg_mapper(table = "Fortune")]
pub struct Fortune {
    pub id: i32,
    pub message: String,
}

#[allow(non_snake_case)]
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize, PostgresMapper)]
#[pg_mapper(table = "World")]
pub struct World {
    pub id: i32,
    #[serde(rename = "randomNumber")]
    pub randomnumber: i32,
}
