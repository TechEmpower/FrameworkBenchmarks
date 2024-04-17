use ohkami::typed::{Payload, Query};
use ohkami::builtin::payload::JSON;


#[Payload(JSON/S)]
pub struct Message {
    pub message: &'static str,
}

#[derive(sqlx::FromRow)]
pub struct Fortune {
    pub id:      i32,
    pub message: String,
}

#[derive(sqlx::FromRow)]
#[Payload(JSON/S)]
pub struct World {
    pub id:           i32,
    #[serde(rename="randomNumber")]
    pub randomnumber: i32,
}

#[Query]
pub struct WorldsQuery<'q> {
    q: Option<&'q str>,
}
impl WorldsQuery<'_> {
    #[inline(always)]
    pub fn parse(self) -> usize {
        match self.q.unwrap_or("1").parse::<usize>().unwrap_or(1) {
            n @ 1..=500 => n,
            0           => 1,
            501..       => 500,
        }
    }
}
