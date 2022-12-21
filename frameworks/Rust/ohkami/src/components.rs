pub(crate) mod consts {
    use std::ops::RangeInclusive;

    pub const RAND_RANGE: RangeInclusive<usize>  = 1..=10000;
    pub const DB_URL:              &'static str  = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world?sslmode=disable";
    pub const MAX_CONNECTIONS:     u32           = 10000;
}

pub(crate) mod models {
    use serde::Serialize;
    use sqlx::FromRow;

    #[derive(FromRow, Serialize)]
    pub struct World {
        id:           i32,
        randomnumber: i32,
    }
    #[derive(FromRow, Serialize)]
    pub struct Fortune {
        pub id:      i32,
        pub message: String,
    }
}

pub(crate) mod functions {
    use rand::Rng;
    use ohkami::response::Body;
    use super::{models::Fortune, consts::RAND_RANGE};

    pub fn random_i32() -> i32 {
        rand::thread_rng().gen_range(RAND_RANGE) as i32
    }
    pub fn random_i32s(n: usize) -> std::vec::IntoIter<i32> {
        let mut generator = rand::thread_rng();
        let mut i32s = Vec::with_capacity(n);
        for _ in 0..n {
            i32s.push(generator.gen_range(RAND_RANGE) as i32)
        }
        i32s.into_iter()
    }
    pub fn html_from(fortunes: Vec<Fortune>) -> Body {
        Body::text_html(fortunes
            .into_iter()
            .fold(
                String::from("<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>"),
                |it, next| it + &format!("<tr><td>{}</td><td>{}</td></tr>", next.id, next.message)
            ) + "</table></body></html>"
        )
    }
}
