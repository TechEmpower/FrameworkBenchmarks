pub(crate) mod consts {
    use std::ops::RangeInclusive;

    pub const RAND_RANGE: RangeInclusive<usize>  = 1..=10000;
    pub const DB_URL:              &'static str  = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world?sslmode=disable";
    pub const MAX_CONNECTIONS:     u32           = 10000;
}

pub(crate) mod models {
    use serde::Serialize;
    use sqlx::FromRow;
    use yarte::Template;

    #[derive(FromRow, Serialize)]
    pub struct World {
        id:           i32,
        randomnumber: i32,
    } impl World {
        pub fn set_randomnumber(&mut self, new_randomnumber: i32) {
            self.randomnumber = new_randomnumber
        }
    }

    #[derive(FromRow, Serialize)]
    pub struct Fortune {
        pub id:      i32,
        pub message: String,
    }
    #[derive(Template)]
    #[template(path = "fortunes.hbs")]
    pub(crate) struct FortunesTemplate {
        pub(crate) fortunes: Vec<Fortune>
    }
}

pub(crate) mod functions {
    use ohkami::{prelude::Body, result::{Result, ElseResponseWithErr}, response::Response};
    use rand::Rng;
    use yarte::Template;
    use super::{models::{Fortune, FortunesTemplate}, consts::RAND_RANGE};

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
    pub fn render_html(fortunes: Vec<Fortune>) -> Result<Response> {
        Response::OK(Body::html(
            FortunesTemplate {fortunes}
                .call()
                ._else(|_| Response::InternalServerError("failed to render template"))?
        ))
    }
}
