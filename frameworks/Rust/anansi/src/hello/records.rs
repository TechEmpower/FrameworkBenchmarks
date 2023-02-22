#![allow(non_snake_case)]
use async_trait::async_trait;
use anansi::web::{Result, BaseRequest};
use anansi::record;
use anansi::records::{Relate, Int, Text, random_int};
use serde::Serialize;

#[record(table_name = "World")]
#[derive(Serialize)]
pub struct World {
    #[field(primary_key = "true", default_fn = "random_int")]
    pub id: Int,
    pub randomNumber: Int,
}

#[async_trait]
impl<R: BaseRequest> Relate<R> for World {
    async fn on_save(&self, _req: &mut R) -> Result<()> {
        unimplemented!();
    }
    async fn on_delete(&self, _req: &R) -> Result<()> {
        unimplemented!();
    }
}

#[record(table_name = "Fortune")]
pub struct Fortune {
    #[field(primary_key = "true", default_fn = "random_int")]
    pub id: Int,
    pub message: Text,
}

impl Fortune {
    pub fn additional() -> Self {
        Self {id: Int::new(0), message: Text::from("Additional fortune added at request time.".to_string())}
    }
}

#[async_trait]
impl<R: BaseRequest> Relate<R> for Fortune {
    async fn on_save(&self, _req: &mut R) -> Result<()> {
        unimplemented!();
    }
    async fn on_delete(&self, _req: &R) -> Result<()> {
        unimplemented!();
    }
}
