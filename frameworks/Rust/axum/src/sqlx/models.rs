use std::borrow::Cow;

use serde::{Deserialize, Serialize};
use sqlx::{FromRow, Row};
use sqlx::postgres::PgRow;

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct Fortune {
    pub id: i32,
    pub message: Cow<'static, str>,
}

impl FromRow<'_, PgRow> for Fortune {
  fn from_row(row: &PgRow) -> Result<Self, sqlx::Error> {
    Ok(Fortune {
        id: row.try_get(0usize)?,
        message: Cow::Owned(row.try_get(1usize)?)
      })
  }
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize, FromRow)]
pub struct World {
    pub id: i32,
    #[sqlx(rename = "randomnumber")]
    #[serde(rename = "randomNumber")]
    pub random_number: i32,
}
