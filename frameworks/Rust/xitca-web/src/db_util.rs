use xitca_postgres::{
    statement::{Statement, StatementNamed},
    types::Type,
};

use crate::util::Error;

pub const FORTUNE_STMT: StatementNamed = Statement::named("SELECT id,message FROM fortune", &[]);

pub const WORLD_STMT: StatementNamed = Statement::named("SELECT id,randomnumber FROM world WHERE id=$1", &[Type::INT4]);

pub const UPDATE_STMT: StatementNamed = Statement::named(
    "UPDATE world SET randomnumber=w.r FROM (SELECT unnest($1) as i,unnest($2) as r) w WHERE world.id=w.i",
    &[Type::INT4_ARRAY, Type::INT4_ARRAY],
);

#[cold]
#[inline(never)]
pub fn not_found() -> Error {
    "request World does not exist".into()
}
