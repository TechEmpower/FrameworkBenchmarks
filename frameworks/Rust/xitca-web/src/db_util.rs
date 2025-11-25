#[cfg(feature = "diesel")]
// diesel does not support high level bulk update api. use raw sql to bypass the limitation.
// relate discussion: https://github.com/diesel-rs/diesel/discussions/2879
pub fn update_query_from_ids(mut rngs: Vec<(i32, i32)>) -> String {
    rngs.sort_by(|(a, _), (b, _)| a.cmp(b));

    const PREFIX: &str = "UPDATE world SET randomNumber=w.r FROM (VALUES ";
    const SUFFIX: &str = ") AS w (i,r) WHERE world.id=w.i";

    let mut query = String::from(PREFIX);

    use core::fmt::Write;
    rngs.iter().for_each(|(w_id, num)| {
        write!(query, "({}::int,{}::int),", w_id, num).unwrap();
    });

    if query.ends_with(',') {
        query.pop();
    }

    query.push_str(SUFFIX);

    query
}

#[cfg(feature = "pg")]
pub use pg::*;

#[cfg(feature = "pg")]
pub mod pg {
    #![allow(dead_code)]

    use xitca_io::bytes::BytesMut;
    use xitca_postgres::{
        statement::{Statement, StatementNamed},
        types::Type,
    };

    use crate::util::{Error, Rand};

    pub type Shared = (Rand, BytesMut);

    pub const FORTUNE_STMT: StatementNamed = Statement::named("SELECT id,message FROM fortune", &[]);
    pub const WORLD_STMT: StatementNamed =
        Statement::named("SELECT id,randomnumber FROM world WHERE id=$1", &[Type::INT4]);
    pub const UPDATE_STMT: StatementNamed = Statement::named(
        "UPDATE world SET randomnumber=$1 WHERE id=$2",
        &[Type::INT4, Type::INT4],
    );
    pub const UPDATE_BATCH_STMT: StatementNamed = Statement::named(
        "UPDATE world SET randomnumber=w.r FROM (SELECT unnest($1) as i,unnest($2) as r) w WHERE world.id=w.i",
        &[Type::INT4_ARRAY, Type::INT4_ARRAY],
    );

    #[cold]
    #[inline(never)]
    pub fn not_found() -> Error {
        "request World does not exist".into()
    }
}
