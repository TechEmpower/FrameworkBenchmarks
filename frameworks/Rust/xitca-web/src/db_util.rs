use crate::util::Error;

#[cfg(any(feature = "pg-orm", feature = "pg-orm-async"))]
// diesel does not support high level bulk update api. use raw sql to bypass the limitation.
// relate discussion: https://github.com/diesel-rs/diesel/discussions/2879
pub fn update_query_from_ids(ids: &[(i32, i32)]) -> String {
    update_query(|query| {
        use core::fmt::Write;
        ids.iter().for_each(|(w_id, num)| {
            write!(query, "({}::int,{}::int),", w_id, num).unwrap();
        });
    })
}

fn update_query(func: impl FnOnce(&mut String)) -> String {
    const PREFIX: &str = "UPDATE world SET randomNumber = w.r FROM (VALUES ";
    const SUFFIX: &str = ") AS w (i,r) WHERE world.id = w.i";

    let mut query = String::from(PREFIX);

    func(&mut query);

    if query.ends_with(',') {
        query.pop();
    }

    query.push_str(SUFFIX);

    query
}

#[cold]
#[inline(never)]
pub fn not_found() -> Error {
    "request World does not exist".into()
}

#[cfg(feature = "pg")]
pub use pg::*;

#[cfg(feature = "pg")]
pub mod pg {
    use xitca_io::bytes::BytesMut;
    use xitca_postgres::{
        statement::{Statement, StatementNamed},
        types::Type,
    };

    use crate::util::Rand;

    pub type Shared = (Rand, BytesMut);

    pub const FORTUNE_STMT: StatementNamed = Statement::named("SELECT * FROM fortune", &[]);
    pub const WORLD_STMT: StatementNamed = Statement::named("SELECT * FROM world WHERE id=$1", &[Type::INT4]);

    pub fn update_query_from_num(num: usize) -> Box<str> {
        super::update_query(|query| {
            use core::fmt::Write;
            (1..=num).fold(1, |idx, _| {
                write!(query, "(${}::int,${}::int),", idx, idx + 1).unwrap();
                idx + 2
            });
        })
        .into_boxed_str()
    }

    pub fn sort_update_params(params: &[[i32; 2]]) -> impl ExactSizeIterator<Item = i32> {
        let mut params = params.to_owned();
        params.sort_by(|a, b| a[0].cmp(&b[0]));

        struct ParamIter<I>(I);

        impl<I> Iterator for ParamIter<I>
        where
            I: Iterator,
        {
            type Item = I::Item;

            #[inline]
            fn next(&mut self) -> Option<Self::Item> {
                self.0.next()
            }

            #[inline]
            fn size_hint(&self) -> (usize, Option<usize>) {
                self.0.size_hint()
            }
        }

        // impl depends on compiler optimization to flat Vec<[T]> to Vec<T> when inferring
        // it's size hint. possible to cause runtime panic.
        impl<I> ExactSizeIterator for ParamIter<I> where I: Iterator {}

        ParamIter(params.into_iter().flatten())
    }
}
