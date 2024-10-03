use xitca_io::bytes::BytesMut;
use xitca_postgres::{
    statement::{Statement, StatementNamed},
    types::Type,
};

use crate::util::{bulk_update_gen, Rand};

pub(super) type Shared = (Rand, BytesMut);

pub(super) const FORTUNE_STMT: StatementNamed = Statement::named("SELECT * FROM fortune", &[]);
pub(super) const WORLD_STMT: StatementNamed = Statement::named("SELECT * FROM world WHERE id=$1", &[Type::INT4]);

pub(super) fn update_query(num: usize) -> Box<str> {
    bulk_update_gen(|query| {
        use std::fmt::Write;
        (1..=num).fold((1, query), |(idx, query), _| {
            write!(query, "(${}::int,${}::int),", idx, idx + 1).unwrap();
            (idx + 2, query)
        });
    })
    .into_boxed_str()
}

pub(super) fn sort_update_params(params: &[[i32; 2]]) -> impl ExactSizeIterator<Item = i32> {
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
