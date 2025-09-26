//! Database schema as defined by the benchmark fixtures.

table! {
    world (id) {
        id -> Integer,
        randomnumber -> Integer,
    }
}

table! {
    fortune (id) {
        id -> Integer,
        message -> Text,
    }
}
