#![allow(non_snake_case)]

table! {
    World (id) {
        id -> Integer,
        randomnumber -> Integer,
    }
}

table! {
    Fortune (id) {
        id -> Integer,
        message -> Text,
    }
}
