#![allow(non_snake_case)]

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
