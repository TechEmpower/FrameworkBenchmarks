diesel::table! {
    world (id) {
        id -> Integer,
        randomnumber -> Integer,
    }
}

diesel::table! {
    fortune (id) {
        id -> Integer,
        message -> Text,
    }
}
