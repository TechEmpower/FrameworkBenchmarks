// @generated automatically by Diesel CLI.

diesel::table! {
    Fortune (id) {
        id -> Int4,
        message -> Varchar,
    }
}

diesel::table! {
    World (id) {
        id -> Int4,
        randomnumber -> Int4,
    }
}

diesel::table! {
    fortune (id) {
        id -> Int4,
        message -> Varchar,
    }
}

diesel::table! {
    world (id) {
        id -> Int4,
        randomnumber -> Int4,
    }
}

diesel::allow_tables_to_appear_in_same_query!(Fortune, World, fortune, world,);
