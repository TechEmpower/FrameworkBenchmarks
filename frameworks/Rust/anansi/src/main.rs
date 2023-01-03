use anansi::*;
use anansi::util::{auth, sessions, admin};

mod urls;
mod project;
mod http_errors;
mod hello;

apps! {
    auth,
    sessions,
    hello,
}

app_statics! {
    admin,
}

app_admins! {
    auth,
}

main!();
