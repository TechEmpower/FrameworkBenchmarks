use anansi::*;

mod urls;
mod project;
mod http_errors;
mod hello;

apps! {
    hello,
}

app_statics! {}

main!();
