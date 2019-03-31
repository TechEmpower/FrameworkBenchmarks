FROM rust:1.33.0

ADD ./ /actix
WORKDIR /actix

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

CMD RUST_BACKTRACE=1 ./target/release/actix-platform
