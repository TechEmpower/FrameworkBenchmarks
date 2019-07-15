FROM rust:latest

ADD ./ /actix
WORKDIR /actix

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

CMD ./target/release/actix-platform
