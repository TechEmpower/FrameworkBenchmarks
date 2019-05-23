FROM rust:1.34.1

ADD ./ /actix
WORKDIR /actix

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

CMD ./target/release/actix-diesel
