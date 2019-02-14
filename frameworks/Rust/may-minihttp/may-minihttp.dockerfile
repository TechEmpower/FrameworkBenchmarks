FROM rust:1.32.0

ADD ./ /may
WORKDIR /may

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

CMD ./target/release/may-minihttp
