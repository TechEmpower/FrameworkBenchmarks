FROM rust:1.25

ADD ./ /tokio
WORKDIR /tokio

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

CMD ./target/release/tokio-minihttp
