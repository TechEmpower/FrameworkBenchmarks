FROM rust:1.60.0

# Disable simd at jsonescape
# ENV CARGO_CFG_JSONESCAPE_DISABLE_AUTO_SIMD=

RUN apt-get update -yqq && apt-get install -yqq cmake g++

ADD ./ /ntex
WORKDIR /ntex

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release --features="tokio"

EXPOSE 8080

CMD ./target/release/ntex-plt
