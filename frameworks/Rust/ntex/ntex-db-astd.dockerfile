FROM rust:1.58.0

# Disable simd at jsonescape
# ENV CARGO_CFG_JSONESCAPE_DISABLE_AUTO_SIMD=

RUN apt-get update -yqq && apt-get install -yqq cmake g++

ADD ./ /ntex
WORKDIR /ntex

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release --features="async-std"

EXPOSE 8080

CMD ./target/release/ntex-db-astd
