FROM rust:1.46

# Disable simd at jsonescape
# ENV CARGO_CFG_HTMLESCAPE_DISABLE_AUTO_AVX=

RUN apt-get update -yqq && apt-get install -yqq cmake g++

ADD ./ /ntex
WORKDIR /ntex

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

CMD ./target/release/ntex-yarte
