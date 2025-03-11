FROM rust:1.85

RUN apt-get update -yqq && apt-get install -yqq cmake g++

ADD ./ /hyperlane_techempower
WORKDIR /hyperlane_techempower

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

EXPOSE 60000

CMD ./target/release/hyperlane_techempower
