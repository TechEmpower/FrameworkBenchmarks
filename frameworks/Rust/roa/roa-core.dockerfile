FROM rust:1.42

RUN apt-get update -yqq && apt-get install -yqq cmake g++

ADD ./ /roa
WORKDIR /roa

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release --bin roa-core

CMD ./target/release/roa-core
