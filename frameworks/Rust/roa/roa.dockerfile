FROM rust:1.44

RUN apt-get update -yqq && apt-get install -yqq cmake g++

ADD ./ /roa
WORKDIR /roa

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release --bin roa

CMD ./target/release/roa
