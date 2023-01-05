FROM rust:1.59.0

RUN apt-get update -yqq && apt-get install -yqq cmake g++

ADD ./ /roa
WORKDIR /roa

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release --bin roa

EXPOSE 8080

CMD ./target/release/roa
