FROM rust:latest

RUN apt-get update -yqq && apt-get install -yqq cmake g++

ADD ./ /water
WORKDIR /water

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release --bin json --features json_plaintext

EXPOSE 8080

CMD ./target/release/json