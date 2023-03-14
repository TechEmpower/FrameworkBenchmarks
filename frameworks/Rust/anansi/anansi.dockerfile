FROM rust:1.64

RUN apt-get update -yqq && apt-get install -yqq --no-install-recommends redis-server

ADD ./ /anansi
WORKDIR /anansi

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

EXPOSE 8080

ENV RUST_LOG=off
CMD service redis-server start && ./target/release/tfb-anansi 0.0.0.0:8080
