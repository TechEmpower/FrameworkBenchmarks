FROM rust:1.44

RUN apt-get update -yqq && apt-get install -yqq cmake

ADD ./ /may
WORKDIR /may

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

EXPOSE 8080

CMD ./target/release/may-minihttp
