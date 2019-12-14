FROM rust:1.38

RUN apt update -yqq && apt install -yqq cmake

ADD ./ /may
WORKDIR /may

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

CMD ./target/release/may-minihttp
