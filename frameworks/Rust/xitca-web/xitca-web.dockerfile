FROM rustlang/rust:nightly-slim

RUN apt-get update -yqq && apt-get install -yqq cmake g++

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN cargo clean
RUN rustup default nightly-2021-07-26
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

EXPOSE 8080

CMD ./target/release/xitca-web
