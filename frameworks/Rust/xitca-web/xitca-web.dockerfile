FROM rustlang/rust:nightly-slim

RUN apt-get update -yqq && apt-get install -yqq cmake g++

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

EXPOSE 8080

CMD ./target/release/xitca-web

