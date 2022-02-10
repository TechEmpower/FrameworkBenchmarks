FROM rust:1.58

RUN apt-get update -yqq && apt-get install -yqq cmake g++

ADD ./ /actix
WORKDIR /actix

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

EXPOSE 8080

CMD ./target/release/tfb-web-diesel
