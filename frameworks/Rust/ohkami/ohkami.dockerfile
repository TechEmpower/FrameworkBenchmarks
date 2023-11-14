FROM rust:1.65

RUN apt update -yqq \
 && apt install -yqq cmake g++

ADD ./ /ohkami
WORKDIR /ohkami

RUN cargo clean \
 && RUSTFLAGS="-C target-cpu=native" cargo build --release

EXPOSE 8080
CMD ./target/release/ohkami