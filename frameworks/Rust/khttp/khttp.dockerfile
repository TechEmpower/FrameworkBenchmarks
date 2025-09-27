FROM docker.io/rust:1.88

RUN apt-get update -yqq && apt-get install -yqq cmake g++

ADD ./ /app
WORKDIR /app

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

EXPOSE 8080

CMD ./target/release/khttp-techempower
