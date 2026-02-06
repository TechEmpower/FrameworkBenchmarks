FROM rust:1.93

RUN apt-get update -yqq && apt-get install -yqq cmake g++

WORKDIR /water
COPY . .

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release --bin water-http --features all

EXPOSE 8080

CMD ./target/release/water-http