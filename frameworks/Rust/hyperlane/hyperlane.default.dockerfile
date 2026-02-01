FROM rust:1.93

RUN apt-get update -yqq && apt-get install -yqq cmake g++ binutils lld

ENV POSTGRES_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world

ADD ./ /hyperlane_techempower
WORKDIR /hyperlane_techempower

RUN cargo clean
RUN cargo build --release

EXPOSE 8080

CMD ./target/release/hyperlane_techempower
