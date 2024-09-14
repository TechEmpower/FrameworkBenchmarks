FROM rust:1.78-buster

ENV TECHEMPOWER_POSTGRES_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world
ENV TECHEMPOWER_MAX_POOL_SIZE=28

ADD ./ /salvo
WORKDIR /salvo

ENV RUSTFLAGS "-C target-cpu=native"
RUN cargo build --release

EXPOSE 8080

CMD ./target/release/main-pg-pool
