FROM rust:1.68.2

ENV TECHEMPOWER_POSTGRES_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world

ADD ./ /salvo
WORKDIR /salvo

ENV RUSTFLAGS "-C target-cpu=native"
RUN cargo build --release

EXPOSE 8080

CMD ./target/release/main-pg
