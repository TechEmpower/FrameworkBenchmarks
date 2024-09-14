FROM rust:1.80-slim-bullseye
WORKDIR /ohkami_framework_benchmarks

ENV DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world
ENV MAX_CONNECTIONS=56
ENV MIN_CONNECTIONS=56

COPY ./src        ./src
COPY ./Cargo.toml ./Cargo.toml
COPY ./Cargo.lock ./Cargo.lock

RUN apt update && apt install -y --no-install-recommends \
    libpq-dev pkg-config libssl-dev && \
    rm -rf /var/lib/apt/lists/* 

RUN RUSTFLAGS="-C target-cpu=native" cargo build --release
EXPOSE 8000
CMD ./target/release/ohkami_framework_benchmarks
