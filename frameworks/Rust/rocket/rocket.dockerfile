FROM rust:1.59-slim-buster

ENV ROCKET_BENCHMARK_DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world

RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev \
&& rm -rf /var/lib/apt/lists/*

ADD ./ /rocket
WORKDIR /rocket

RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

EXPOSE 8000

CMD ./target/release/rocket_techempower
