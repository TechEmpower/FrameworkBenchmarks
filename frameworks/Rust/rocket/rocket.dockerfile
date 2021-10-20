FROM rust:1.55-slim-buster

ENV ROCKET_BENCHMARK_DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world

RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev \
&& rm -rf /var/lib/apt/lists/*

RUN rustup toolchain install nightly-2021-09-15 -t x86_64-unknown-linux-gnu --no-self-update --profile minimal

ADD ./ /rocket
WORKDIR /rocket

RUN RUSTFLAGS="-C target-cpu=native" cargo +nightly-2021-09-15 build --release

EXPOSE 8000

CMD ./target/release/rocket_benchmark
