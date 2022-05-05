FROM rust:1.60-slim-buster

ENV AXUM_TECHEMPOWER_DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world
ENV AXUM_TECHEMPOWER_MAX_POOL_SIZE=56
ENV AXUM_TECHEMPOWER_MIN_POOL_SIZE=56

RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev pkg-config libssl-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /axum
COPY ./src ./src
COPY ./templates ./templates
COPY ./Cargo.toml ./Cargo.toml
COPY ./Cargo.lock ./Cargo.lock
COPY ./run.sh ./run.sh
RUN chmod +x ./run.sh

ENV RUSTFLAGS "-C target-cpu=native"
RUN cargo build --release
RUN cp ./target/release/axum-sqlx ./target/release/axum-techempower


EXPOSE 8000

CMD ["./run.sh"]
