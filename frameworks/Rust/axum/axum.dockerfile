FROM rust:1.55-slim-buster

ENV AXUM_TECHEMPOWER_DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world
ENV RUST_LOG=axum_techempower=debug

RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev pkg-config libssl-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /axum
COPY ./src ./src
COPY ./templates ./templates
COPY ./Cargo.toml ./Cargo.toml
COPY ./Cargo.lock ./Cargo.lock

ENV RUSTFLAGS "-C target-cpu=native"
RUN cargo build --release

RUN ls -la target/release/axum_techempower

EXPOSE 8000

CMD ["./target/release/axum_techempower"]
