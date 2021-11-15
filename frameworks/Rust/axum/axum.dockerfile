FROM rust:1.55-slim-buster

RUN apt-get update && apt-get install -y --no-install-recommends \
    pkg-config libssl-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /axum
COPY ./src ./src
COPY ./templates ./templates
COPY ./Cargo.toml ./Cargo.toml
COPY ./Cargo.lock ./Cargo.lock

ENV RUSTFLAGS "-C target-cpu=native"
RUN cargo build --release

EXPOSE 8000

CMD ["./target/release/axum"]
