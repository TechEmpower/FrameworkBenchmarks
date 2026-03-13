FROM rust:1.91-slim-bookworm AS builder

RUN apt-get update && apt-get install -y --no-install-recommends \
    pkg-config libssl-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build

# Copy manifests and fetch dependencies first (layer caching)
COPY ./Cargo.toml /build/
COPY ./benchmark/Cargo.toml /build/benchmark/
RUN mkdir -p benchmark/src && echo "fn main() {}" > benchmark/src/main.rs \
    && cargo fetch \
    && rm -rf benchmark/src

# Copy source and build
COPY ./benchmark/ /build/benchmark/

ENV RUSTFLAGS="-C target-cpu=native"
RUN cargo build --release -p chopin-benchmark

FROM debian:bookworm-slim

COPY --from=builder /build/target/release/chopin-benchmark /app/chopin-benchmark
EXPOSE 8000
CMD ["/app/chopin-benchmark"]
