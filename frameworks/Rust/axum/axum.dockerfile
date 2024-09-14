FROM docker.io/rust:1.80-slim-bookworm AS builder

RUN apt-get update && apt-get install -y --no-install-recommends \
    pkg-config libssl-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build
COPY ./Cargo.toml ./Cargo.lock /build/
RUN cargo fetch
COPY ./templates/ /build/templates
COPY ./src/ /build/src
ENV RUSTFLAGS "-C target-cpu=native"
RUN cargo build --release

FROM gcr.io/distroless/cc-debian12
ENV POSTGRES_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world
ENV POSTGRES_MIN_POOL_SIZE=56
ENV POSTGRES_MAX_POOL_SIZE=56
ENV MONGODB_URL=mongodb://tfb-database:27017
ENV MONGODB_MIN_POOL_SIZE=28
ENV MONGODB_MAX_POOL_SIZE=14
COPY --from=builder /build/target/release/axum* /app/
EXPOSE 8000
CMD ["/app/axum"]
