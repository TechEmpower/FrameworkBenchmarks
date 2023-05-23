FROM lukemathwalker/cargo-chef:latest-rust-1 AS chef
ENV RUSTFLAGS "-C target-cpu=native"
# Needed for using snmalloc_rs
RUN apt-get update -yqq && apt-get install -yqq cmake g++
WORKDIR /app

FROM chef AS planner
COPY . .
RUN cargo chef prepare --recipe-path recipe.json

FROM chef AS builder 
COPY --from=planner /app/recipe.json recipe.json
# Build dependencies - this is the caching Docker layer!
RUN cargo chef cook --release --recipe-path recipe.json
# Build application
COPY . .
RUN cargo build --release --bin api

# We do not need the Rust toolchain to run the binary!
FROM debian:buster-slim AS runtime
WORKDIR /app
COPY --from=builder /app/target/release/api api
EXPOSE 8000
CMD ./api