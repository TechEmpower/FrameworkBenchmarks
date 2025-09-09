FROM rust:1.89-slim-bookworm AS builder

RUN apt update && apt install -y --no-install-recommends \
    pkg-config \
    libpq-dev libssl-dev \
    && rm -rf /var/lib/apt/lists/*

COPY ./Cargo.toml /build/
COPY ./src/       /build/src/
COPY ./rt_tokio/  /build/rt_tokio/
    
WORKDIR /build/rt_tokio
ENV RUSTFLAGS="-C target-cpu=native"
RUN cargo build --release

##########################################################

FROM gcr.io/distroless/cc-debian12

COPY --from=builder /build/rt_tokio/target/release/framework_benchmarks-tokio /app/

EXPOSE 8000
ENV DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world
ENV MAX_CONNECTIONS=56
ENV MIN_CONNECTIONS=56
CMD [ "/app/framework_benchmarks-tokio" ]
