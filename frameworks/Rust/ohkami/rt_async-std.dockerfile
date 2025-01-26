FROM rust:1.84-slim-bookworm AS builder

RUN apt update && apt install -y --no-install-recommends \
    libpq-dev pkg-config libssl-dev \
    && rm -rf /var/lib/apt/lists/*

COPY ./Cargo.toml    /build/
COPY ./Cargo.lock    /build/
COPY ./src/          /build/src/
COPY ./rt_async-std/ /build/rt_async-std/
    
WORKDIR /build/rt_async-std
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

##########################################################

FROM gcr.io/distroless/cc-debian12

COPY --from=builder /build/rt_async-std/target/release/framework_benchmarks-async-std /app/

EXPOSE 8000
ENV DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world
ENV MAX_CONNECTIONS=56
ENV MIN_CONNECTIONS=56
CMD /app/framework_benchmarks-async-std
