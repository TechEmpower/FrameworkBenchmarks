FROM rust:1.84-slim-bookworm AS builder

RUN apt update && apt install -y --no-install-recommends \
    libpq-dev pkg-config libssl-dev \
    && rm -rf /var/lib/apt/lists/*

COPY ./rt_glommio ./Cargo.toml ./Cargo.lock ./src /build/

WORKDIR /build/rt_glommio
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

##########################################################

FROM gcr.io/distroless/cc-debian12

COPY --from=builder /build/rt_glommio/target/release/framework_benchmarks-glommio /app/

EXPOSE 8000
ENV DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world
ENV MAX_CONNECTIONS=56
ENV MIN_CONNECTIONS=56
CMD /app/framework_benchmarks-glommio
