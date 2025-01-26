FROM rust:1.84-slim-bookworm AS builder

RUN apt update && apt install -y --no-install-recommends \
    libpq-dev pkg-config libssl-dev \
    && rm -rf /var/lib/apt/lists/*

COPY ./Cargo.toml /build/
COPY ./Cargo.lock /build/
COPY ./src/       /build/src/
COPY ./rt_smol/   /build/rt_smol/
    
WORKDIR /build/rt_smol
ENV RUSTFLAGS="-C target-cpu=native"
RUN cargo build --release

##########################################################

FROM gcr.io/distroless/cc-debian12

COPY --from=builder /build/rt_smol/target/release/framework_benchmarks-smol /app/

EXPOSE 8000
ENV DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world
ENV MAX_CONNECTIONS=56
ENV MIN_CONNECTIONS=56
CMD [ "/app/framework_benchmarks-smol" ]
