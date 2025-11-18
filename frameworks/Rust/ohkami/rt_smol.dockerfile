FROM rust:1.89-slim-bookworm AS builder

RUN apt update && apt install -y --no-install-recommends \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

COPY ./Cargo.toml /build/
COPY ./src/       /build/src/
COPY ./rt_smol/   /build/rt_smol/
    
WORKDIR /build/rt_smol
ENV RUSTFLAGS="-C target-cpu=native"
RUN cargo build --release

##########################################################

FROM gcr.io/distroless/cc-debian12

COPY --from=builder /build/rt_smol/target/release/framework_benchmarks-smol /app/

EXPOSE 8000
CMD [ "/app/framework_benchmarks-smol" ]
