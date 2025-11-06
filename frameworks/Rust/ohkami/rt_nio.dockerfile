FROM rust:1.89-slim-bookworm AS builder

RUN apt update && apt install -y --no-install-recommends \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

COPY ./Cargo.toml /build/
COPY ./src/       /build/src/
COPY ./rt_nio/    /build/rt_nio/
    
WORKDIR /build/rt_nio
ENV RUSTFLAGS="-C target-cpu=native"
RUN cargo build --release

##########################################################

FROM gcr.io/distroless/cc-debian12

COPY --from=builder /build/rt_nio/target/release/framework_benchmarks-nio /app/

EXPOSE 8000
CMD [ "/app/framework_benchmarks-nio" ]
