FROM rust:1.93-slim-trixie AS builder

RUN apt update && apt install -y --no-install-recommends \
    pkg-config \
    git \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build/rt_glommio
COPY ./Cargo.toml /build/
COPY ./src/       /build/src/
COPY ./rt_glommio/  /build/rt_glommio/
ENV RUSTFLAGS="-C target-cpu=native"
RUN cargo build --release

##########################################################

FROM gcr.io/distroless/cc-debian12
COPY --from=builder /build/rt_glommio/target/release/framework_benchmarks-glommio /app/
EXPOSE 8000
CMD [ "/app/framework_benchmarks-glommio" ]
