FROM rust:1.67 AS compile

ARG WASMTIME_VERSION=6.0.0

ARG RUSTFLAGS="-C target-feature=+simd128 --cfg tokio_unstable"
WORKDIR /tmp
COPY / ./
RUN curl -LSs "https://github.com/bytecodealliance/wasmtime/releases/download/v${WASMTIME_VERSION}/wasmtime-v${WASMTIME_VERSION}-$(uname -m)-linux.tar.xz" | \
      tar --strip-components=1 -Jx && \
    rustup target add wasm32-wasi && \
    cargo build --bin xitca-web-wasm --features web --release --target wasm32-wasi

FROM ubuntu:22.04

COPY --from=compile \
     /tmp/target/wasm32-wasi/release/xitca-web-wasm.wasm \
     /tmp/wasmtime \
     /opt/xitca-web-wasm/
EXPOSE 8080
ARG BENCHMARK_ENV
ARG TFB_TEST_DATABASE
ARG TFB_TEST_NAME

CMD /opt/xitca-web-wasm/wasmtime run \
      --env FD_COUNT=3 \
      --tcplisten 0.0.0.0:8080 \
      /opt/xitca-web-wasm/xitca-web-wasm.wasm
