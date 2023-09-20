ARG WASMTIME_VERSION=12.0.1
ARG WASM_TARGET=wasm32-wasi-preview1-threads

FROM rust:1.67 AS compile

ARG WASMTIME_VERSION
ARG WASM_TARGET

WORKDIR /tmp
COPY / ./
RUN curl -LSs "https://github.com/bytecodealliance/wasmtime/releases/download/v${WASMTIME_VERSION}/wasmtime-v${WASMTIME_VERSION}-$(uname -m)-linux.tar.xz" | \
tar --strip-components=1 -Jx && \
rustup target add ${WASM_TARGET} && \
cargo build --bin xitca-web-wasm --features serde,web --release --target ${WASM_TARGET}


FROM ubuntu:22.04

ARG WASM_TARGET

COPY --from=compile \
/tmp/target/${WASM_TARGET}/release/xitca-web-wasm.wasm \
/tmp/wasmtime \
/opt/xitca-web-wasm/
EXPOSE 8080
ARG BENCHMARK_ENV
ARG TFB_TEST_DATABASE
ARG TFB_TEST_NAME

CMD /opt/xitca-web-wasm/wasmtime run /opt/xitca-web-wasm/xitca-web-wasm.wasm \
--wasm-features=threads \
--wasi-modules experimental-wasi-threads \
--allow-precompiled \
--env FD_COUNT=3 \
--tcplisten 0.0.0.0:8080
