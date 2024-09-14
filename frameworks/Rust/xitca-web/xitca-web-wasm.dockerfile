ARG WASMTIME_VERSION=15.0.0
ARG WASM_TARGET=wasm32-wasip1-threads

FROM rust:1.79 AS compile

ARG WASMTIME_VERSION
ARG WASM_TARGET

WORKDIR /tmp
COPY / ./
RUN curl -LSs "https://github.com/bytecodealliance/wasmtime/releases/download/v${WASMTIME_VERSION}/wasmtime-v${WASMTIME_VERSION}-$(uname -m)-linux.tar.xz" | \
tar --strip-components=1 -Jx && \
rustup default nightly && \
rustup target add ${WASM_TARGET} && \
cargo build --bin xitca-web-wasm --features web --release --target ${WASM_TARGET}


FROM ubuntu:22.04

ARG WASM_TARGET
ARG BENCHMARK_ENV
ARG TFB_TEST_DATABASE
ARG TFB_TEST_NAME

COPY --from=compile \
/tmp/target/${WASM_TARGET}/release/xitca-web-wasm.wasm \
/tmp/wasmtime \
/opt/xitca-web-wasm/
EXPOSE 8080

CMD /opt/xitca-web-wasm/wasmtime run \
--wasm all-proposals=y \
--wasi threads=y,tcplisten=0.0.0.0:8080 \
--env FD_COUNT=3 \
/opt/xitca-web-wasm/xitca-web-wasm.wasm
