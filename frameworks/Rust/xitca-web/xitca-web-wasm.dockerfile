FROM rust:latest

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN rustup default nightly-2022-09-04
RUN rustup target add wasm32-wasi
RUN curl https://wasmtime.dev/install.sh -sSf | bash
RUN cargo clean
RUN RUSTFLAGS="--cfg tokio_unstable" cargo build --release --bin xitca-web-wasm --target wasm32-wasi --features web

EXPOSE 8080

CMD wasmtime ./target/wasm32-wasi/release/xitca-web-wasm.wasm --tcplisten 0.0.0.0:8080 --env FD_COUNT=3
