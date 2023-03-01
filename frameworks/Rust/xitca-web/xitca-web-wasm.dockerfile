FROM rust:latest

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN rustup default nightly-2023-02-20
RUN rustup target add wasm32-wasi
RUN cargo clean
RUN RUSTFLAGS="--cfg tokio_unstable" cargo build --release --bin xitca-web-wasm --target wasm32-wasi --features web

RUN curl --show-error --location --fail https://github.com/bytecodealliance/wasmtime/releases/download/v6.0.0/wasmtime-v6.0.0-x86_64-linux.tar.xz --output wasmtime.tar.xz
RUN tar -xvf wasmtime.tar.xz

EXPOSE 8080

CMD ./wasmtime-v6.0.0-x86_64-linux/wasmtime ./target/wasm32-wasi/release/xitca-web-wasm.wasm --tcplisten 0.0.0.0:8080 --env FD_COUNT=3
