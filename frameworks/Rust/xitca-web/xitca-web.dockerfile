FROM rust:latest

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN rustup default nightly-2022-09-04
RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release --bin xitca-web --features raw,simd

EXPOSE 8080

CMD ./target/release/xitca-web
