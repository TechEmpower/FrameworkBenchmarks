FROM rust:latest

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN rustup default nightly-2023-02-20
RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release --bin xitca-web-iou --features io-uring,raw,sailfish,serde

EXPOSE 8080

CMD ./target/release/xitca-web-iou
