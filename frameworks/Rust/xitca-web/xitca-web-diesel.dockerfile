FROM rust:latest

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN rustup default nightly-2022-10-27
RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release --bin xitca-web-diesel --features orm,sailfish,serde,web

EXPOSE 8080

CMD ./target/release/xitca-web-diesel
