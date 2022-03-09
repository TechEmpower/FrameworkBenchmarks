FROM rust:1.58

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN rustup default nightly-2022-01-26
RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release --bin xitca-web

EXPOSE 8080

CMD ./target/release/xitca-web
