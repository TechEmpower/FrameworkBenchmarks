FROM rust:1.60

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN rustup default nightly-2022-04-27
RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release --bin xitca-web-diesel

EXPOSE 8080

CMD ./target/release/xitca-web-diesel
