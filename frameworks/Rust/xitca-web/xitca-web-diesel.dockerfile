FROM rust:1.55

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN cargo clean
RUN rustup default nightly-2021-10-11
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release --bin xitca-web-diesel

EXPOSE 8080

CMD ./target/release/xitca-web-diesel
