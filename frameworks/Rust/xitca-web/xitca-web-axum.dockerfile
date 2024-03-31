FROM rust:1.77

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN cargo build --release --bin xitca-web-axum --features axum,io-uring,pg-sync,template

EXPOSE 8080

CMD ./target/release/xitca-web-axum
