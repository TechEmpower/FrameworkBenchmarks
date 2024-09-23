FROM rust:1.81

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN cargo build --release --bin xitca-web-axum --features axum,io-uring,perf,pg-sync,template

EXPOSE 8080

CMD ./target/release/xitca-web-axum
