FROM rust:1.74

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN cargo build --release --bin xitca-web-axum --features axum,io-uring

EXPOSE 8080

CMD ./target/release/xitca-web-axum
