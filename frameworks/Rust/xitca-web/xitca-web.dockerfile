FROM rust:latest

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN cargo build --release --bin xitca-web --features pg,serde,template

EXPOSE 8080

CMD ./target/release/xitca-web
