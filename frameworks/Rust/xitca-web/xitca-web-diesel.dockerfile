FROM rust:latest

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN cargo build --release --bin xitca-web-diesel --features pg-orm,serde,template,web

EXPOSE 8080

CMD ./target/release/xitca-web-diesel
