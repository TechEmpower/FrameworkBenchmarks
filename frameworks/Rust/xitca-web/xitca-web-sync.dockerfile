FROM rust:1.79

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN cargo build --release --bin xitca-web-sync --features pg-orm,template,web-codegen

EXPOSE 8080

CMD ./target/release/xitca-web-sync
