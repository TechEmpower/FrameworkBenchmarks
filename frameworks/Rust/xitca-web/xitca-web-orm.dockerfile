FROM rust:1.85

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN cargo build --release --bin xitca-web-orm --features pg-orm-async,template,web-codegen

EXPOSE 8080

CMD ./target/release/xitca-web-orm
