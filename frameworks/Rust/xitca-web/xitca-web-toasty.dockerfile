FROM rust:1.91.1

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN cargo build --release --bin xitca-web-toasty --features toasty,template,web-codegen

EXPOSE 8080

CMD ./target/release/xitca-web-toasty
