FROM rust:1.91.1

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN cargo build --release --bin xitca-web-barebone --features perf,perf-json,pg,template

EXPOSE 8080

CMD ./target/release/xitca-web-barebone
