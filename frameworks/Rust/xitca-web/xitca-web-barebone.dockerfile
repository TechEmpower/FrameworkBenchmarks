FROM rust:1.93

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN cargo build --release --bin xitca-web-barebone --features perf,perf-json,pg,template,zero-copy

EXPOSE 8080

CMD ./target/release/xitca-web-barebone
