FROM rust:1.81

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN cargo build --release --bin xitca-web-unrealistic --features perf,pg,template

EXPOSE 8080

CMD ./target/release/xitca-web-unrealistic
