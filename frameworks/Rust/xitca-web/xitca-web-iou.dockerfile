FROM rust:1.77

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN cargo build --release --bin xitca-web-iou --features io-uring,pg-iou,template

EXPOSE 8080

CMD ./target/release/xitca-web-iou
