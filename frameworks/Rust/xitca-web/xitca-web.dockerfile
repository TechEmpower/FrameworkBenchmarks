FROM rust:1.92

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN cargo build --release --bin xitca-web --features io-uring,pg,router,template,zero-copy,perf-json,perf-allocator

EXPOSE 8080

CMD ./target/release/xitca-web
