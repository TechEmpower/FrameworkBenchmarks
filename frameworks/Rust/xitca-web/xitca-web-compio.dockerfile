FROM rust:1.92

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN rustup default nightly-2025-12-23
RUN cargo build --release --bin xitca-web-compio --features compio,perf,perf-json,pg,router,template,zero-copy

EXPOSE 8080

CMD ./target/release/xitca-web-compio
