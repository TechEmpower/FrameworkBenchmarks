FROM rust:1.93

ADD ./ /xitca-web
WORKDIR /xitca-web

RUN rustup default nightly-2026-02-24
RUN cargo build --release --bin xitca-web-compio --features compio,perf,perf-json,pg,router,template,zero-copy

EXPOSE 8080

CMD ./target/release/xitca-web-compio
