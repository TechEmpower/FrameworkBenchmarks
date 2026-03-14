FROM rust:1.93

ADD ./ /xitca-web
WORKDIR /xitca-web

# nightly rust enables toasty to use more static dispatch of async trait 
RUN rustup default nightly-2026-02-24
RUN cargo build --release --bin xitca-web-toasty --features perf,template,toasty,web-codegen

EXPOSE 8080

CMD ./target/release/xitca-web-toasty
