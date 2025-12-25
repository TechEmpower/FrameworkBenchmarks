FROM rust:1.92

ADD ./ /xitca-web
WORKDIR /xitca-web

# nightly rust enables toasty to use more static dispatch of async trait 
RUN rustup default nightly-2025-12-23
RUN cargo build --release --bin xitca-web-toasty --features perf,template,toasty,web-codegen

EXPOSE 8080

CMD ./target/release/xitca-web-toasty
