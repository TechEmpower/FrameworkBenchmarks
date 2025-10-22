FROM rust:1.89

ADD ./ /viz
WORKDIR /viz

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release --bin viz-pg --features="tokio-postgres,yarte"

EXPOSE 8080

CMD ./target/release/viz-pg
