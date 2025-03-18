FROM rust:1.83

ADD ./ /viz
WORKDIR /viz

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release --bin viz-sqlx --features="sqlx,markup,v_htmlescape"

EXPOSE 8080

CMD ./target/release/viz-sqlx
