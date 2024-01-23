FROM rust:1.64

ADD ./ /anansi
WORKDIR /anansi

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release --features raw

EXPOSE 8080

CMD RUST_LOG=off ./target/release/tfb-anansi 0.0.0.0:8080
