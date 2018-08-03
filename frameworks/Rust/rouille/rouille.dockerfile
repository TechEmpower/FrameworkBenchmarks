FROM rust:1.27

WORKDIR /rouille
COPY src src
COPY Cargo.toml Cargo.toml

RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

CMD ["./target/release/rouille"]
