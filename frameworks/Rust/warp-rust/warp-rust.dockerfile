FROM rust:1.40

WORKDIR /warp-rust
COPY src src
COPY Cargo.toml Cargo.toml

RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

CMD ./target/release/warp-rust
