FROM rust:1.44

WORKDIR /warp-rust
COPY src src
COPY templates templates
COPY Cargo.toml Cargo.toml

RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

CMD ./target/release/warp-rust
