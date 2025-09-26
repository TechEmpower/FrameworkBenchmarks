FROM rust:1.50

WORKDIR /tide
COPY src src
COPY templates templates
COPY Cargo.toml Cargo.toml

RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

EXPOSE 8080

CMD ./target/release/tide
