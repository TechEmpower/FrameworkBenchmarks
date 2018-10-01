FROM rust:1.29.1

WORKDIR /gotham
COPY ./src ./src
COPY ./Cargo.toml ./Cargo.toml

ENV RUSTFLAGS "-C target-cpu=native"
RUN cargo build --release

CMD ["./target/release/gotham_techempower"]
