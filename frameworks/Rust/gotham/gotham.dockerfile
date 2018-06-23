FROM rust:1.26

WORKDIR /gotham
COPY ./src ./src
COPY ./Cargo.toml ./Cargo.toml
COPY ./Cargo.lock ./Cargo.lock

ENV RUSTFLAGS "-C target-cpu=native"
RUN cargo build --release

CMD ["./target/release/gotham_techempower"]
