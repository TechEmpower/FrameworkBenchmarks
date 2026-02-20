FROM rust:1.93

WORKDIR /saphir
COPY . .

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release --quiet

EXPOSE 8080

CMD ./target/release/saphir-techempower
