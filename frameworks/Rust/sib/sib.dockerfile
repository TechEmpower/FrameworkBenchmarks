FROM rust:1.93

WORKDIR /sib
COPY . .

RUN apt-get update && apt-get install -y cmake clang lld llvm libclang-dev > /dev/null
RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release --quiet

EXPOSE 8080

CMD ["./target/release/sib-techempower"]
