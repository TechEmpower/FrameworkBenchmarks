FROM rust:1.93

WORKDIR /sib
COPY . .

RUN apt-get update && apt-get install -y cmake clang lld llvm libclang-dev
RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

EXPOSE 8080

CMD ["./target/release/sib-techempower"]
