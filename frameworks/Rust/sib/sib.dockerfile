FROM rust:latest

ADD ./ /sib
WORKDIR /sib

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

EXPOSE 8080

CMD ["./target/release/sib-techempower"]
