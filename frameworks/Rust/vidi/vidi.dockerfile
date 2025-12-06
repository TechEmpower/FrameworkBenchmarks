FROM rust:1.91

ADD ./ /vidi
WORKDIR /vidi

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release --bin vidi

EXPOSE 8080

CMD ./target/release/vidi
