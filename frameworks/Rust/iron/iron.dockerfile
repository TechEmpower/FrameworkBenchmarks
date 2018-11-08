FROM rust:1.29.1

ADD ./ /iron
WORKDIR /iron

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

CMD ./target/release/iron
