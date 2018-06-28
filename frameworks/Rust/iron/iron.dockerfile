FROM rust:1.27

ADD ./ /iron
WORKDIR /iron

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

CMD ./target/release/iron
