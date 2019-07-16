FROM rust:1.36

ADD ./ /nickel
WORKDIR /nickel

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

CMD ./target/release/nickel
