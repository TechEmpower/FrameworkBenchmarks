FROM rust:1.44

ADD ./ /nickel
WORKDIR /nickel

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

EXPOSE 8080

CMD ./target/release/nickel
