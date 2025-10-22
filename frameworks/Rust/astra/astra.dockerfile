FROM rust:1.58

ADD ./ /astra
WORKDIR /astra

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

EXPOSE 8080

CMD ./target/release/astra
