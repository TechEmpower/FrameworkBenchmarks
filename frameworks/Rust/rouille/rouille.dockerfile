FROM rust:1.25

ADD ./ /rouille
WORKDIR /rouille

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

CMD ./target/release/rouille
