FROM rust:1.64

ADD ./ /anansi
WORKDIR /anansi

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

EXPOSE 8080

CMD ./target/release/tfb-anansi 8080
