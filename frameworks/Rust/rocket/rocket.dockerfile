FROM rust:1.76

ADD ./ /rocket
WORKDIR /rocket

RUN RUSTFLAGS="-C target-cpu=native" cargo build --release
RUN cp ./target/release/rocket ./target/release/rocket-techempower

EXPOSE 8000

CMD ./target/release/rocket-techempower
