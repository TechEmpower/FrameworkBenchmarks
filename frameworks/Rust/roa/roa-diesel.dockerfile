FROM rust:1.41

RUN apt update -yqq && apt install -yqq cmake g++

ADD ./ /roa
WORKDIR /roa

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release --bin roa-diesel

CMD ./target/release/roa-diesel