FROM techempower/rust:0.1

ADD ./ /actix
WORKDIR /actix

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release
