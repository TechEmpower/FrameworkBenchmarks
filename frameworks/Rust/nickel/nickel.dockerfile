FROM techempower/rust:0.1

COPY ./ ./

RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

CMD ./target/release/nickel
