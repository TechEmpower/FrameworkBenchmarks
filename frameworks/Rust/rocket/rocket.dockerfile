FROM rustlang/rust:nightly

ADD ./ /rocket
WORKDIR /rocket

RUN apt-get update && apt-get install -yqq clang-6.0

ENV DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world
RUN cargo clean
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

CMD ./target/release/rocket
