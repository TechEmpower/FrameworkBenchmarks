FROM rust:1.63

ENV ROCKET_BENCHMARK_DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world

WORKDIR /rocket-diesel
COPY ./rocket-diesel ./rocket-diesel
COPY ./templates ./templates
COPY ./Cargo.toml ./Cargo.toml

ENV RUSTFLAGS="-C target-cpu=native"
RUN cargo build --release --bin rocket-diesel

RUN cp ./target/release/rocket-diesel ./target/release/rocket-techempower

EXPOSE 8000
CMD ./target/release/rocket-techempower

