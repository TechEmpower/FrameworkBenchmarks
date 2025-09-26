FROM rust:1.57
WORKDIR /trillium
COPY src src
COPY templates templates
COPY Cargo.toml Cargo.toml
COPY Cargo.lock Cargo.lock

EXPOSE 8080

ENV RUSTFLAGS="-C target-cpu=native"
ENV PORT=8080
ENV HOST=0.0.0.0
ENV DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world

RUN cargo build --release
CMD ["./target/release/trillium-techempower"]
