FROM rust:1.93.0 as builder

WORKDIR /app
COPY Cargo.toml ./
COPY src ./src
COPY templates ./templates

RUN cargo build --release

FROM debian:bookworm-slim
WORKDIR /app

RUN apt-get update && apt-get install -y libpq5 && rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/target/release/hotaru_bench /app/server
COPY templates /app/templates

ENV DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world
ENV DB_POOL_SIZE=56

EXPOSE 8080
CMD ["/app/server"]
