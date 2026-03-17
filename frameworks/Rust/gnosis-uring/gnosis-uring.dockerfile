FROM rust:1.83-bookworm AS builder

WORKDIR /build

COPY src/ src/
COPY Cargo.toml .

RUN cargo build --release

FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends liburing2 && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /var/www && \
    echo '<html><body>Hello, World!</body></html>' > /var/www/index.html

COPY --from=builder /build/target/release/gnosis-uring /usr/local/bin/gnosis-uring

EXPOSE 8080

CMD ["gnosis-uring", "--uring", "--port", "8080", "--root", "/var/www", "--threads", "0"]
