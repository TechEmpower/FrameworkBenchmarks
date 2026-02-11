FROM rust:1.93.0 AS builder

# Install soli from crates.io with locked dependencies
RUN cargo install solilang --locked

# Runtime stage
FROM debian:testing-20260202-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy soli binary from builder
COPY --from=builder /usr/local/cargo/bin/soli /usr/local/bin/soli

# Copy benchmark application files
COPY app/ /app/app/
COPY config/ /app/config/

EXPOSE 3000

CMD ["soli", "serve", "/app", "--port", "3000"]
