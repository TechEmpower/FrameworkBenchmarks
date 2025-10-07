FROM docker.io/rust:1.88-slim-bookworm AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    pkg-config \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /build

# Copy dependency files first for better caching
COPY ./Cargo.toml ./Cargo.lock /build/

# Fetch dependencies
RUN cargo fetch

# Copy template files
COPY ./templates/ /build/templates/

# Copy source code
COPY ./src/ /build/src/

# Set Rust compilation flags for native CPU optimizations
ENV RUSTFLAGS="-C target-cpu=native"

# Build all release binaries
RUN cargo build --release

# Use distroless for minimal runtime image
FROM gcr.io/distroless/cc-debian12

# Set environment variables
ENV POSTGRES_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world
ENV POSTGRES_MIN_POOL_SIZE=56
ENV POSTGRES_MAX_POOL_SIZE=56
ENV MONGODB_URL=mongodb://tfb-database:27017
ENV MONGODB_MIN_POOL_SIZE=56
ENV MONGODB_MAX_POOL_SIZE=56

# Copy all compiled binaries from builder
COPY --from=builder /build/target/release/ignitia* /app/

# Expose port
EXPOSE 8000

# Default command (can be overridden by docker_cmd in benchmark_config.json)
CMD ["/app/ignitia"]

