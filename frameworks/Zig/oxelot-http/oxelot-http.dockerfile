FROM debian:bookworm-slim AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    xz-utils \
    libpq-dev \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

# Install Zig (pinned dev build hosted on oxelot-http releases)
ARG ZIG_VERSION=0.16.0-dev.1859+212968c57
RUN curl -sSL "https://github.com/samcschneider/oxelot-http/releases/download/v0.1.0/zig-x86_64-linux-${ZIG_VERSION}.tar.xz" \
    -o /tmp/zig.tar.xz && \
    tar -xJf /tmp/zig.tar.xz -C /usr/local && \
    ln -s /usr/local/zig-x86_64-linux-${ZIG_VERSION}/zig /usr/local/bin/zig && \
    rm /tmp/zig.tar.xz

WORKDIR /build

# Copy source files
COPY build.zig build.zig.zon ./
COPY src/ src/
COPY lib/ lib/
COPY examples/ examples/

# Build only the techempower binary
RUN zig build build-techempower

# --- Runtime stage ---
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq5 \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/zig-out/bin/techempower /app/techempower

ENV DBHOST=tfb-database
ENV DBPORT=5432
ENV DBNAME=hello_world
ENV DBUSER=benchmarkdbuser
ENV DBPASS=benchmarkdbpass
ENV DB_CONNS=4
ENV PORT=8000

EXPOSE 8000

CMD ["/app/techempower"]
