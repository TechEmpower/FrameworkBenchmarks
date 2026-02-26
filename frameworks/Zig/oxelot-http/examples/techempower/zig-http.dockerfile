FROM debian:bookworm-slim AS build

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl xz-utils ca-certificates libpq-dev libc6-dev && \
    rm -rf /var/lib/apt/lists/*

# Install Zig
ARG ZIG_VERSION=0.14.1
RUN curl -fsSL "https://ziglang.org/download/${ZIG_VERSION}/zig-linux-x86_64-${ZIG_VERSION}.tar.xz" | \
    tar -xJ -C /usr/local && \
    ln -s /usr/local/zig-linux-x86_64-${ZIG_VERSION}/zig /usr/local/bin/zig

WORKDIR /app
COPY . .

RUN zig build techempower -Doptimize=ReleaseFast

# Runtime image
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends libpq5 && \
    rm -rf /var/lib/apt/lists/*

COPY --from=build /app/zig-out/bin/techempower /usr/local/bin/techempower

EXPOSE 8080

CMD ["techempower"]
