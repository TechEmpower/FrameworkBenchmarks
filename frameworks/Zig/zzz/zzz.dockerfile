FROM debian:12-slim AS build

# Set a non-root user for added security
RUN useradd -m ziguser

# Install dependencies (update to latest secure versions)
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    wget xz-utils \
    ca-certificates && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Download the latest stable Zig binary from the official website
ARG ZIG_VERSION=0.14.0
RUN wget https://ziglang.org/download/0.14.0/zig-linux-{{arch}}-0.14.0.tar.xz

RUN tar -xvf zig-linux-{{arch}}-0.14.0.tar.xz

RUN mv zig-linux-{{arch}}-0.14.0 /usr/local/zig 

# Add Zig to the PATH
ENV PATH="/usr/local/zig:$PATH"

WORKDIR /home/ziguser

# Switch to the non-root user
USER ziguser

RUN zig build -Doptimize=ReleaseFast
RUN ls

FROM debian:12-slim

RUN apt-get -qq update 
RUN apt-get -qy install ca-certificates curl

COPY --from=build /home/ziguser/zig-out/bin/zzz /server
EXPOSE 8080
ENTRYPOINT ./server
