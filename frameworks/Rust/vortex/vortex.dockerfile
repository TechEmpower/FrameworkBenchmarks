FROM rust:1.82-slim-bookworm AS builder

# Install build deps + LLVM 16 for BOLT
RUN apt-get update && apt-get install -y \
    build-essential pkg-config liburing-dev lld wget gnupg git \
    && wget -qO- https://apt.llvm.org/llvm-snapshot.gpg.key | gpg --dearmor -o /etc/apt/trusted.gpg.d/llvm-snapshot.gpg \
    && echo "deb http://apt.llvm.org/bookworm/ llvm-toolchain-bookworm-16 main" > /etc/apt/sources.list.d/llvm-16.list \
    && apt-get update && apt-get install -y bolt-16 libbolt-16-dev \
    && ln -s /usr/lib/llvm-16/lib/libbolt_rt_instr.a /usr/lib/libbolt_rt_instr.a \
    && ln -s /usr/lib/llvm-16/lib/libbolt_rt_hugify.a /usr/lib/libbolt_rt_hugify.a \
    && rm -rf /var/lib/apt/lists/*

RUN rustup component add llvm-tools-preview

# --- PGO: build and run profgen from vortex framework repo ---
WORKDIR /profgen-build
RUN git clone https://github.com/yp3y5akh0v/vortex .

RUN RUSTFLAGS="-Ctarget-cpu=native -Clink-arg=-fuse-ld=lld -Cprofile-generate=/tmp/pgo-data" \
    cargo build --release --bin vortex-profgen

RUN /profgen-build/target/release/vortex-profgen

RUN LLVM_PROFDATA="$(rustc --print sysroot)/lib/rustlib/x86_64-unknown-linux-gnu/bin/llvm-profdata" && \
    $LLVM_PROFDATA merge -o /tmp/pgo-merged.profdata /tmp/pgo-data/

# --- Benchmark app: build from local source with PGO + emit-relocs for BOLT ---
WORKDIR /app
COPY Cargo.toml Cargo.lock ./
COPY src ./src

RUN RUSTFLAGS="-Ctarget-cpu=native -Clink-arg=-fuse-ld=lld -Clink-arg=-Wl,--emit-relocs -Cprofile-use=/tmp/pgo-merged.profdata" \
    cargo build --release --bin vortex-bench

# --- BOLT: rebuild profgen with PGO for instrumentation ---
WORKDIR /profgen-build
RUN RUSTFLAGS="-Ctarget-cpu=native -Clink-arg=-fuse-ld=lld -Clink-arg=-Wl,--emit-relocs -Cprofile-use=/tmp/pgo-merged.profdata" \
    cargo build --release --bin vortex-profgen

RUN llvm-bolt-16 /profgen-build/target/release/vortex-profgen \
    -instrument \
    -instrumentation-file=/tmp/bolt-prof \
    -o /tmp/vortex-profgen-bolt

RUN /tmp/vortex-profgen-bolt && \
    llvm-bolt-16 /app/target/release/vortex-bench \
    -data=/tmp/bolt-prof \
    --profile-ignore-hash \
    -reorder-blocks=ext-tsp \
    -reorder-functions=hfsort+ \
    -split-functions \
    -split-all-cold \
    -icf=1 \
    -frame-opt=all \
    -plt=hot \
    -o /app/target/release/vortex-bench-bolted

# Runtime image
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    liburing2 \
    procps \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/target/release/vortex-bench-bolted /usr/local/bin/vortex
COPY run.sh /usr/local/bin/run.sh
RUN chmod +x /usr/local/bin/run.sh

EXPOSE 8080

ENTRYPOINT ["/usr/local/bin/run.sh"]
