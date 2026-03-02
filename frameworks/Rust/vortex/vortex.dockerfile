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

# llvm-tools for PGO profile merging
RUN rustup component add llvm-tools-preview

WORKDIR /vortex

# Copy manifests first for dependency caching
COPY Cargo.toml Cargo.lock* ./
COPY .cargo .cargo

# Create dummy source files for dependency compilation
RUN mkdir -p src && echo "fn main() {}" > src/main.rs && \
    echo "fn main() {}" > src/profgen.rs

# Pre-compile dependencies with PGO instrumentation flags (cached layer)
RUN RUSTFLAGS="-Ctarget-cpu=native -Clink-arg=-fuse-ld=lld -Cprofile-generate=/tmp/pgo-data" \
    cargo build --release 2>/dev/null || true

# Copy actual source code
COPY src src
RUN find src -name "*.rs" -exec touch {} +

# === PGO Phase 1: Build instrumented profiling binary ===
RUN RUSTFLAGS="-Ctarget-cpu=native -Clink-arg=-fuse-ld=lld -Cprofile-generate=/tmp/pgo-data" \
    cargo build --release --bin vortex-profgen

# === PGO Phase 2: Run profiling harness to generate profile data ===
RUN /vortex/target/release/vortex-profgen

# === PGO Phase 3: Merge profile data ===
RUN LLVM_PROFDATA="$(rustc --print sysroot)/lib/rustlib/x86_64-unknown-linux-gnu/bin/llvm-profdata" && \
    $LLVM_PROFDATA merge -o /tmp/pgo-merged.profdata /tmp/pgo-data/

# === PGO Phase 4: Rebuild with PGO + emit-relocs (BOLT needs relocations) ===
RUN RUSTFLAGS="-Ctarget-cpu=native -Clink-arg=-fuse-ld=lld -Clink-arg=-Wl,--emit-relocs -Cprofile-use=/tmp/pgo-merged.profdata" \
    cargo build --release --bin vortex-bench --bin vortex-profgen

# === BOLT Phase 5: Instrument profgen binary ===
RUN llvm-bolt-16 /vortex/target/release/vortex-profgen \
    -instrument \
    -instrumentation-file=/tmp/bolt-prof \
    -o /tmp/vortex-profgen-bolt

# === BOLT Phase 6: Run instrumented profgen + optimize server binary ===
RUN /tmp/vortex-profgen-bolt && \
    llvm-bolt-16 /vortex/target/release/vortex-bench \
    -data=/tmp/bolt-prof \
    -reorder-blocks=ext-tsp \
    -reorder-functions=hfsort+ \
    -split-functions \
    -split-all-cold \
    -o /vortex/target/release/vortex-bench-bolted

# Runtime image
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    liburing2 \
    procps \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /vortex/target/release/vortex-bench-bolted /usr/local/bin/vortex
COPY run.sh /usr/local/bin/run.sh
RUN chmod +x /usr/local/bin/run.sh

EXPOSE 8080

ENTRYPOINT ["/usr/local/bin/run.sh"]
