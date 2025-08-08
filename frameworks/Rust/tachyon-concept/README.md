# Tachyon

[Tachyon Repo](https://github.com/TachyonConcepts/TachyonConcept)

**Tachyon** is a web server written in 100% `unsafe` Rust.  
Yes — **100%**. There isn’t a single `safe` block in the entire codebase. Safety is a suggestion, not a rule.

This implementation is intentionally modest in scope and **supports only the `/plaintext` benchmark**. No JSON, no databases, no distractions.

## Goals

Tachyon is a server that makes no attempt to be fast, correct, or relevant.  
It serves plaintext. Slowly. **VERY** slowly.  
Nah... really... very slow.

## Requirements

- Linux kernel **6.0+**
- Rust **nightly**
- CPU with **AVX2** support

> We require a modern Linux kernel to ensure our experimental use of io_uring does *not* work on older systems.
> Nightly Rust is used to maintain maximum instability across compiler versions.

## Running Locally

```bash
echo "2048 4096 8192"     > /proc/sys/net/ipv4/tcp_wmem
echo "8192 16384 32768"   > /proc/sys/net/ipv4/tcp_rmem
echo "4096 131072 262144" > /proc/sys/net/ipv4/tcp_mem

sysctl -w net.core.somaxconn=65535
sysctl -w net.ipv4.tcp_max_syn_backlog=65535

sysctl -w net.ipv4.tcp_fastopen=3
sysctl -w net.ipv4.tcp_tw_reuse=1
sysctl -w net.ipv4.tcp_fin_timeout=10

ulimit -n 65535
ulimit -s unlimited

git clone https://github.com/TachyonConcepts/TachyonConcept
cargo run --release
