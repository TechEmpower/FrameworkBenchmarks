# TechEmpower Benchmark Results

## Test Environment
- **Host**: 24-core (12C/24T) local workstation
- **OS**: Linux 6.17.9
- **Zig**: 0.16.0-dev.1859
- **PostgreSQL**: TFB docker image (`tfb-database`) on port 5434
- **wrk**: `-t8 -c256 -d10s` (plaintext pipelined with `-s pipeline.lua -- 16`)

## Axum Baseline (local, Round 23 code)

Built from `/home/sam/FrameworkBenchmarks/frameworks/Rust/axum/`.

| Test | req/s |
|------|-------|
| json | 2,010,721 |
| plaintext | 1,687,888 |
| db | 345,038 |
| fortune | 322,833 |
| queries q=1 | 427,457 |
| queries q=5 | 145,060 |
| queries q=20 | 47,856 |
| updates q=1 | 8,275 |
| updates q=5 | 8,143 |
| updates q=20 | 4,309 |

## Round 23 Official Axum Results (benchmark hardware)

| Test | req/s |
|------|-------|
| json | 2,709,795 |
| plaintext | 12,048,256 |
| db | 1,190,767 |
| fortune | 1,114,266 |
| queries q=20 | 87,231 |
| updates q=20 | 55,709 |
| cached q=1 | 1,112,890 (axum-sqlx) |

## Environment Multipliers (R23 official / local axum)

Used to extrapolate local results to approximate R23 benchmark hardware performance.

- **CPU-bound** (json, plaintext, cached): **1.35x**
- **DB-read** (db, fortune, queries): **2.64x**
- **Updates**: **2.64x**

Note: Plaintext R23 multiplier is 7.14x due to TFB using pipelined wrk (depth 16), while local baseline used non-pipelined wrk.

## zig-http Results — Phase 1 (async libpq + io_uring poll)

Async non-blocking libpq with io_uring poll_add for DB operations.
4 connections per thread, per-request pipeline enter/exit.

| Test | req/s | vs local Axum |
|------|-------|---------------|
| json | 1,517,010 | 0.75x |
| plaintext | 1,199,502 | 0.71x |
| plaintext (pipelined 16) | 11,200,368 | — |
| db | 156,812 | 0.45x |
| fortune | 157,274 | 0.49x |
| queries q=20 | 82,133 | 1.72x |
| updates q=20 | 7,919 | 1.84x |
| cached q=1 | 1,209,762 | — |

## zig-http Results — Phase 2 (current)

Phase 2 optimizations over Phase 1:
1. **Zero-copy raw responses**: Skip Server/Date header allocs for raw handlers; return threadlocal buffer directly without alloc+copy
2. **Batched DB pipelining**: Connections stay in pipeline mode permanently; all queued requests submitted immediately
3. **Binary format results**: World queries use INT4 binary format (skip parseInt, use @byteSwap)
4. **Increased response list capacity**: 64→256 for higher DB concurrency

| Test | req/s | vs Phase 1 | vs local Axum |
|------|-------|------------|---------------|
| json | 1,476,174 | ~same | 0.73x |
| plaintext | 1,267,882 | ~same | 0.75x |
| db | 234,421 | **+49%** | 0.68x |
| fortune | 227,801 | **+45%** | 0.71x |
| queries q=20 | 94,710 | **+15%** | 1.98x |
| updates q=20 | 10,628 | **+34%** | 2.47x |

## Extrapolated Round 23 Rankings

Extrapolated using environment multipliers and compared against all ~500 R23 framework entries.

| Test | Local | Extrap R23 | Rank | Total | Percentile | Neighbors |
|------|-------|-----------|------|-------|------------|-----------|
| json | 1,476K | 1,993K | #110 | 540 | top 20% | act(Java), cutelyst(C++) |
| plaintext | 1,268K | 1,712K | #206 | 533 | top 39% | non-pipelined* |
| db | 234K | 619K | #91 | 554 | **top 16%** | feersum(Perl), elysia(Bun) |
| fortune | 228K | 601K | #56 | 518 | **top 11%** | aspnetcore-minimal, mixphp |
| query q=20 | 95K | 250K | **#1** | 534 | **top 0.2%** | — |
| update q=20 | 10.6K | 28K | #83 | 483 | **top 17%** | mormot(Pascal), es4x(JS) |
| cached q=1 | 1,210K | 1,633K | #29 | 126 | top 23% | feersum(Perl), lwan(C) |

\* Plaintext non-pipelined; with pipelining (depth 16) at 11.2M local / ~15.1M extrapolated, would rank significantly higher.

### vs Round 23 #1

| Test | R23 #1 | R23 #1 framework | Our extrap | % of #1 |
|------|--------|-----------------|-----------|---------|
| json | 3,118K | libreactor (C) | 1,993K | 64% |
| db | 1,448K | may-minihttp (Rust) | 619K | 43% |
| fortune | 1,327K | may-minihttp (Rust) | 601K | 45% |
| query q=20 | 90K | xitca-web (Rust) | 250K | **278%** |
| update q=20 | 63K | xitca-web (Rust) | 28K | 44% |

## All Optimizations Applied

1. **UNNEST batch updates**: Single `UNNEST($1::int[], $2::int[])` UPDATE instead of partitioned CASE/WHEN workers
2. **Eliminate double HTTP parse**: `parseAndFindRequestEnd()` + `processRequestPreparsed()` avoids re-parsing
3. **HTTP pipelining batching**: Up to 32 requests batched into single `write()` syscall
4. **Response.setRaw()**: Pre-formatted responses bypass header/body serialization
5. **Async libpq + io_uring poll**: Non-blocking DB with per-thread `DbConnPool`
6. **Zero-copy raw responses**: Skip alloc+copy for threadlocal response buffers; skip Server/Date header heap allocs
7. **Batched DB pipelining**: Connections stay in pipeline mode; multiple requests multiplexed per connection
8. **Binary query results**: INT4 binary format for world queries (skip text parseInt)

## Remaining Opportunities

1. **JSON (top 20%)**: Response.init() still allocates Headers hashmap even for raw handlers. A dedicated fast-path that skips Response construction entirely could help.
2. **DB/Fortune (top 11-16%)**: More DB connections (limited by local PG max_connections=100). On TFB hardware (max_connections=2000), 16+ conns/thread would increase concurrency.
3. **Updates (top 17%)**: Bottlenecked by UPDATE query latency. Could benefit from connection-affinity to reduce UNNEST overhead.
4. **Plaintext**: Implement TFB's pipelined plaintext benchmark format for accurate comparison.
