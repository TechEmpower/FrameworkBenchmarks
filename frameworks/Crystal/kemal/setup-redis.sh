#!/bin/bash

# This redis implementation works but there is currently no redis installation
# for the benchmark suite

fw_depends crystal

shards install

crystal build --release --no-debug server-redis.cr

export GC_MARKERS=1

export KEMAL_ENV=production

for i in $(seq 1 $(nproc --all)); do
  ./server-redis &
done

wait
