#!/bin/bash

# This redis implementation works but there is currently no redis installation
# for the benchmark suite

fw_depends crystal

shards install

crystal build --release server-redis.cr

./server-redis &
