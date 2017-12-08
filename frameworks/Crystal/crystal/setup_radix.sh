#!/bin/bash

fw_depends postgresql crystal

shards install

crystal build --release --no-debug server_radix.cr -o server_radix.out

export GC_MARKERS=1

for i in $(seq 1 $(nproc --all)); do
  ./server_radix.out &
done

wait
