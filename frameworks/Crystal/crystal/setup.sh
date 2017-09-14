#!/bin/bash

fw_depends postgresql crystal

shards install

crystal build --release --no-debug server.cr -o server.out

for i in $(seq 1 $(nproc --all)); do
  ./server.out &
done

wait
