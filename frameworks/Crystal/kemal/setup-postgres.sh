#!/bin/bash

fw_depends postgresql crystal

shards install

crystal build --release server-postgres.cr

for i in {1..$(nproc --all)}; do
  ./server-postgres &
done

wait
