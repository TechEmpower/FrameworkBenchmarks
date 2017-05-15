#!/bin/bash

fw_depends postgresql crystal

shards install

crystal build --release src/kemalyst.cr

for i in $(seq 1 $(nproc --all)); do
  ./kemalyst &
done

wait
